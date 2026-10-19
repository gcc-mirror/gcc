/* GCC minipool support for ARM.
   Copyright (C) 2026 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* GCC's standard model for literal pools places the entire table of literals
   either before or after the body of the function.  However, if the function
   is large, then this table can be too far away from the locations in the code
   that need to load the values.  One approach to handling this would be to
   reserve a register pointing to the literal pool, but that's costly when
   register pressure is high and may still not be sufficient if the number of
   literals within the pool is very large.

   This code takes an alternative approach, which is more suitable for RISC
   architectures with limited PC-relative offset addressing: we divide the
   literal pool into smaller pools (minipools) and place those inline within
   the body of the function such that every instruction that needs to load a
   literal can do so directly with a PC-relative offset.

   It is important to minimize the number of minipools because very small pools
   waste cache space on most Harvard architecture caches if a line contains
   both code and data.  Additionally, we may need to insert branches around a
   minipool, which adds further instructions to the function.

   Normally we can hide a minipool after an existing unconditional branch so
   that there is no interruption of the flow, but in the worst case the code
   looks like this:

	ldr	Rn, L1
	...
	b	L2
	align
   L1:	.long value
   L2:
	...

	ldr	Rn, L3
	...
	b	L4
	align
   L3:	.long value
   L4:
	...

   We do this by performing a scan after scheduling, identifying which
   instructions need to have their operands fetched from the literal pool and
   building the minipools.

   The algorithm starts by building a list of all the locations in the
   instruction stream that need fixing up and all the natural barriers in the
   function (places where constants can be dropped without breaking the flow).
   For each fixup we note how far the PC-relative replacement will be able to
   reach and the address of the instruction in the function.

   The next step is to assign fixups to a minipool.  This is done linearly
   through the function: fixups are added to a minipool until adding a fixup
   would break the constraints on the pool.  The placement of the pool in the
   instruction stream is then determined, and we attempt to satisfy further
   fixups by creating backward references to the placed pool.  As soon as a
   backward reference fails the pool is emitted and we start building a new
   minipool.  The process continues until there are no more fixups to handle.
   The final minipool will always be placed at the end of the function if that
   can be done while still satisfying the pool's constraints.

   To optimize pool size, we try to share pool entries if the fixups describe
   the same value.  This involves calculating the combined constraints for the
   referencing fixups and shuffling the order of entries within the pool to
   keep the constraints as loose as possible.  Both forward and backward
   references to the pool can share an entry, but if a backward reference is
   unable to address an existing entry we will try to insert a second copy of
   the same value.

   Each minipool is structured as a series of subpools, such that every entry
   in a subpool has the same alignment restriction.  When a minipool is
   finally emitted, the subpools are emitted in decreasing order of
   alignment.  This ensures that alignment is only needed before the pool and
   never within it.  As each pool is emitted, the relevant operand in the
   referencing instruction is adjusted to fetch the required value from the
   pool.

   When placing a minipool we try to place it after a barrier instruction,
   since control will never flow through this point.  If no suitable natural
   barrier exists, we select a location at which to create one, preferring
   locations around jumps and labels where possible.  If necessary, this may
   break a linear stream of instructions.  A new jump instruction and barrier
   are then inserted to jump over the literal pool.

   Note that this pass breaks the CFG layout data structures, so must only
   be run once that information is no longer needed.

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be a benefit in
   aligning the pools to the start of cache lines; this additional alignment
   would need to be taken into account when calculating addressability of a
   pool.  */

#define IN_TARGET_CODE 1

#define INCLUDE_LIST
#define INCLUDE_ALGORITHM
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "memmodel.h"
#include "rtl.h"
#include "emit-rtl.h"
#include "print-rtl.h"
#include "output.h"
#include "insn-attr.h"
#include "optabs.h"
#include "recog.h"
#include "obstack.h"

#include "minipool.h"

namespace minipools {

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *minipool_startobj;

/* Forward definitions of types.  */
typedef struct minipool_node Mnode;
typedef struct minipool_fixup Mfix;

typedef HOST_WIDE_INT minipool_address;
#define IS_MINIPOOL_ADDRESS_MIN(a) (a == HOST_WIDE_INT_MIN)
#define IS_MINIPOOL_ADDRESS_MAX(a) (a == HOST_WIDE_INT_MAX)
#define MINIPOOL_ADDRESS_MIN HOST_WIDE_INT_MIN
#define MINIPOOL_ADDRESS_MAX HOST_WIDE_INT_MAX

struct minipool_node
{
  /* The highest address that this entry can be placed.  While pushing fixes
     for forward references, all entries are sorted in order of increasing
     max_address within each subpool.  */
  minipool_address max_address;
  /* The lowest address that this entry can be placed for a backward
     reference.  */
  minipool_address min_address;
  /* The number of fixes referencing this entry.  This can become zero
     if we "unpush" an entry.  In this case we ignore the entry when we
     come to emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  machine_mode mode;
  /* The size of the value.  */
  int fix_size;
};

struct minipool_fixup
{
  /* The next fixup in the insn stream.  */
  Mfix *next;
  /* The insn that needs adjusting, or a barrier insn.  */
  rtx_insn *insn;
  /* The address that we calculate for the placement of the instruction
     relative to the start of the function.  Note that excludes any
     later adjustments that will occur as minipools get placed, but that
     doesn't matter because we never reach over one minipool to an
     earlier or later one.  */
  minipool_address address;
  /* The size of the insn in the stream.  */
  unsigned insn_size;
  /* Pointer to the operand in the insn that needs adjusting to reference
     the minipool.  */
  rtx *operand_loc;
  /* The machine mode associated with the literal pool entry.  Necessary
     because VALUE will often lack a mode.  */
  machine_mode mode;
  /* The size of the literal pool entry that will be needed.  For entries
     smaller than the least aligned subpool, this is rounded up to a multiple
     of the subpool alignment.  */
  int fix_size;
  /* The value that will need to be placed in the minipool.  */
  rtx value;
  /* Once the fix has been tentatively assigned to a minipool, the
     minipool that will hold this fix.  */
  Mnode *minipool;
  /* The highest address that this insn can use when using a forward
     reference.  */
  minipool_address max_address;
  /* The lowest address that this insn can use when using a backward reference.
     This may be negative if the insn could reach back further than the start
     of the function.  */
  minipool_address min_address;
};

/* Each minipool is divided into one or more subpools, where all the elements
   in each subpool share a common alignment (though not necessarily a common
   size).  When we emit the pool it will be aligned to the most aligned
   subpool and the subpools are emitted in decreasing order of alignment
   This ensures we only need one alignment directive before the start of the
   pool.  */
class subpool
{
 public:
  explicit subpool (HOST_WIDE_INT align);
  void reset ();
  unsigned align () const
    { return m_alignment; }
  unsigned size () const
    { return m_size; }
  Mnode *add_forward_ref (Mfix *);
  Mnode *add_backward_ref (Mfix *);
  HOST_WIDE_INT assign_offsets (HOST_WIDE_INT);
  minipool_address backwards () const
    { return m_min_address; }
  minipool_address forwards () const
    { return m_max_address; }
  rtx_insn *dump (rtx_insn *);
  void update_bounds (void);
  void debug (void) const;
 private:
  Mnode *move_fix_forward_ref (std::list<Mnode *>::iterator,
			       std::list<Mnode *>::iterator,
			       minipool_address);
  Mnode *move_fix_backward_ref (std::list<Mnode *>::iterator,
				std::list<Mnode *>::iterator,
				minipool_address);
  std::list<Mnode *> m_elements;
  /* Current size of the subpool.  */
  HOST_WIDE_INT m_size;
  /* The lowest address at which the end of this subpool can be located while
     still allowing all the elements it contains to be reached by referencing
     insns.  */
  minipool_address m_min_address;
  /* The highest address at which the start of this subpool can be located
     while still allowing all the elements it contains to be reached by
     referencing insns.  */
  minipool_address m_max_address;
  /* The subpool's alignment.  */
  const unsigned m_alignment;
};

subpool::subpool (HOST_WIDE_INT align)
  : m_alignment (align)
{
  reset ();
}

void
subpool::reset ()
{
  m_elements.clear ();
  m_min_address = MINIPOOL_ADDRESS_MIN;
  m_max_address = MINIPOOL_ADDRESS_MAX;
  m_size = 0;
}

class minipool
{
 public:
  explicit minipool (unsigned insn_align);
  void reset ();
  rtx_code_label *label ();
  Mnode *add_forward_ref (Mfix *, minipool_address);
  Mnode *add_backward_ref (Mfix *, minipool_address);
  void assign_offsets (Mfix *);
  void dump (rtx_insn *);
  Mfix *create_fix_barrier (Mfix *, minipool_address);
  HOST_WIDE_INT get_label_padding (rtx label) const;
  HOST_WIDE_INT get_alignment_size (void) const;
  minipool_address forwards (void) const
    { return m_max_address; }
  unsigned size () const
    { return m_size; }
  void update_bounds (void);
  void debug (void) const;
 private:
  std::list<subpool *>::iterator find_subpool (unsigned align, Mfix *fix,
					       minipool_address loc,
					       HOST_WIDE_INT padding);
  std::list<subpool *> m_subpools;
  /* The aligned label for the base of the minipool.  All references to
     the pool will offset from this label.  */
  rtx_code_label *m_vector_label;
  /* The highest address at which the pool can start whilst all the
     subpool constraint can be met.  This is only relevant while pushing
     forward references.  After that point the pool's location is fixed.  */
  minipool_address m_max_address;
  /* The insn alignment.  */
  const unsigned m_insn_align;
  /* Total size of the pool.  */
  unsigned m_size;
};

minipool::minipool (unsigned insn_align)
  : m_insn_align (insn_align)
{
  reset ();
}

void
minipool::reset ()
{
  m_subpools.clear ();
  m_vector_label = NULL;
  m_max_address = MINIPOOL_ADDRESS_MAX;
  m_size = 0;
}

rtx_code_label *
minipool::label ()
{
  if (!m_vector_label)
    m_vector_label = gen_label_rtx ();
  return m_vector_label;
}

/* The linked list of all minipool fixes required for this function.  */
Mfix *minipool_fix_head;
Mfix *minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *minipool_barrier;

static void push_minipool_barrier (rtx_insn *, HOST_WIDE_INT);
static void push_minipool_fix (rtx_insn *, HOST_WIDE_INT, rtx *,
			       machine_mode, rtx);
static int barrier_cost (rtx_insn *);

void add_gc_roots (void)
{
  gcc_obstack_init (&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}

/* Print a symbolic form of X to the debug file, F.  */
static void
print_value (FILE *f, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case CONST_DOUBLE:
      {
	char fpstr[20];
	real_to_decimal (fpstr, CONST_DOUBLE_REAL_VALUE (x),
			 sizeof (fpstr), 0, 1);
	fputs (fpstr, f);
      }
      return;

    case CONST_VECTOR:
      {
	int i;

	fprintf (f, "<");
	for (i = 0; i < CONST_VECTOR_NUNITS (x); ++i)
	  {
	    fprintf (f, HOST_WIDE_INT_PRINT_HEX,
		     INTVAL (CONST_VECTOR_ELT (x, i)));
	    if (i < (CONST_VECTOR_NUNITS (x) - 1))
	      fputc (',', f);
	  }
	fprintf (f, ">");
      }
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      print_value (f, XEXP (x, 0));
      return;

    case PLUS:
      print_value (f, XEXP (x, 0));
      fprintf (f, "+");
      print_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
    }
}

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0
#endif

static HOST_WIDE_INT
get_jump_table_size (rtx_jump_table_data *insn)
{
  /* ADDR_VECs only take room if read-only data does into the text
     section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION || readonly_data_section == text_section)
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;
      HOST_WIDE_INT size;
      HOST_WIDE_INT modesize;

      modesize = GET_MODE_SIZE (GET_MODE (body));
      size = modesize * XVECLEN (body, elt);
      switch (modesize)
	{
	case 1:
	  /* Round up size  of TBB table to a halfword boundary.  */
	  size = (size + 1) & ~HOST_WIDE_INT_1;
	  break;
	case 2:
	  /* No padding necessary for TBH.  */
	  break;
	case 4:
	  /* Add two bytes for alignment on Thumb.  */
	  if (TARGET_THUMB)
	    size += 2;
	  break;
	default:
	  gcc_unreachable ();
	}
      return size;
    }

  return 0;
}

/* Return the cost of forcibly inserting a barrier after INSN.  */
static int
barrier_cost (rtx_insn *insn)
{
  /* Basing the location of the pool on the loop depth is preferable, but at
     the moment, the basic block information seems to be corrupt by this
     stage of the compilation.  */
  int base_cost = 50;
  rtx_insn *next = next_nonnote_insn (insn);

  if (next != NULL && LABEL_P (next))
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
	 than after it.  */
      return 50;

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}

/* Fixes less than a word need padding out to a word boundary.  */
#define MINIPOOL_FIX_SIZE(mode)					\
  (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4)

/* (Re-)compute the bounds of the subpool.  */
void
subpool::update_bounds (void)
{
  /* We don't know the size of the subpool so the forward and backward
     bounds have to be calculated separately, counting from opposite
     ends of the subpool.  */
  m_max_address = MINIPOOL_ADDRESS_MAX;
  HOST_WIDE_INT offset = HOST_WIDE_INT_0;
  std::list<Mnode *>::iterator mpi;

  for (mpi = m_elements.begin (); mpi != m_elements.end (); ++mpi)
    {
      Mnode *mp = *mpi;
      if (!mp->refcount)
	continue;
      if (!IS_MINIPOOL_ADDRESS_MAX (mp->max_address))
	m_max_address = std::min (m_max_address, mp->max_address - offset);
      offset += mp->fix_size;
    }

  m_size = offset;

  m_min_address = MINIPOOL_ADDRESS_MIN;
  offset = HOST_WIDE_INT_0;
  std::list<Mnode *>::reverse_iterator rmpi;
  for (rmpi = m_elements.rbegin (); rmpi != m_elements.rend (); ++rmpi)
    {
      Mnode *mp = *rmpi;
      if (!mp->refcount)
	continue;
      offset += mp->fix_size;
      if (!IS_MINIPOOL_ADDRESS_MIN (mp->min_address))
	m_min_address = std::max (m_min_address, mp->min_address + offset);
    }
}

/* Move a minipool fix MPI from its current location to before MAX_MPI.  If
   MAX_MPI points to the end of the list, then MPI doesn't need moving, but
   the addressing constraints may still need updating.  */
Mnode *
subpool::move_fix_forward_ref (std::list<Mnode *>::iterator mpi,
			       std::list<Mnode *>::iterator max_mpi,
			       minipool_address fix_max_address)
{
  /* The code below assumes these are different.  */
  gcc_assert (mpi != max_mpi);

  Mnode *mp = *mpi;

  mp->max_address = std::min (mp->max_address, fix_max_address);
  if (max_mpi != m_elements.end ())
    m_elements.splice (max_mpi, m_elements, mpi);

  update_bounds ();
  return mp;
}

/* Add a constant to the subpool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */
Mnode *
subpool::add_forward_ref (Mfix *fix)
{
  /* max_mpi points to the first subpool entry that has a less strict
     constraint than the one we are trying to add.  */
  std::list<Mnode *>::iterator max_mpi = m_elements.end ();
  /* Scan the subpool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  std::list<Mnode *>::iterator mpi = m_elements.begin ();
  for (; mpi != m_elements.end (); ++mpi)
    {
      Mnode *mp = *mpi;
      /* This is perhaps too strict.  At least on little-endian platforms
	 we could share CONST_INT values where they will be loaded into
	 different modes.  */
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (!LABEL_P (fix->value)
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return move_fix_forward_ref (mpi, max_mpi, fix->max_address);
	}

      /* Note the insertion point if necessary.  Conceptually the test here
	 is mp->max + mp->size > fix->max + fix->size, but that might
	 overflow if mp->max is MINIPOOL_ADDRESS_MAX.  */
      if (max_mpi == m_elements.end ()
	  && (mp->max_address - fix->fix_size
	      > fix->max_address - mp->fix_size))
	max_mpi = mpi;
    }

  /* The value is not currently in the subpool, so we need to create a new
     entry for it.  */
  Mnode *mp = XNEW (Mnode);
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->offset = 0;
  /* Not yet required for a backward reference.  */
  mp->min_address = MINIPOOL_ADDRESS_MIN;

  mp->max_address = fix->max_address;
  mpi = m_elements.insert (max_mpi, mp);

  update_bounds ();
  return mp;
}

Mnode *
subpool::move_fix_backward_ref (std::list<Mnode *>::iterator mpi,
				std::list<Mnode *>::iterator min_mpi,
				minipool_address  fix_min_address)
{
  /* The code below assumes these are different.  */
  gcc_assert (std::next (mpi) != min_mpi);

  Mnode *mp = *mpi;

  mp->min_address = std::max (mp->min_address, fix_min_address);
  /* We're moving an entry later in the list, so the insertion point
     of begin () can never happen.  We use this as the sentinel for
     not moving the element.  */
  if (min_mpi != m_elements.begin ())
    m_elements.splice (min_mpi, m_elements, mpi);

  update_bounds ();
  return mp;
}

/* Add a constant to the subpool for a backward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.

   Note that offsets for a backward reference are calculated relative to
   the end of the subpool.  */
Mnode *
subpool::add_backward_ref (Mfix *fix)
{
  /* This can be negative, since it is only a constraint.  */
  HOST_WIDE_INT  min_address = fix->min_address;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  std::list<Mnode *>::reverse_iterator rmpi = m_elements.rbegin ();
  std::list<Mnode *>::reverse_iterator min_rmpi = m_elements.rend ();
  for (; rmpi != m_elements.rend (); ++rmpi)
    {
      Mnode *mp = *rmpi;
      /* This is perhaps too strict.  At least on little-endian platforms
	 we could share CONST_INT values where they will be loaded into
	 different modes.  */
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (!LABEL_P (fix->value)
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  std::list<Mnode *>::iterator before = min_rmpi.base ();
	  mp->refcount++;
	  /* If we can't safely move the entry later in the pool, then
	     we will need to do it in place.  */
	  if (mp->max_address - m_size <= m_max_address)
	    before = m_elements.begin ();
	  return move_fix_backward_ref (std::prev (rmpi.base ()),
					before, fix->min_address);
	}

      /* Note the insertion point if necessary.  */
      if (min_rmpi == m_elements.rend ()
	  && (mp->min_address + mp->fix_size
	      < fix->min_address + fix->fix_size))
	min_rmpi = rmpi;
    }

  /* We need to create a new entry.  */
  Mnode *mp = XNEW (Mnode);
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->offset = 0;
  mp->max_address = MINIPOOL_ADDRESS_MAX;

  mp->min_address = min_address;

  if (min_rmpi == m_elements.rend ())
    m_elements.insert (m_elements.begin (), mp);
  else
    m_elements.insert (min_rmpi.base (), mp);

  update_bounds ();
  return mp;
}

HOST_WIDE_INT
subpool::assign_offsets (HOST_WIDE_INT offset_in)
{
  HOST_WIDE_INT offset = offset_in;
  std::list<Mnode *>::iterator mpi = m_elements.begin ();
  for (; mpi != m_elements.end (); ++mpi)
    {
      Mnode *mp = *mpi;
      mp->offset = offset;

      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
  /* The final size of this subpool.  */
  m_size = offset - offset_in;
  return offset;
}

rtx_insn *
subpool::dump (rtx_insn *scan)
{
  if (dump_file)
    fprintf (dump_file, ";; subpool with alignment %u\n", m_alignment);

  std::list<Mnode *>::iterator mpi;
  for (mpi = m_elements.begin (); mpi != m_elements.end (); ++mpi)
    {
      Mnode *mp = *mpi;
      if (mp->refcount > 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset,
		       (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      print_value (dump_file, mp->value);
	      fputc ('\n', dump_file);
	    }

	  rtx val = copy_rtx (mp->value);

	  switch (GET_MODE_SIZE (mp->mode))
	    {
#ifdef HAVE_consttable_1
	    case 1:
	      scan = emit_insn_after (gen_consttable_1 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_2
	    case 2:
	      scan = emit_insn_after (gen_consttable_2 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_4
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_8
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (val), scan);
	      break;

#endif
#ifdef HAVE_consttable_16
	    case 16:
	      scan = emit_insn_after (gen_consttable_16 (val), scan);
	      break;

#endif
	    default:
	      gcc_unreachable ();
	    }
	}
      free (mp);
    }
  return scan;
}

void
subpool::debug (void) const
{
  fprintf (stderr, " subpool @%p", (const void *) this);
  fprintf (stderr, "  alignment: %u", m_alignment);
  fprintf (stderr, "  size: " HOST_WIDE_INT_PRINT_DEC "\n", m_size);
  if (IS_MINIPOOL_ADDRESS_MIN (m_min_address))
    fprintf (stderr, "  min_address: -\n");
  else
    fprintf (stderr, "  min_address: " HOST_WIDE_INT_PRINT_DEC "\n",
	     (HOST_WIDE_INT) m_min_address);
  if (IS_MINIPOOL_ADDRESS_MAX (m_max_address))
    fprintf (stderr, "  max_address: -\n");
  else
    fprintf (stderr, "  max_address: " HOST_WIDE_INT_PRINT_DEC "\n",
	     (HOST_WIDE_INT) m_max_address);

  std::list<Mnode *>::const_iterator mpi = m_elements.begin ();

  for (; mpi != m_elements.end (); ++mpi)
    {
      Mnode *mp = *mpi;
      fprintf (stderr, "   mode: %s; min: ", GET_MODE_NAME (mp->mode));
      if (IS_MINIPOOL_ADDRESS_MIN (mp->min_address))
	fprintf (stderr, "-");
      else
	fprintf (stderr, HOST_WIDE_INT_PRINT_DEC,
		 (HOST_WIDE_INT) mp->min_address);
      fprintf (stderr, "; max: ");
      if (IS_MINIPOOL_ADDRESS_MAX (mp->max_address))
	fprintf (stderr, "-");
      else
	fprintf (stderr, HOST_WIDE_INT_PRINT_DEC,
		 (HOST_WIDE_INT) mp->max_address);
      fprintf (stderr,
	       "; size: %d"
	       "; offset: " HOST_WIDE_INT_PRINT_DEC
	       "; refcount: %d"
	       "; value: ",
	       mp->fix_size, mp->offset, mp->refcount);
      ::debug (mp->value);
    }
}

/* Return the maximum amount of padding that will be inserted before label
   LABEL.  */
HOST_WIDE_INT
minipool::get_label_padding (rtx label) const
{
  HOST_WIDE_INT align;

  align = 1 << label_to_alignment (label).levels[0].log;
  return align > m_insn_align ? align - m_insn_align : 0;
}

HOST_WIDE_INT
minipool::get_alignment_size (void) const
{
  if (m_subpools.empty ())
    return m_insn_align;
  return m_subpools.front ()->align () - m_insn_align;
}

/* Find the subpool that needs to hold an element of alignment ALIGN.  We need
   to ensure that this won't make existing references unreachable.  */
std::list<subpool *>::iterator
minipool::find_subpool (unsigned align, Mfix *fix, minipool_address loc,
			HOST_WIDE_INT padding)
{
  std::list<subpool *>::iterator spi = m_subpools.begin ();
  subpool *sp;

  /* A new minipool has no existing placement constraints.  */
  if (spi == m_subpools.end ())
    {
      sp = XNEW (subpool);
      ::new (static_cast<void*> (sp)) subpool (align);
      m_subpools.push_back (sp);
      return m_subpools.begin ();
    }
  else
    {
      minipool_address minloc = MINIPOOL_ADDRESS_MIN;
      minipool_address maxloc = MINIPOOL_ADDRESS_MAX;

      sp = NULL;
      std::list<subpool *>::iterator before = m_subpools.end ();
      unsigned offset = 0;
      for (; spi != m_subpools.end (); ++spi)
	{
	  unsigned subpool_size = (*spi)->size ();
	  unsigned reverse_offset = m_size - offset - subpool_size;
	  if ((*spi)->align () > align)
	    {
	      /* Subpools before our subpool.  Need to check this
		 doesn't affect backward reachability.  */
	      minloc = std::max (minloc,
				 (*spi)->backwards () + reverse_offset);
	    }
	  else if ((*spi)->align () == align)
	    {
	      sp = *spi;
	      minloc = std::max (minloc,
				 (*spi)->backwards () + reverse_offset);
	      maxloc = std::min (maxloc, (*spi)->forwards () - offset);
	    }
	  else
	    {
	      /* Subpools after our subpool.  Need to check this
		 doesn't affect forward reachability.  */
	      maxloc = std::min (maxloc, (*spi)->forwards () - offset);
	      if (before == m_subpools.end ())
		before = spi;
	    }
	  offset += (*spi)->size ();
	}

      /* If we're going to insert a new subpool with more alignment, we
	 need to check the additional padding as well.  */
      if (before == m_subpools.begin ())
	padding += align - (*before)->align ();

      if (fix->address < loc)
	{
	  /* A forward reference, because we've not yet placed the pool
	     we don't need to worry about backward references.  */
	  if (maxloc - (fix->fix_size + padding) < loc)
	    return m_subpools.end ();
	}
      else
	{
	  /* A backward reference.  In addition to checking backward
	     references, we need to check that inserting FIX into the pool
	     won't break forward references.  */
	  if (maxloc - (fix->fix_size + padding) < loc
	      || minloc + fix->fix_size > loc)
	    return m_subpools.end ();
	}

      if (sp)
	return std::prev (before);

      sp = XNEW (subpool);
      ::new (static_cast<void*> (sp)) subpool (align);
      return m_subpools.insert (before, sp);
    }
}

/* Try to insert FIX into the current minipool.  LOC is the earliest
   address at which we can insert the pool.  */
Mnode *
minipool::add_forward_ref (Mfix *fix, minipool_address loc)
{
  unsigned align = GET_MODE_ALIGNMENT (fix->mode) / BITS_PER_UNIT;
  /* Pool entries must be at least word aligned.  */
  align = std::max (align, 4u);
  /* And at most double-word aligned, if we require double-word
     alignment.  */
  align = std::min (align, ARM_DOUBLEWORD_ALIGN ? 8u: 4u);

  if (loc > m_max_address)
    return NULL;
  std::list<subpool *>::iterator spi
    = find_subpool (align, fix, loc, get_alignment_size ());
  if (spi == m_subpools.end ())
    return NULL;
  subpool *sp = *spi;
  Mnode *mp = sp->add_forward_ref (fix);
  unsigned offset = 0;
  for (spi = m_subpools.begin (); spi != m_subpools.end (); ++spi)
    {
      m_max_address = std::min (m_max_address, (*spi)->forwards () - offset);
      offset += (*spi)->size ();
    }
  m_size = offset;
  return mp;
}

/* Try to insert FIX into the current minipool.  LOC is the confirmed
   address at which the pool will be placed; for backward refs this
   will be the end of the minipool.  */
Mnode *
minipool::add_backward_ref (Mfix *fix, minipool_address loc)
{
  unsigned align = GET_MODE_ALIGNMENT (fix->mode) / BITS_PER_UNIT;
  /* Pool entries must be at least word aligned.  */
  align = std::max (align, 4u);
  /* And at most double-word aligned, if we require double-word
     alignment.  */
  align = std::min (align, ARM_DOUBLEWORD_ALIGN ? 8u: 4u);

  /* We don't need to take account of pool alignment for backward refs;
     the pool is always a multiple of the instruction alignment.  */

  /* Ensure we haven't moved beyond the addressable limit of the pool.  */
  if (fix->min_address > loc - m_size - fix->fix_size)
    return NULL;

  std::list<subpool *>::iterator spi
    = find_subpool (align, fix, loc, get_alignment_size ());
  if (spi == m_subpools.end ())
    return NULL;

  subpool *sp = *spi;
  unsigned old_size = sp->size ();
  Mnode *mp = sp->add_backward_ref (fix);
  m_size += sp->size () - old_size;
  return mp;
}

void
minipool::update_bounds (void)
{
  std::list<subpool *>::iterator spi = m_subpools.begin ();
  unsigned size = 0;
  for (; spi != m_subpools.end (); ++spi)
    {
      (*spi)->update_bounds ();
      size += (*spi)->size ();
    }
  m_size = size;
}

void
minipool::assign_offsets (Mfix *barrier)
{
  HOST_WIDE_INT offset = 0;

  minipool_barrier = barrier;

  std::list<subpool *>::iterator spi = m_subpools.begin ();
  for (; spi != m_subpools.end (); ++spi)
    offset = (*spi)->assign_offsets (offset);
}

/* Output the literal table.  */
void
minipool::dump (rtx_insn *scan)
{
  unsigned align = 0;

  std::list<subpool *>::iterator spi = m_subpools.begin ();
  for (; spi != m_subpools.end (); ++spi)
    if ((*spi)->size ())
      {
	align = (*spi)->align ();
	break;
      }

  gcc_assert (align != 0);

  if (dump_file)
    fprintf (dump_file,
	     ";; Emitting minipool after insn %u; address %ld; "
	     "align %d (bytes)\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address,
	     align);

  scan = emit_label_after (gen_label_rtx (), scan);
  scan = emit_insn_after (align == 8 ? gen_align_8 () : gen_align_4 (),
			  scan);
  scan = emit_label_after (m_vector_label, scan);
  for (spi = m_subpools.begin (); spi != m_subpools.end (); ++spi)
    {
      scan = (*spi)->dump (scan);
      free (*spi);
    }

  reset ();
  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
}

/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */
Mfix *
minipool::create_fix_barrier (Mfix *fix, minipool_address max_address)
{
  HOST_WIDE_INT count = 0;
  rtx_barrier *barrier;
  rtx_insn *from = fix->insn;
  /* The instruction after which we will insert the jump.  */
  rtx_insn *selected = NULL;
  int selected_cost;
  /* The address at which the jump instruction will be placed.  */
  HOST_WIDE_INT selected_address;
  Mfix * new_fix;
  HOST_WIDE_INT max_count = max_address - fix->address;
  rtx_code_label *label = gen_label_rtx ();

  selected_cost = barrier_cost (from);
  selected_address = fix->address;

  while (from && count < max_count)
    {
      rtx_jump_table_data *tmp;
      int new_cost;

      /* This code shouldn't have been called if there was a natural barrier
	 within range.  */
      gcc_assert (!BARRIER_P (from));

      /* Count the length of this insn.  This must stay in sync with the
	 code that pushes minipool fixes.  */
      if (LABEL_P (from))
	count += get_label_padding (from);
      else
	count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      if (tablejump_p (from, NULL, &tmp))
	{
	  count += get_jump_table_size (tmp);

	  /* Jump tables aren't in a basic block, so base the cost on
	     the dispatch insn.  If we select this location, we will
	     still put the pool after the table.  */
	  new_cost = barrier_cost (from);

	  if (count < max_count
	      && (!selected || new_cost <= selected_cost))
	    {
	      selected = tmp;
	      selected_cost = new_cost;
	      selected_address = fix->address + count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (tmp);
	  continue;
	}

      new_cost = barrier_cost (from);

      if (count < max_count
	  && (!selected || new_cost <= selected_cost))
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = fix->address + count;
	}

      from = NEXT_INSN (from);
    }

  /* Make sure that we found a place to insert the jump.  */
  gcc_assert (selected);

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  new_fix->next = fix->next;
  fix->next = new_fix;

  return new_fix;
}

void
minipool::debug (void) const
{
  fprintf (stderr, "Minipool @%p\n", (const void *) this);
  fprintf (stderr, " insn_alignment: %u\n", m_insn_align);
  fprintf (stderr, " vector_label: %p ", (const void *) m_vector_label);
  ::debug ((rtx) m_vector_label);

  std::list<subpool *>::const_iterator spi = m_subpools.begin ();
  for (; spi != m_subpools.end (); ++spi)
    (*spi)->debug ();
}

/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */
static void
push_minipool_barrier (rtx_insn *insn, HOST_WIDE_INT address)
{
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */
static void
push_minipool_fix (rtx_insn *insn, HOST_WIDE_INT address, rtx *loc,
		   machine_mode mode, rtx value)
{
  gcc_assert (!arm_disable_literal_pool);
  Mfix * fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (* fix));

  fix->insn = insn;
  fix->address = address;
  fix->operand_loc = loc;
  fix->mode = mode;
  fix->fix_size = MINIPOOL_FIX_SIZE (mode);
  fix->value = value;
  fix->max_address = get_attr_pool_range (insn);
  fix->min_address = -get_attr_neg_pool_range (insn);
  fix->insn_size = get_attr_length (insn);
  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to stop now than
     to generate duff assembly code.  */
  gcc_assert (fix->max_address || fix->min_address);
  fix->max_address += address;
  fix->min_address += address;

  if (dump_file)
    {
      fprintf (dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address,
	       (long)fix->min_address, (long)fix->max_address);
      print_value (dump_file, fix->value);
      fprintf (dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;

  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Scan INSN and note any of its operands that need fixing.
   If DO_PUSHES is false we do not actually push any of the fixups
   needed.  */
static void
note_invalid_constants (rtx_insn *insn, HOST_WIDE_INT address, int do_pushes)
{
  int opno;

  extract_constrain_insn (insn);

  if (recog_data.n_alternatives == 0)
    return;

  /* Fill in recog_op_alt with information about the constraints of
     this insn.  */
  preprocess_constraints (insn);

  const operand_alternative *op_alt = which_op_alt ();
  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs.  */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      /* If this alternative is a memory reference, then any mention
	 of constants in this alternative is really to fool reload
	 into allowing us to accept one there.  We need to fix them up
	 now so that we output the right code.  */
      if (op_alt[opno].memory_ok)
	{
	  rtx op = recog_data.operand[opno];

	  if (CONSTANT_P (op))
	    {
	      if (do_pushes)
		push_minipool_fix (insn, address,
				   recog_data.operand_loc[opno],
				   recog_data.operand_mode[opno], op);
	    }
	  else if (MEM_P (op)
		   && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
	    {
	      if (do_pushes)
		{
		  rtx cop = avoid_constant_pool_reference (op);

		  /* Casting the address of something to a mode narrower than a
		     word can cause avoid_constant_pool_reference to return the
		     pool reference itself.  That's no good to us here.  Lets
		     just hope that we can use the constant pool value
		     directly.  */
		  if (op == cop)
		    cop = get_pool_constant (XEXP (op, 0));

		  push_minipool_fix (insn, address,
				     recog_data.operand_loc[opno],
				     recog_data.operand_mode[opno], cop);
		}

	    }
	}
    }

  return;
}

/* GCC normally builds a single literal pool for an entire function,
   but PC-relative loads on Arm (and especially thumb1) have very
   limited offsets, which means accessing values through a single
   pool is expensive if we have to build an address for the pool entry.
   To mitigate this we build tables of literals into the body of a function
   if it is too large to put them at the end, then place the tables
   inside the function body (ideally after unconditional branches).  */
void
place_minipools (void)
{
  rtx_insn *insn;
  HOST_WIDE_INT address = 0;
  Mfix *fix;
  minipool mpool (TARGET_THUMB ? 2u : 4u);
  /* Length of a branch sequence to skip a pool, if we needed to force
     a barrier.  */
  unsigned pool_skip = TARGET_THUMB ? 2u : 4u;

  /* Ensure all insns that must be split have been split at this point.
     Otherwise, the pool placement code below may compute incorrect
     insn lengths.  Note that when optimizing, all insns have already
     been split at this point.  */
  if (!optimize)
    split_all_insns_noflow ();

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't
     scan it properly.  */
  insn = get_insns ();
  gcc_assert (NOTE_P (insn));

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (insn); insn;
       insn = next_nonnote_insn (insn))
    {
      if (BARRIER_P (insn))
	push_minipool_barrier (insn, address);
      else if (INSN_P (insn))
	{
	  rtx_jump_table_data *table;

	  note_invalid_constants (insn, address, true);
	  address += get_attr_length (insn);

	  /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	  if (tablejump_p (insn, NULL, &table))
	    {
	      address += get_jump_table_size (table);
	      insn = table;
	    }
	}
      else if (LABEL_P (insn))
	/* Add the worst-case padding due to alignment.  We don't add
	   the _current_ padding because the minipool insertions
	   themselves might change it.  */
	address += mpool.get_label_padding (insn);
    }

  fix = minipool_fix_head;

  /* Now scan the fixups and perform the required changes.  */
  while (fix)
    {
      Mfix *ftmp;
      Mfix *fdel;
      Mfix *last_added_fix;
      Mfix *last_barrier = NULL;
      Mfix *this_fix;

      /* Skip any further barriers before the next fix.  */
      while (fix && BARRIER_P (fix->insn))
	fix = fix->next;

      /* No more fixes.  */
      if (fix == NULL)
	break;

      last_added_fix = NULL;

      for (ftmp = fix; ftmp; ftmp = ftmp->next)
	{
	  if (BARRIER_P (ftmp->insn))
	    {
	      if (ftmp->address + mpool.get_alignment_size ()
		  > mpool.forwards ())
		break;

	      last_barrier = ftmp;
	    }
	  else
	    {
	      minipool_address mp_addr
		= ftmp->address + ftmp->insn_size + pool_skip;
	      if ((ftmp->minipool = mpool.add_forward_ref (ftmp, mp_addr))
		  == NULL)
		break;
	    }

	  last_added_fix = ftmp;  /* Keep track of the last fix added.  */
	}

      /* If we found a barrier, drop back to that; any fixes that we could have
	 reached but come after the barrier will now go in the next mini-pool
	 (if they are not re-added as backward references).  */
      if (last_barrier != NULL)
	{
	  /* Reduce the refcount for those fixes that won't go into this pool
	     after all.  Note that if the insn supports backward references we
	     will probably end up re-adding this value to the same pool, so
	     it's not worth the effort of unlinking it from the chain.  */
	  for (fdel = last_barrier->next;
	       fdel && fdel != ftmp;
	       fdel = fdel->next)
	    {
	      fdel->minipool->refcount--;
	      /* If there are no more forward references to the entry, then the
		 forward address is no longer constrained.  */
	      if (fdel->minipool->refcount == 0)
		fdel->minipool->max_address = MINIPOOL_ADDRESS_MAX;
	      fdel->minipool = NULL;
	    }

	  mpool.update_bounds ();
	  ftmp = last_barrier;
	}
      else
	{
	  /* ftmp is first fix that we can't fit into this pool and
	     there no natural barriers that we could use.  Insert a
	     new barrier in the code somewhere between the previous
	     fix and this one, and arrange to jump around it.  */
	  minipool_address max_address;

	  /* The last item on the list of fixes must be a barrier, so
	     we can never run off the end of the list of fixes without
	     last_barrier being set.  */
	  gcc_assert (ftmp);

	  max_address = mpool.forwards ();
	  /* Check that there isn't another fix that is in range that
	     we couldn't fit into this pool because the pool was
	     already too large: we need to put the pool before such an
	     instruction.  The pool itself may come just after the
	     fix because create_fix_barrier also allows space for a
	     jump instruction.  */
	  if (ftmp->address < max_address)
	    max_address = ftmp->address + 1;

	  last_barrier = mpool.create_fix_barrier (last_added_fix,
						   max_address);
	}

      while (ftmp)
	{
	  if (!BARRIER_P (ftmp->insn)
	      && ((ftmp->minipool
		   = mpool.add_backward_ref (ftmp, last_barrier->address))
		  == NULL))
	    break;

	  ftmp = ftmp->next;
	}

      mpool.assign_offsets (last_barrier);

      /* Scan over the fixes we have identified for this pool, fixing them
	 up and adding the constants to the pool itself.  */
      for (this_fix = fix; this_fix && ftmp != this_fix;
	   this_fix = this_fix->next)
	if (!BARRIER_P (this_fix->insn))
	  {
	    rtx addr
	      = plus_constant (Pmode,
			       gen_rtx_LABEL_REF (VOIDmode,
						  mpool.label ()),
			       this_fix->minipool->offset);
	    *this_fix->operand_loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      mpool.dump (last_barrier->insn);
      fix = ftmp;
    }

  /* From now on we must synthesize any constants that we can't handle
     directly.  This can happen if the RTL gets split during final
     instruction generation.  */
  cfun->machine->after_arm_reorg = 1;

  /* Free the minipool memory.  */
  obstack_free (&minipool_obstack, minipool_startobj);
}

} // namespace minipools
