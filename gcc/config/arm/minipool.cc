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

/* Arm instructions cannot load a large constant directly into a
   register; they have to come from a pc relative load.  The constant
   must therefore be placed in the addressable range of the pc
   relative load.  Depending on the precise pc relative load
   instruction the range is somewhere between 256 bytes and 4k.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow
   things down and make the code larger.

   Normally we can hide the table after an existing unconditional
   branch so that there is no interruption of the flow, but in the
   worst case the code looks like this:

	ldr	rn, L1
	...
	b	L2
	align
	L1:	.long value
	L2:
	...

	ldr	rn, L3
	...
	b	L4
	align
	L3:	.long value
	L4:
	...

   We fix this by performing a scan after scheduling, which notices
   which instructions need to have their operands fetched from the
   constant table and builds the table.

   The algorithm starts by building a table of all the constants that
   need fixing up and all the natural barriers in the function (places
   where a constant table can be dropped without breaking the flow).
   For each fixup we note how far the pc-relative replacement will be
   able to reach and the offset of the instruction into the function.

   Having built the table we then group the fixes together to form
   tables that are as large as possible (subject to addressing
   constraints) and emit each table of constants after the last
   barrier that is within range of all the instructions in the group.
   If a group does not contain a barrier, then we forcibly create one
   by inserting a jump instruction into the flow.  Once the table has
   been inserted, the insns are then modified to reference the
   relevant entry in the pool.

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.  */

#define IN_TARGET_CODE 1

#define INCLUDE_LIST
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "memmodel.h"
#include "rtl.h"
#include "emit-rtl.h"
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
  typedef struct minipool_node    Mnode;
  typedef struct minipool_fixup   Mfix;

  struct minipool_node
  {
    /* The maximum offset into the code that this entry can be placed.  While
       pushing fixes for forward references, all entries are sorted in order
       of increasing max_address.  */
    HOST_WIDE_INT max_address;
    /* Similarly for an entry inserted for a backwards ref.  */
    HOST_WIDE_INT min_address;
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
    Mfix *            next;
    rtx_insn *        insn;
    HOST_WIDE_INT     address;
    rtx *             loc;
    machine_mode mode;
    int               fix_size;
    rtx               value;
    Mnode *           minipool;
    HOST_WIDE_INT     forwards;
    HOST_WIDE_INT     backwards;
  };

  class minipool
  {
  public:
    minipool () { reset (); }
    void reset()
      {
	elements.clear();
	vector_label = NULL;
      }
    Mnode *head () { return elements.front (); }
    rtx_code_label *label () { return vector_label; }
    Mnode *add_forward_ref (Mfix *);
    Mnode *add_backward_ref (Mfix *);
    void assign_offsets (Mfix *);
    void dump (rtx_insn *);
  private:
    Mnode *move_fix_forward_ref (std::list<Mnode *>::iterator,
				 std::list<Mnode *>::iterator, HOST_WIDE_INT);
    Mnode *move_fix_backward_ref (std::list<Mnode *>::iterator,
				  std::list<Mnode *>::iterator, HOST_WIDE_INT);
    std::list<Mnode *> elements;
    rtx_code_label *vector_label;
  };

  static int minipool_pad;

  /* The linked list of all minipool fixes required for this function.  */
  Mfix *minipool_fix_head;
  Mfix *minipool_fix_tail;
  /* The fix entry for the current minipool, once it has been placed.  */
  Mfix *minipool_barrier;


  static Mfix *create_fix_barrier (Mfix *, HOST_WIDE_INT);
  static void push_minipool_barrier (rtx_insn *, HOST_WIDE_INT);
  static void push_minipool_fix (rtx_insn *, HOST_WIDE_INT, rtx *,
				 machine_mode, rtx);
  static int barrier_cost (rtx_insn *);

  void add_gc_roots (void)
  {
    gcc_obstack_init(&minipool_obstack);
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
	  for (i = 0; i < CONST_VECTOR_NUNITS (x); i++)
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

  /* Return the maximum amount of padding that will be inserted before label
     LABEL.  */
  static HOST_WIDE_INT
  get_label_padding (rtx label)
  {
    HOST_WIDE_INT align, min_insn_size;

    align = 1 << label_to_alignment (label).levels[0].log;
    min_insn_size = TARGET_THUMB ? 2 : 4;
    return align > min_insn_size ? align - min_insn_size : 0;
  }

  /* Fixes less than a word need padding out to a word boundary.  */
#define MINIPOOL_FIX_SIZE(mode)					\
  (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4)

  /* Move a minipool fix MP from its current location to before MAX_MP.
     If MAX_MP is NULL, then MP doesn't need moving, but the addressing
     constraints may need updating.  */
  Mnode *
  minipool::move_fix_forward_ref (std::list<Mnode *>::iterator mpi,
				  std::list<Mnode *>::iterator max_mpi,
				  HOST_WIDE_INT max_address)
  {
    /* The code below assumes these are different.  */
    gcc_assert (mpi != max_mpi);

    Mnode *mp = *mpi;

    if (max_mpi == elements.end ())
      {
	if (max_address < mp->max_address)
	  mp->max_address = max_address;
      }
    else
      {
	Mnode *max_mp = *max_mpi;

	if (max_address > max_mp->max_address - mp->fix_size)
	  mp->max_address = max_mp->max_address - mp->fix_size;
	else
	  mp->max_address = max_address;

	elements.splice (max_mpi, elements, mpi);
      }

    Mnode *result = mp;

    /* Scan over the preceding entries and adjust their addresses as
       required.  */
    Mnode *pmp;
    for (; mpi != elements.begin (); mp = pmp, mpi--)
      {
	pmp = *std::prev (mpi);
	if (pmp->max_address <= mp->max_address - pmp->fix_size)
	  break;
	pmp->max_address = mp->max_address - pmp->fix_size;
      }

    return result;
  }

  /* Add a constant to the minipool for a forward reference.  Returns the
     node added or NULL if the constant will not fit in this pool.  */
  Mnode *
  minipool::add_forward_ref (Mfix *fix)
  {
    HOST_WIDE_INT max_address = fix->address + fix->forwards - minipool_pad;

    /* If the minipool starts before the end of FIX->INSN then this FIX
       cannot be placed into the current pool.  Furthermore, adding the
       new constant pool entry may cause the pool to start FIX_SIZE bytes
       earlier.  */
    if (!elements.empty () &&
	(fix->address + get_attr_length (fix->insn)
	 >= elements.front ()->max_address - fix->fix_size))
      return NULL;

    /* If valid, max_mp is the first pool_entry that has a lower
       constraint than the one we are trying to add.  */
    std::list<Mnode *>::iterator max_mpi = elements.end ();
    /* Scan the pool to see if a constant with the same value has
       already been added.  While we are doing this, also note the
       location where we must insert the constant if it doesn't already
       exist.  */
    std::list<Mnode *>::iterator mpi = elements.begin ();
    for (; mpi != elements.end (); mpi++)
      {
	Mnode *mp = *mpi;
	if (GET_CODE (fix->value) == GET_CODE (mp->value)
	    && fix->mode == mp->mode
	    && (!LABEL_P (fix->value)
		|| (CODE_LABEL_NUMBER (fix->value)
		    == CODE_LABEL_NUMBER (mp->value)))
	    && rtx_equal_p (fix->value, mp->value))
	  {
	    /* More than one fix references this entry.  */
	    mp->refcount++;
	    return move_fix_forward_ref (mpi, max_mpi, max_address);
	  }

	/* Note the insertion point if necessary.  */
	if (max_mpi == elements.end ()
	    && mp->max_address > max_address)
	  max_mpi = mpi;

	/* If we are inserting an 8-bytes aligned quantity and
	   we have not already found an insertion point, then
	   make sure that all such 8-byte aligned quantities are
	   placed at the start of the pool.  */
	if (ARM_DOUBLEWORD_ALIGN
	    && max_mpi == elements.end ()
	    && fix->fix_size >= 8
	    && mp->fix_size < 8)
	  {
	    max_mpi = mpi;
	    max_address = mp->max_address;
	  }
      }

    /* The value is not currently in the minipool, so we need to create
       a new entry for it.  If MAX_MP is NULL, the entry will be put on
       the end of the list since the placement is less constrained than
       any existing entry.  Otherwise, we insert the new fix before
       MAX_MP and, if necessary, adjust the constraints on the other
       entries.  */
    Mnode *mp = XNEW (Mnode);
    mp->fix_size = fix->fix_size;
    mp->mode = fix->mode;
    mp->value = fix->value;
    mp->refcount = 1;
    /* Not yet required for a backwards ref.  */
    mp->min_address = -65536;

    if (max_mpi == elements.end ())
      {
	mp->max_address = max_address;
	if (elements.empty ())
	  vector_label = gen_label_rtx ();
	mpi = elements.insert (elements.end (), mp);
      }
    else
      {
	if (max_address > (*max_mpi)->max_address - mp->fix_size)
	  mp->max_address = (*max_mpi)->max_address - mp->fix_size;
	else
	  mp->max_address = max_address;

	mpi = elements.insert (max_mpi, mp);
      }
    Mnode *result = mp;

    /* Scan over the preceding entries and adjust their addresses as
       required.  */
    Mnode *pmp;
    for (; mpi != elements.begin (); mp = pmp, mpi--)
      {
	pmp = *std::prev (mpi);
	if (pmp->max_address <= mp->max_address - pmp->fix_size)
	  break;
	pmp->max_address = mp->max_address - pmp->fix_size;
      }

    return result;
  }

  Mnode *
  minipool::move_fix_backward_ref (std::list<Mnode *>::iterator mpi,
				   std::list<Mnode *>::iterator min_mpi,
				   HOST_WIDE_INT  min_address)
  {
    HOST_WIDE_INT offset;

    /* The code below assumes these are different.  */
    gcc_assert (std::next (mpi) != min_mpi);

    Mnode *result = *mpi;

    if (min_mpi == elements.begin ())
      {
	if (min_address > result->min_address)
	  result->min_address = min_address;
      }
    else
      {
	/* We will adjust this below if it is too loose.  */
	result->min_address = min_address;
	/* Insert before min_mpi.  */
	elements.splice (min_mpi, elements, mpi);
      }

    offset = 0;
    mpi = elements.begin ();
    std::list<Mnode *>::iterator next;
    for (; mpi != elements.end (); mpi = next)
      {
	Mnode *mp = *mpi;
	mp->offset = offset;
	if (mp->refcount > 0)
	  offset += mp->fix_size;

	next = std::next (mpi);
	if (next != elements.end ())
	  {
	    Mnode *nmp = *next;
	    if (nmp->min_address < mp->min_address + mp->fix_size)
	      nmp->min_address = mp->min_address + mp->fix_size;
	  }
      }

    return result;
  }

  /* Add a constant to the minipool for a backward reference.  Returns the
     node added or NULL if the constant will not fit in this pool.

     Note that the code for insertion for a backwards reference can be
     somewhat confusing because the calculated offsets for each fix do
     not take into account the size of the pool (which is still under
     construction.  */
  Mnode *
  minipool::add_backward_ref (Mfix *fix)
  {
    /* If set, min_mp is the last pool_entry that has a lower constraint
       than the one we are trying to add.  */
    /* This can be negative, since it is only a constraint.  */
    HOST_WIDE_INT  min_address = fix->address - fix->backwards;

    /* If we can't reach the current pool from this insn, or if we can't
       insert this entry at the end of the pool without pushing other
       fixes out of range, then we don't try.  This ensures that we
       can't fail later on.  */
    if (min_address >= minipool_barrier->address
	|| (elements.back ()->min_address + fix->fix_size
	    >= minipool_barrier->address))
      return NULL;

    /* Scan the pool to see if a constant with the same value has
       already been added.  While we are doing this, also note the
       location where we must insert the constant if it doesn't already
       exist.  */
    std::list<Mnode *>::reverse_iterator rmpi = elements.rbegin ();
    std::list<Mnode *>::reverse_iterator min_rmpi = elements.rend ();
    for (; rmpi != elements.rend (); rmpi++)
      {
	Mnode *mp = *rmpi;
	if (GET_CODE (fix->value) == GET_CODE (mp->value)
	    && fix->mode == mp->mode
	    && (!LABEL_P (fix->value)
		|| (CODE_LABEL_NUMBER (fix->value)
		    == CODE_LABEL_NUMBER (mp->value)))
	    && rtx_equal_p (fix->value, mp->value)
	    /* Check that there is enough slack to move this entry to the
	       end of the table (this is conservative).  */
	    && (mp->max_address
		> (minipool_barrier->address
		   + elements.back ()->offset
		   + elements.back ()->fix_size)))
	  {
	    mp->refcount++;
	    return move_fix_backward_ref (std::prev (rmpi.base ()),
					  min_rmpi.base (), min_address);
	  }

	if (min_rmpi != elements.rend ())
	  mp->min_address += fix->fix_size;
	else
	  {
	    /* Note the insertion point if necessary.  */
	    if (mp->min_address < min_address)
	      {
		/* For now, we do not allow the insertion of 8-byte alignment
		   requiring nodes anywhere but at the start of the pool.  */
		if (ARM_DOUBLEWORD_ALIGN
		    && fix->fix_size >= 8 && mp->fix_size < 8)
		  return NULL;
		else
		  min_rmpi = rmpi;
	      }
	    else if (mp->max_address
		     < minipool_barrier->address + mp->offset + fix->fix_size)
	      {
		/* Inserting before this entry would push the fix beyond
		   its maximum address (which can happen if we have
		   re-located a forwards fix); force the new fix to come
		   after it.  */
		if (ARM_DOUBLEWORD_ALIGN
		    && fix->fix_size >= 8 && mp->fix_size < 8)
		  return NULL;
		else
		  {
		    min_rmpi = rmpi;
		    min_address = mp->min_address + fix->fix_size;
		  }
	      }
	    /* Do not insert a non-8-byte aligned quantity before 8-byte
	       aligned quantities.  */
	    else if (ARM_DOUBLEWORD_ALIGN
		     && fix->fix_size < 8
		     && mp->fix_size >= 8)
	      {
		min_rmpi = rmpi;
		min_address = mp->min_address + fix->fix_size;
	      }
	  }
      }

    /* We need to create a new entry.  */
    Mnode *mp = XNEW (Mnode);
    mp->fix_size = fix->fix_size;
    mp->mode = fix->mode;
    mp->value = fix->value;
    mp->refcount = 1;
    mp->max_address = minipool_barrier->address + 65536;

    mp->min_address = min_address;

    std::list<Mnode *>::iterator mpi;

    if (min_rmpi == elements.rend ())
      {
	gcc_assert (!elements.empty ());
	mpi = elements.insert (elements.begin (), mp);
      }
    else
      mpi = elements.insert (min_rmpi.base (), mp);

    Mnode *result = mp;

    if (mpi != elements.begin ())
      mp = *--mpi;
    else
      mp->offset = 0;

    /* Scan over the following entries and adjust their offsets.  */
    Mnode *nmp;
    for (mpi++; mpi != elements.end (); mp = nmp, mpi++)
      {
	nmp = *mpi;
	if (nmp->min_address < mp->min_address + mp->fix_size)
	  nmp->min_address = mp->min_address + mp->fix_size;

	if (mp->refcount)
	  nmp->offset = mp->offset + mp->fix_size;
	else
	  nmp->offset = mp->offset;
      }

    return result;
  }

  void
  minipool::assign_offsets (Mfix *barrier)
  {
    HOST_WIDE_INT offset = 0;

    minipool_barrier = barrier;

    std::list<Mnode *>::iterator mpi = elements.begin ();
    for (; mpi != elements.end (); mpi++)
      {
	Mnode *mp = *mpi;
	mp->offset = offset;

	if (mp->refcount > 0)
	  offset += mp->fix_size;
      }
  }

  /* Output the literal table */
  void
  minipool::dump (rtx_insn *scan)
  {
    int align64 = 0;

    std::list<Mnode *>::iterator mpi = elements.begin ();
    if (ARM_DOUBLEWORD_ALIGN)
      for (; mpi != elements.end (); mpi++)
	{
	  Mnode *mp = *mpi;
	  if (mp->refcount > 0 && mp->fix_size >= 8)
	    {
	      align64 = 1;
	      break;
	    }
	}

    if (dump_file)
      fprintf (dump_file,
	       ";; Emitting minipool after insn %u; address %ld; "
	       "align %d (bytes)\n",
	       INSN_UID (scan), (unsigned long) minipool_barrier->address,
	       align64 ? 8 : 4);

    scan = emit_label_after (gen_label_rtx (), scan);
    scan = emit_insn_after (align64 ? gen_align_8 () : gen_align_4 (), scan);
    scan = emit_label_after (vector_label, scan);

    for (mpi = elements.begin (); mpi != elements.end (); mpi++)
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

    reset ();
    scan = emit_insn_after (gen_consttable_end (), scan);
    scan = emit_barrier_after (scan);
  }

  /* Find the best place in the insn stream in the range
     (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
     Create the barrier by inserting a jump and add a new fix entry for
     it.  */
  static Mfix *
  create_fix_barrier (Mfix *fix, HOST_WIDE_INT max_address)
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
    fix->loc = loc;
    fix->mode = mode;
    fix->fix_size = MINIPOOL_FIX_SIZE (mode);
    fix->value = value;
    fix->forwards = get_attr_pool_range (insn);
    fix->backwards = get_attr_neg_pool_range (insn);
    fix->minipool = NULL;

    /* If an insn doesn't have a range defined for it, then it isn't
       expecting to be reworked by this code.  Better to stop now than
       to generate duff assembly code.  */
    gcc_assert (fix->forwards || fix->backwards);

    /* If an entry requires 8-byte alignment then assume all constant pools
       require 4 bytes of padding.  Trying to do this later on a per-pool
       basis is awkward because existing pool entries have to be modified.  */
    if (ARM_DOUBLEWORD_ALIGN && fix->fix_size >= 8)
      minipool_pad = 4;

    if (dump_file)
      {
	fprintf (dump_file,
		 ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
		 GET_MODE_NAME (mode),
		 INSN_UID (insn), (unsigned long) address,
		 -1 * (long)fix->backwards, (long)fix->forwards);
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

		    /* Casting the address of something to a mode narrower
		       than a word can cause avoid_constant_pool_reference()
		       to return the pool reference itself.  That's no good to
		       us here.  Lets just hope that we can use the
		       constant pool value directly.  */
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
     but pc-relative loads on Arm (and especially thumb1) have very
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
    minipool mpool;

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
    minipool_pad = 0;

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
	  address += get_label_padding (insn);
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
		if (ftmp->address >= mpool.head ()->max_address)
		  break;

		last_barrier = ftmp;
	      }
	    else if ((ftmp->minipool = mpool.add_forward_ref (ftmp)) == NULL)
	      break;

	    last_added_fix = ftmp;  /* Keep track of the last fix added.  */
	  }

	/* If we found a barrier, drop back to that; any fixes that we
	   could have reached but come after the barrier will now go in
	   the next mini-pool.  */
	if (last_barrier != NULL)
	  {
	    /* Reduce the refcount for those fixes that won't go into this
	       pool after all.  */
	    for (fdel = last_barrier->next;
		 fdel && fdel != ftmp;
		 fdel = fdel->next)
	      {
		fdel->minipool->refcount--;
		fdel->minipool = NULL;
	      }

	    ftmp = last_barrier;
	  }
	else
	  {
	    /* ftmp is first fix that we can't fit into this pool and
	       there no natural barriers that we could use.  Insert a
	       new barrier in the code somewhere between the previous
	       fix and this one, and arrange to jump around it.  */
	    HOST_WIDE_INT max_address;

	    /* The last item on the list of fixes must be a barrier, so
	       we can never run off the end of the list of fixes without
	       last_barrier being set.  */
	    gcc_assert (ftmp);

	    max_address = mpool.head()->max_address;
	    /* Check that there isn't another fix that is in range that
	       we couldn't fit into this pool because the pool was
	       already too large: we need to put the pool before such an
	       instruction.  The pool itself may come just after the
	       fix because create_fix_barrier also allows space for a
	       jump instruction.  */
	    if (ftmp->address < max_address)
	      max_address = ftmp->address + 1;

	    last_barrier = create_fix_barrier (last_added_fix, max_address);
	  }

	mpool.assign_offsets (last_barrier);

	while (ftmp)
	  {
	    if (!BARRIER_P (ftmp->insn)
		&& ((ftmp->minipool = mpool.add_backward_ref (ftmp)) == NULL))
	      break;

	    ftmp = ftmp->next;
	  }

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
	      *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
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
