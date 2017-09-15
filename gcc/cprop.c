/* Global constant/copy propagation for RTL.
   Copyright (C) 1997-2017 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "cfghooks.h"
#include "df.h"
#include "insn-config.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgcleanup.h"
#include "params.h"
#include "cselib.h"
#include "intl.h"
#include "tree-pass.h"
#include "dbgcnt.h"
#include "cfgloop.h"
#include "gcse.h"


/* An obstack for our working variables.  */
static struct obstack cprop_obstack;

/* Occurrence of an expression.
   There is one per basic block.  If a pattern appears more than once the
   last appearance is used.  */

struct cprop_occr
{
  /* Next occurrence of this expression.  */
  struct cprop_occr *next;
  /* The insn that computes the expression.  */
  rtx_insn *insn;
};

/* Hash table entry for assignment expressions.  */

struct cprop_expr
{
  /* The expression (DEST := SRC).  */
  rtx dest;
  rtx src;

  /* Index in the available expression bitmaps.  */
  int bitmap_index;
  /* Next entry with the same hash.  */
  struct cprop_expr *next_same_hash;
  /* List of available occurrence in basic blocks in the function.
     An "available occurrence" is one that is the last occurrence in the
     basic block and whose operands are not modified by following statements
     in the basic block [including this insn].  */
  struct cprop_occr *avail_occr;
};

/* Hash table for copy propagation expressions.
   Each hash table is an array of buckets.
   ??? It is known that if it were an array of entries, structure elements
   `next_same_hash' and `bitmap_index' wouldn't be necessary.  However, it is
   not clear whether in the final analysis a sufficient amount of memory would
   be saved as the size of the available expression bitmaps would be larger
   [one could build a mapping table without holes afterwards though].
   Someday I'll perform the computation and figure it out.  */

struct hash_table_d
{
  /* The table itself.
     This is an array of `set_hash_table_size' elements.  */
  struct cprop_expr **table;

  /* Size of the hash table, in elements.  */
  unsigned int size;

  /* Number of hash table elements.  */
  unsigned int n_elems;
};

/* Copy propagation hash table.  */
static struct hash_table_d set_hash_table;

/* Array of implicit set patterns indexed by basic block index.  */
static rtx *implicit_sets;

/* Array of indexes of expressions for implicit set patterns indexed by basic
   block index.  In other words, implicit_set_indexes[i] is the bitmap_index
   of the expression whose RTX is implicit_sets[i].  */
static int *implicit_set_indexes;

/* Bitmap containing one bit for each register in the program.
   Used when performing GCSE to track which registers have been set since
   the start or end of the basic block while traversing that block.  */
static regset reg_set_bitmap;

/* Various variables for statistics gathering.  */

/* Memory used in a pass.
   This isn't intended to be absolutely precise.  Its intent is only
   to keep an eye on memory usage.  */
static int bytes_used;

/* Number of local constants propagated.  */
static int local_const_prop_count;
/* Number of local copies propagated.  */
static int local_copy_prop_count;
/* Number of global constants propagated.  */
static int global_const_prop_count;
/* Number of global copies propagated.  */
static int global_copy_prop_count;

#define GOBNEW(T)		((T *) cprop_alloc (sizeof (T)))
#define GOBNEWVAR(T, S)		((T *) cprop_alloc ((S)))

/* Cover function to obstack_alloc.  */

static void *
cprop_alloc (unsigned long size)
{
  bytes_used += size;
  return obstack_alloc (&cprop_obstack, size);
}

/* Return nonzero if register X is unchanged from INSN to the end
   of INSN's basic block.  */

static int
reg_available_p (const_rtx x, const rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return ! REGNO_REG_SET_P (reg_set_bitmap, REGNO (x));
}

/* Hash a set of register REGNO.

   Sets are hashed on the register that is set.  This simplifies the PRE copy
   propagation code.

   ??? May need to make things more elaborate.  Later, as necessary.  */

static unsigned int
hash_mod (int regno, int hash_table_size)
{
  return (unsigned) regno % hash_table_size;
}

/* Insert assignment DEST:=SET from INSN in the hash table.
   DEST is a register and SET is a register or a suitable constant.
   If the assignment is already present in the table, record it as
   the last occurrence in INSN's basic block.
   IMPLICIT is true if it's an implicit set, false otherwise.  */

static void
insert_set_in_table (rtx dest, rtx src, rtx_insn *insn,
		     struct hash_table_d *table, bool implicit)
{
  bool found = false;
  unsigned int hash;
  struct cprop_expr *cur_expr, *last_expr = NULL;
  struct cprop_occr *cur_occr;

  hash = hash_mod (REGNO (dest), table->size);

  for (cur_expr = table->table[hash]; cur_expr;
       cur_expr = cur_expr->next_same_hash)
    {
      if (dest == cur_expr->dest
	  && src == cur_expr->src)
	{
	  found = true;
	  break;
	}
      last_expr = cur_expr;
    }

  if (! found)
    {
      cur_expr = GOBNEW (struct cprop_expr);
      bytes_used += sizeof (struct cprop_expr);
      if (table->table[hash] == NULL)
	/* This is the first pattern that hashed to this index.  */
	table->table[hash] = cur_expr;
      else
	/* Add EXPR to end of this hash chain.  */
	last_expr->next_same_hash = cur_expr;

      /* Set the fields of the expr element.
	 We must copy X because it can be modified when copy propagation is
	 performed on its operands.  */
      cur_expr->dest = copy_rtx (dest);
      cur_expr->src = copy_rtx (src);
      cur_expr->bitmap_index = table->n_elems++;
      cur_expr->next_same_hash = NULL;
      cur_expr->avail_occr = NULL;
    }

  /* Now record the occurrence.  */
  cur_occr = cur_expr->avail_occr;

  if (cur_occr
      && BLOCK_FOR_INSN (cur_occr->insn) == BLOCK_FOR_INSN (insn))
    {
      /* Found another instance of the expression in the same basic block.
	 Prefer this occurrence to the currently recorded one.  We want
	 the last one in the block and the block is scanned from start
	 to end.  */
      cur_occr->insn = insn;
    }
  else
    {
      /* First occurrence of this expression in this basic block.  */
      cur_occr = GOBNEW (struct cprop_occr);
      bytes_used += sizeof (struct cprop_occr);
      cur_occr->insn = insn;
      cur_occr->next = cur_expr->avail_occr;
      cur_expr->avail_occr = cur_occr;
    }

  /* Record bitmap_index of the implicit set in implicit_set_indexes.  */
  if (implicit)
    implicit_set_indexes[BLOCK_FOR_INSN (insn)->index]
      = cur_expr->bitmap_index;
}

/* Determine whether the rtx X should be treated as a constant for CPROP.
   Since X might be inserted more than once we have to take care that it
   is sharable.  */

static bool
cprop_constant_p (const_rtx x)
{
  return CONSTANT_P (x) && (GET_CODE (x) != CONST || shared_const_p (x));
}

/* Determine whether the rtx X should be treated as a register that can
   be propagated.  Any pseudo-register is fine.  */

static bool
cprop_reg_p (const_rtx x)
{
  return REG_P (x) && !HARD_REGISTER_P (x);
}

/* Scan SET present in INSN and add an entry to the hash TABLE.
   IMPLICIT is true if it's an implicit set, false otherwise.  */

static void
hash_scan_set (rtx set, rtx_insn *insn, struct hash_table_d *table,
	       bool implicit)
{
  rtx src = SET_SRC (set);
  rtx dest = SET_DEST (set);

  if (cprop_reg_p (dest)
      && reg_available_p (dest, insn)
      && can_copy_p (GET_MODE (dest)))
    {
      /* See if a REG_EQUAL note shows this equivalent to a simpler expression.

	 This allows us to do a single CPROP pass and still eliminate
	 redundant constants, addresses or other expressions that are
	 constructed with multiple instructions.

	 However, keep the original SRC if INSN is a simple reg-reg move.  In
	 In this case, there will almost always be a REG_EQUAL note on the
	 insn that sets SRC.  By recording the REG_EQUAL value here as SRC
	 for INSN, we miss copy propagation opportunities.

	 Note that this does not impede profitable constant propagations.  We
	 "look through" reg-reg sets in lookup_set.  */
      rtx note = find_reg_equal_equiv_note (insn);
      if (note != 0
	  && REG_NOTE_KIND (note) == REG_EQUAL
	  && !REG_P (src)
	  && cprop_constant_p (XEXP (note, 0)))
	src = XEXP (note, 0), set = gen_rtx_SET (dest, src);

      /* Record sets for constant/copy propagation.  */
      if ((cprop_reg_p (src)
	   && src != dest
	   && reg_available_p (src, insn))
	  || cprop_constant_p (src))
	insert_set_in_table (dest, src, insn, table, implicit);
    }
}

/* Process INSN and add hash table entries as appropriate.  */

static void
hash_scan_insn (rtx_insn *insn, struct hash_table_d *table)
{
  rtx pat = PATTERN (insn);
  int i;

  /* Pick out the sets of INSN and for other forms of instructions record
     what's been modified.  */

  if (GET_CODE (pat) == SET)
    hash_scan_set (pat, insn, table, false);
  else if (GET_CODE (pat) == PARALLEL)
    for (i = 0; i < XVECLEN (pat, 0); i++)
      {
	rtx x = XVECEXP (pat, 0, i);

	if (GET_CODE (x) == SET)
	  hash_scan_set (x, insn, table, false);
      }
}

/* Dump the hash table TABLE to file FILE under the name NAME.  */

static void
dump_hash_table (FILE *file, const char *name, struct hash_table_d *table)
{
  int i;
  /* Flattened out table, so it's printed in proper order.  */
  struct cprop_expr **flat_table;
  unsigned int *hash_val;
  struct cprop_expr *expr;

  flat_table = XCNEWVEC (struct cprop_expr *, table->n_elems);
  hash_val = XNEWVEC (unsigned int, table->n_elems);

  for (i = 0; i < (int) table->size; i++)
    for (expr = table->table[i]; expr != NULL; expr = expr->next_same_hash)
      {
	flat_table[expr->bitmap_index] = expr;
	hash_val[expr->bitmap_index] = i;
      }

  fprintf (file, "%s hash table (%d buckets, %d entries)\n",
	   name, table->size, table->n_elems);

  for (i = 0; i < (int) table->n_elems; i++)
    if (flat_table[i] != 0)
      {
	expr = flat_table[i];
	fprintf (file, "Index %d (hash value %d)\n  ",
		 expr->bitmap_index, hash_val[i]);
	print_rtl (file, expr->dest);
	fprintf (file, " := ");
	print_rtl (file, expr->src);
	fprintf (file, "\n");
      }

  fprintf (file, "\n");

  free (flat_table);
  free (hash_val);
}

/* Record as unavailable all registers that are DEF operands of INSN.  */

static void
make_set_regs_unavailable (rtx_insn *insn)
{
  df_ref def;

  FOR_EACH_INSN_DEF (def, insn)
    SET_REGNO_REG_SET (reg_set_bitmap, DF_REF_REGNO (def));
}

/* Top level function to create an assignment hash table.

   Assignment entries are placed in the hash table if
   - they are of the form (set (pseudo-reg) src),
   - src is something we want to perform const/copy propagation on,
   - none of the operands or target are subsequently modified in the block

   Currently src must be a pseudo-reg or a const_int.

   TABLE is the table computed.  */

static void
compute_hash_table_work (struct hash_table_d *table)
{
  basic_block bb;

  /* Allocate vars to track sets of regs.  */
  reg_set_bitmap = ALLOC_REG_SET (NULL);

  FOR_EACH_BB_FN (bb, cfun)
    {
      rtx_insn *insn;

      /* Reset tables used to keep track of what's not yet invalid [since
	 the end of the block].  */
      CLEAR_REG_SET (reg_set_bitmap);

      /* Go over all insns from the last to the first.  This is convenient
	 for tracking available registers, i.e. not set between INSN and
	 the end of the basic block BB.  */
      FOR_BB_INSNS_REVERSE (bb, insn)
        {
	  /* Only real insns are interesting.  */
	  if (!NONDEBUG_INSN_P (insn))
	    continue;

	  /* Record interesting sets from INSN in the hash table.  */
	  hash_scan_insn (insn, table);

	  /* Any registers set in INSN will make SETs above it not AVAIL.  */
	  make_set_regs_unavailable (insn);
	}

      /* Insert implicit sets in the hash table, pretending they appear as
	 insns at the head of the basic block.  */
      if (implicit_sets[bb->index] != NULL_RTX)
	hash_scan_set (implicit_sets[bb->index], BB_HEAD (bb), table, true);
    }

  FREE_REG_SET (reg_set_bitmap);
}

/* Allocate space for the set/expr hash TABLE.
   It is used to determine the number of buckets to use.  */

static void
alloc_hash_table (struct hash_table_d *table)
{
  int n;

  n = get_max_insn_count ();

  table->size = n / 4;
  if (table->size < 11)
    table->size = 11;

  /* Attempt to maintain efficient use of hash table.
     Making it an odd number is simplest for now.
     ??? Later take some measurements.  */
  table->size |= 1;
  n = table->size * sizeof (struct cprop_expr *);
  table->table = XNEWVAR (struct cprop_expr *, n);
}

/* Free things allocated by alloc_hash_table.  */

static void
free_hash_table (struct hash_table_d *table)
{
  free (table->table);
}

/* Compute the hash TABLE for doing copy/const propagation or
   expression hash table.  */

static void
compute_hash_table (struct hash_table_d *table)
{
  /* Initialize count of number of entries in hash table.  */
  table->n_elems = 0;
  memset (table->table, 0, table->size * sizeof (struct cprop_expr *));

  compute_hash_table_work (table);
}

/* Expression tracking support.  */

/* Lookup REGNO in the set TABLE.  The result is a pointer to the
   table entry, or NULL if not found.  */

static struct cprop_expr *
lookup_set (unsigned int regno, struct hash_table_d *table)
{
  unsigned int hash = hash_mod (regno, table->size);
  struct cprop_expr *expr;

  expr = table->table[hash];

  while (expr && REGNO (expr->dest) != regno)
    expr = expr->next_same_hash;

  return expr;
}

/* Return the next entry for REGNO in list EXPR.  */

static struct cprop_expr *
next_set (unsigned int regno, struct cprop_expr *expr)
{
  do
    expr = expr->next_same_hash;
  while (expr && REGNO (expr->dest) != regno);

  return expr;
}

/* Reset tables used to keep track of what's still available [since the
   start of the block].  */

static void
reset_opr_set_tables (void)
{
  /* Maintain a bitmap of which regs have been set since beginning of
     the block.  */
  CLEAR_REG_SET (reg_set_bitmap);
}

/* Return nonzero if the register X has not been set yet [since the
   start of the basic block containing INSN].  */

static int
reg_not_set_p (const_rtx x, const rtx_insn *insn ATTRIBUTE_UNUSED)
{
  return ! REGNO_REG_SET_P (reg_set_bitmap, REGNO (x));
}

/* Record things set by INSN.
   This data is used by reg_not_set_p.  */

static void
mark_oprs_set (rtx_insn *insn)
{
  df_ref def;

  FOR_EACH_INSN_DEF (def, insn)
    SET_REGNO_REG_SET (reg_set_bitmap, DF_REF_REGNO (def));
}

/* Compute copy/constant propagation working variables.  */

/* Local properties of assignments.  */
static sbitmap *cprop_avloc;
static sbitmap *cprop_kill;

/* Global properties of assignments (computed from the local properties).  */
static sbitmap *cprop_avin;
static sbitmap *cprop_avout;

/* Allocate vars used for copy/const propagation.  N_BLOCKS is the number of
   basic blocks.  N_SETS is the number of sets.  */

static void
alloc_cprop_mem (int n_blocks, int n_sets)
{
  cprop_avloc = sbitmap_vector_alloc (n_blocks, n_sets);
  cprop_kill = sbitmap_vector_alloc (n_blocks, n_sets);

  cprop_avin = sbitmap_vector_alloc (n_blocks, n_sets);
  cprop_avout = sbitmap_vector_alloc (n_blocks, n_sets);
}

/* Free vars used by copy/const propagation.  */

static void
free_cprop_mem (void)
{
  sbitmap_vector_free (cprop_avloc);
  sbitmap_vector_free (cprop_kill);
  sbitmap_vector_free (cprop_avin);
  sbitmap_vector_free (cprop_avout);
}

/* Compute the local properties of each recorded expression.

   Local properties are those that are defined by the block, irrespective of
   other blocks.

   An expression is killed in a block if its operands, either DEST or SRC, are
   modified in the block.

   An expression is computed (locally available) in a block if it is computed
   at least once and expression would contain the same value if the
   computation was moved to the end of the block.

   KILL and COMP are destination sbitmaps for recording local properties.  */

static void
compute_local_properties (sbitmap *kill, sbitmap *comp,
			  struct hash_table_d *table)
{
  unsigned int i;

  /* Initialize the bitmaps that were passed in.  */
  bitmap_vector_clear (kill, last_basic_block_for_fn (cfun));
  bitmap_vector_clear (comp, last_basic_block_for_fn (cfun));

  for (i = 0; i < table->size; i++)
    {
      struct cprop_expr *expr;

      for (expr = table->table[i]; expr != NULL; expr = expr->next_same_hash)
	{
	  int indx = expr->bitmap_index;
	  df_ref def;
	  struct cprop_occr *occr;

	  /* For each definition of the destination pseudo-reg, the expression
	     is killed in the block where the definition is.  */
	  for (def = DF_REG_DEF_CHAIN (REGNO (expr->dest));
	       def; def = DF_REF_NEXT_REG (def))
	    bitmap_set_bit (kill[DF_REF_BB (def)->index], indx);

	  /* If the source is a pseudo-reg, for each definition of the source,
	     the expression is killed in the block where the definition is.  */
	  if (REG_P (expr->src))
	    for (def = DF_REG_DEF_CHAIN (REGNO (expr->src));
		 def; def = DF_REF_NEXT_REG (def))
	      bitmap_set_bit (kill[DF_REF_BB (def)->index], indx);

	  /* The occurrences recorded in avail_occr are exactly those that
	     are locally available in the block where they are.  */
	  for (occr = expr->avail_occr; occr != NULL; occr = occr->next)
	    {
	      bitmap_set_bit (comp[BLOCK_FOR_INSN (occr->insn)->index], indx);
	    }
	}
    }
}

/* Hash table support.  */

/* Top level routine to do the dataflow analysis needed by copy/const
   propagation.  */

static void
compute_cprop_data (void)
{
  basic_block bb;

  compute_local_properties (cprop_kill, cprop_avloc, &set_hash_table);
  compute_available (cprop_avloc, cprop_kill, cprop_avout, cprop_avin);

  /* Merge implicit sets into CPROP_AVIN.  They are always available at the
     entry of their basic block.  We need to do this because 1) implicit sets
     aren't recorded for the local pass so they cannot be propagated within
     their basic block by this pass and 2) the global pass would otherwise
     propagate them only in the successors of their basic block.  */
  FOR_EACH_BB_FN (bb, cfun)
    {
      int index = implicit_set_indexes[bb->index];
      if (index != -1)
	bitmap_set_bit (cprop_avin[bb->index], index);
    }
}

/* Copy/constant propagation.  */

/* Maximum number of register uses in an insn that we handle.  */
#define MAX_USES 8

/* Table of uses (registers, both hard and pseudo) found in an insn.
   Allocated statically to avoid alloc/free complexity and overhead.  */
static rtx reg_use_table[MAX_USES];

/* Index into `reg_use_table' while building it.  */
static unsigned reg_use_count;

/* Set up a list of register numbers used in INSN.  The found uses are stored
   in `reg_use_table'.  `reg_use_count' is initialized to zero before entry,
   and contains the number of uses in the table upon exit.

   ??? If a register appears multiple times we will record it multiple times.
   This doesn't hurt anything but it will slow things down.  */

static void
find_used_regs (rtx *xptr, void *data ATTRIBUTE_UNUSED)
{
  int i, j;
  enum rtx_code code;
  const char *fmt;
  rtx x = *xptr;

  /* repeat is used to turn tail-recursion into iteration since GCC
     can't do it when there's no return value.  */
 repeat:
  if (x == 0)
    return;

  code = GET_CODE (x);
  if (REG_P (x))
    {
      if (reg_use_count == MAX_USES)
	return;

      reg_use_table[reg_use_count] = x;
      reg_use_count++;
    }

  /* Recursively scan the operands of this expression.  */

  for (i = GET_RTX_LENGTH (code) - 1, fmt = GET_RTX_FORMAT (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  /* If we are about to do the last recursive call
	     needed at this level, change it into iteration.
	     This function is called enough to be worth it.  */
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }

	  find_used_regs (&XEXP (x, i), data);
	}
      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  find_used_regs (&XVECEXP (x, i, j), data);
    }
}

/* Try to replace all uses of FROM in INSN with TO.
   Return nonzero if successful.  */

static int
try_replace_reg (rtx from, rtx to, rtx_insn *insn)
{
  rtx note = find_reg_equal_equiv_note (insn);
  rtx src = 0;
  int success = 0;
  rtx set = single_set (insn);

  bool check_rtx_costs = true;
  bool speed = optimize_bb_for_speed_p (BLOCK_FOR_INSN (insn));
  int old_cost = set ? set_rtx_cost (set, speed) : 0;

  if (!set
      || CONSTANT_P (SET_SRC (set))
      || (note != 0
	  && REG_NOTE_KIND (note) == REG_EQUAL
	  && (GET_CODE (XEXP (note, 0)) == CONST
	      || CONSTANT_P (XEXP (note, 0)))))
    check_rtx_costs = false;

  /* Usually we substitute easy stuff, so we won't copy everything.
     We however need to take care to not duplicate non-trivial CONST
     expressions.  */
  to = copy_rtx (to);

  validate_replace_src_group (from, to, insn);

  /* If TO is a constant, check the cost of the set after propagation
     to the cost of the set before the propagation.  If the cost is
     higher, then do not replace FROM with TO.  */

  if (check_rtx_costs
      && CONSTANT_P (to)
      && set_rtx_cost (set, speed) > old_cost)
    {
      cancel_changes (0);
      return false;
    }


  if (num_changes_pending () && apply_change_group ())
    success = 1;

  /* Try to simplify SET_SRC if we have substituted a constant.  */
  if (success && set && CONSTANT_P (to))
    {
      src = simplify_rtx (SET_SRC (set));

      if (src)
	validate_change (insn, &SET_SRC (set), src, 0);
    }

  /* If there is already a REG_EQUAL note, update the expression in it
     with our replacement.  */
  if (note != 0 && REG_NOTE_KIND (note) == REG_EQUAL)
    set_unique_reg_note (insn, REG_EQUAL,
			 simplify_replace_rtx (XEXP (note, 0), from, to));
  if (!success && set && reg_mentioned_p (from, SET_SRC (set)))
    {
      /* If above failed and this is a single set, try to simplify the source
	 of the set given our substitution.  We could perhaps try this for
	 multiple SETs, but it probably won't buy us anything.  */
      src = simplify_replace_rtx (SET_SRC (set), from, to);

      if (!rtx_equal_p (src, SET_SRC (set))
	  && validate_change (insn, &SET_SRC (set), src, 0))
	success = 1;

      /* If we've failed perform the replacement, have a single SET to
	 a REG destination and don't yet have a note, add a REG_EQUAL note
	 to not lose information.  */
      if (!success && note == 0 && set != 0 && REG_P (SET_DEST (set)))
	note = set_unique_reg_note (insn, REG_EQUAL, copy_rtx (src));
    }

  if (set && MEM_P (SET_DEST (set)) && reg_mentioned_p (from, SET_DEST (set)))
    {
      /* Registers can also appear as uses in SET_DEST if it is a MEM.
         We could perhaps try this for multiple SETs, but it probably
         won't buy us anything.  */
      rtx dest = simplify_replace_rtx (SET_DEST (set), from, to);

      if (!rtx_equal_p (dest, SET_DEST (set))
          && validate_change (insn, &SET_DEST (set), dest, 0))
        success = 1;
    }

  /* REG_EQUAL may get simplified into register.
     We don't allow that. Remove that note. This code ought
     not to happen, because previous code ought to synthesize
     reg-reg move, but be on the safe side.  */
  if (note && REG_NOTE_KIND (note) == REG_EQUAL && REG_P (XEXP (note, 0)))
    remove_note (insn, note);

  return success;
}

/* Find a set of REGNOs that are available on entry to INSN's block.  If found,
   SET_RET[0] will be assigned a set with a register source and SET_RET[1] a
   set with a constant source.  If not found the corresponding entry is set to
   NULL.  */

static void
find_avail_set (int regno, rtx_insn *insn, struct cprop_expr *set_ret[2])
{
  set_ret[0] = set_ret[1] = NULL;

  /* Loops are not possible here.  To get a loop we would need two sets
     available at the start of the block containing INSN.  i.e. we would
     need two sets like this available at the start of the block:

       (set (reg X) (reg Y))
       (set (reg Y) (reg X))

     This can not happen since the set of (reg Y) would have killed the
     set of (reg X) making it unavailable at the start of this block.  */
  while (1)
    {
      rtx src;
      struct cprop_expr *set = lookup_set (regno, &set_hash_table);

      /* Find a set that is available at the start of the block
	 which contains INSN.  */
      while (set)
	{
	  if (bitmap_bit_p (cprop_avin[BLOCK_FOR_INSN (insn)->index],
			set->bitmap_index))
	    break;
	  set = next_set (regno, set);
	}

      /* If no available set was found we've reached the end of the
	 (possibly empty) copy chain.  */
      if (set == 0)
	break;

      src = set->src;

      /* We know the set is available.
	 Now check that SRC is locally anticipatable (i.e. none of the
	 source operands have changed since the start of the block).

         If the source operand changed, we may still use it for the next
         iteration of this loop, but we may not use it for substitutions.  */

      if (cprop_constant_p (src))
	set_ret[1] = set;
      else if (reg_not_set_p (src, insn))
	set_ret[0] = set;

      /* If the source of the set is anything except a register, then
	 we have reached the end of the copy chain.  */
      if (! REG_P (src))
	break;

      /* Follow the copy chain, i.e. start another iteration of the loop
	 and see if we have an available copy into SRC.  */
      regno = REGNO (src);
    }
}

/* Subroutine of cprop_insn that tries to propagate constants into
   JUMP_INSNS.  JUMP must be a conditional jump.  If SETCC is non-NULL
   it is the instruction that immediately precedes JUMP, and must be a
   single SET of a register.  FROM is what we will try to replace,
   SRC is the constant we will try to substitute for it.  Return nonzero
   if a change was made.  */

static int
cprop_jump (basic_block bb, rtx_insn *setcc, rtx_insn *jump, rtx from, rtx src)
{
  rtx new_rtx, set_src, note_src;
  rtx set = pc_set (jump);
  rtx note = find_reg_equal_equiv_note (jump);

  if (note)
    {
      note_src = XEXP (note, 0);
      if (GET_CODE (note_src) == EXPR_LIST)
	note_src = NULL_RTX;
    }
  else note_src = NULL_RTX;

  /* Prefer REG_EQUAL notes except those containing EXPR_LISTs.  */
  set_src = note_src ? note_src : SET_SRC (set);

  /* First substitute the SETCC condition into the JUMP instruction,
     then substitute that given values into this expanded JUMP.  */
  if (setcc != NULL_RTX
      && !modified_between_p (from, setcc, jump)
      && !modified_between_p (src, setcc, jump))
    {
      rtx setcc_src;
      rtx setcc_set = single_set (setcc);
      rtx setcc_note = find_reg_equal_equiv_note (setcc);
      setcc_src = (setcc_note && GET_CODE (XEXP (setcc_note, 0)) != EXPR_LIST)
		? XEXP (setcc_note, 0) : SET_SRC (setcc_set);
      set_src = simplify_replace_rtx (set_src, SET_DEST (setcc_set),
				      setcc_src);
    }
  else
    setcc = NULL;

  new_rtx = simplify_replace_rtx (set_src, from, src);

  /* If no simplification can be made, then try the next register.  */
  if (rtx_equal_p (new_rtx, SET_SRC (set)))
    return 0;

  /* If this is now a no-op delete it, otherwise this must be a valid insn.  */
  if (new_rtx == pc_rtx)
    delete_insn (jump);
  else
    {
      /* Ensure the value computed inside the jump insn to be equivalent
         to one computed by setcc.  */
      if (setcc && modified_in_p (new_rtx, setcc))
	return 0;
      if (! validate_unshare_change (jump, &SET_SRC (set), new_rtx, 0))
	{
	  /* When (some) constants are not valid in a comparison, and there
	     are two registers to be replaced by constants before the entire
	     comparison can be folded into a constant, we need to keep
	     intermediate information in REG_EQUAL notes.  For targets with
	     separate compare insns, such notes are added by try_replace_reg.
	     When we have a combined compare-and-branch instruction, however,
	     we need to attach a note to the branch itself to make this
	     optimization work.  */

	  if (!rtx_equal_p (new_rtx, note_src))
	    set_unique_reg_note (jump, REG_EQUAL, copy_rtx (new_rtx));
	  return 0;
	}

      /* Remove REG_EQUAL note after simplification.  */
      if (note_src)
	remove_note (jump, note);
     }

  /* Delete the cc0 setter.  */
  if (HAVE_cc0 && setcc != NULL && CC0_P (SET_DEST (single_set (setcc))))
    delete_insn (setcc);

  global_const_prop_count++;
  if (dump_file != NULL)
    {
      fprintf (dump_file,
	       "GLOBAL CONST-PROP: Replacing reg %d in jump_insn %d with "
	       "constant ", REGNO (from), INSN_UID (jump));
      print_rtl (dump_file, src);
      fprintf (dump_file, "\n");
    }
  purge_dead_edges (bb);

  /* If a conditional jump has been changed into unconditional jump, remove
     the jump and make the edge fallthru - this is always called in
     cfglayout mode.  */
  if (new_rtx != pc_rtx && simplejump_p (jump))
    {
      edge e;
      edge_iterator ei;

      FOR_EACH_EDGE (e, ei, bb->succs)
	if (e->dest != EXIT_BLOCK_PTR_FOR_FN (cfun)
	    && BB_HEAD (e->dest) == JUMP_LABEL (jump))
	  {
	    e->flags |= EDGE_FALLTHRU;
	    break;
	  }
      delete_insn (jump);
    }

  return 1;
}

/* Subroutine of cprop_insn that tries to propagate constants.  FROM is what
   we will try to replace, SRC is the constant we will try to substitute for
   it and INSN is the instruction where this will be happening.  */

static int
constprop_register (rtx from, rtx src, rtx_insn *insn)
{
  rtx sset;

  /* Check for reg or cc0 setting instructions followed by
     conditional branch instructions first.  */
  if ((sset = single_set (insn)) != NULL
      && NEXT_INSN (insn)
      && any_condjump_p (NEXT_INSN (insn)) && onlyjump_p (NEXT_INSN (insn)))
    {
      rtx dest = SET_DEST (sset);
      if ((REG_P (dest) || CC0_P (dest))
	  && cprop_jump (BLOCK_FOR_INSN (insn), insn, NEXT_INSN (insn),
			 from, src))
	return 1;
    }

  /* Handle normal insns next.  */
  if (NONJUMP_INSN_P (insn) && try_replace_reg (from, src, insn))
    return 1;

  /* Try to propagate a CONST_INT into a conditional jump.
     We're pretty specific about what we will handle in this
     code, we can extend this as necessary over time.

     Right now the insn in question must look like
     (set (pc) (if_then_else ...))  */
  else if (any_condjump_p (insn) && onlyjump_p (insn))
    return cprop_jump (BLOCK_FOR_INSN (insn), NULL, insn, from, src);
  return 0;
}

/* Perform constant and copy propagation on INSN.
   Return nonzero if a change was made.  */

static int
cprop_insn (rtx_insn *insn)
{
  unsigned i;
  int changed = 0, changed_this_round;
  rtx note;

  do
    {
      changed_this_round = 0;
      reg_use_count = 0;
      note_uses (&PATTERN (insn), find_used_regs, NULL);

      /* We may win even when propagating constants into notes.  */
      note = find_reg_equal_equiv_note (insn);
      if (note)
	find_used_regs (&XEXP (note, 0), NULL);

      for (i = 0; i < reg_use_count; i++)
	{
	  rtx reg_used = reg_use_table[i];
	  unsigned int regno = REGNO (reg_used);
	  rtx src_cst = NULL, src_reg = NULL;
	  struct cprop_expr *set[2];

	  /* If the register has already been set in this block, there's
	     nothing we can do.  */
	  if (! reg_not_set_p (reg_used, insn))
	    continue;

	  /* Find an assignment that sets reg_used and is available
	     at the start of the block.  */
	  find_avail_set (regno, insn, set);
	  if (set[0])
	    src_reg = set[0]->src;
	  if (set[1])
	    src_cst = set[1]->src;

	  /* Constant propagation.  */
	  if (src_cst && cprop_constant_p (src_cst)
	      && constprop_register (reg_used, src_cst, insn))
	    {
	      changed_this_round = changed = 1;
	      global_const_prop_count++;
	      if (dump_file != NULL)
		{
		  fprintf (dump_file,
			   "GLOBAL CONST-PROP: Replacing reg %d in ", regno);
		  fprintf (dump_file, "insn %d with constant ",
			   INSN_UID (insn));
		  print_rtl (dump_file, src_cst);
		  fprintf (dump_file, "\n");
		}
	      if (insn->deleted ())
		return 1;
	    }
	  /* Copy propagation.  */
	  else if (src_reg && cprop_reg_p (src_reg)
		   && REGNO (src_reg) != regno
		   && try_replace_reg (reg_used, src_reg, insn))
	    {
	      changed_this_round = changed = 1;
	      global_copy_prop_count++;
	      if (dump_file != NULL)
		{
		  fprintf (dump_file,
			   "GLOBAL COPY-PROP: Replacing reg %d in insn %d",
			   regno, INSN_UID (insn));
		  fprintf (dump_file, " with reg %d\n", REGNO (src_reg));
		}

	      /* The original insn setting reg_used may or may not now be
		 deletable.  We leave the deletion to DCE.  */
	      /* FIXME: If it turns out that the insn isn't deletable,
		 then we may have unnecessarily extended register lifetimes
		 and made things worse.  */
	    }
	}
    }
  /* If try_replace_reg simplified the insn, the regs found by find_used_regs
     may not be valid anymore.  Start over.  */
  while (changed_this_round);

  if (changed && DEBUG_INSN_P (insn))
    return 0;

  return changed;
}

/* Like find_used_regs, but avoid recording uses that appear in
   input-output contexts such as zero_extract or pre_dec.  This
   restricts the cases we consider to those for which local cprop
   can legitimately make replacements.  */

static void
local_cprop_find_used_regs (rtx *xptr, void *data)
{
  rtx x = *xptr;

  if (x == 0)
    return;

  switch (GET_CODE (x))
    {
    case ZERO_EXTRACT:
    case SIGN_EXTRACT:
    case STRICT_LOW_PART:
      return;

    case PRE_DEC:
    case PRE_INC:
    case POST_DEC:
    case POST_INC:
    case PRE_MODIFY:
    case POST_MODIFY:
      /* Can only legitimately appear this early in the context of
	 stack pushes for function arguments, but handle all of the
	 codes nonetheless.  */
      return;

    case SUBREG:
      if (read_modify_subreg_p (x))
	return;
      break;

    default:
      break;
    }

  find_used_regs (xptr, data);
}

/* Try to perform local const/copy propagation on X in INSN.  */

static bool
do_local_cprop (rtx x, rtx_insn *insn)
{
  rtx newreg = NULL, newcnst = NULL;

  /* Rule out USE instructions and ASM statements as we don't want to
     change the hard registers mentioned.  */
  if (REG_P (x)
      && (cprop_reg_p (x)
          || (GET_CODE (PATTERN (insn)) != USE
	      && asm_noperands (PATTERN (insn)) < 0)))
    {
      cselib_val *val = cselib_lookup (x, GET_MODE (x), 0, VOIDmode);
      struct elt_loc_list *l;

      if (!val)
	return false;
      for (l = val->locs; l; l = l->next)
	{
	  rtx this_rtx = l->loc;
	  rtx note;

	  if (cprop_constant_p (this_rtx))
	    newcnst = this_rtx;
	  if (cprop_reg_p (this_rtx)
	      /* Don't copy propagate if it has attached REG_EQUIV note.
		 At this point this only function parameters should have
		 REG_EQUIV notes and if the argument slot is used somewhere
		 explicitly, it means address of parameter has been taken,
		 so we should not extend the lifetime of the pseudo.  */
	      && (!(note = find_reg_note (l->setting_insn, REG_EQUIV, NULL_RTX))
		  || ! MEM_P (XEXP (note, 0))))
	    newreg = this_rtx;
	}
      if (newcnst && constprop_register (x, newcnst, insn))
	{
	  if (dump_file != NULL)
	    {
	      fprintf (dump_file, "LOCAL CONST-PROP: Replacing reg %d in ",
		       REGNO (x));
	      fprintf (dump_file, "insn %d with constant ",
		       INSN_UID (insn));
	      print_rtl (dump_file, newcnst);
	      fprintf (dump_file, "\n");
	    }
	  local_const_prop_count++;
	  return true;
	}
      else if (newreg && newreg != x && try_replace_reg (x, newreg, insn))
	{
	  if (dump_file != NULL)
	    {
	      fprintf (dump_file,
		       "LOCAL COPY-PROP: Replacing reg %d in insn %d",
		       REGNO (x), INSN_UID (insn));
	      fprintf (dump_file, " with reg %d\n", REGNO (newreg));
	    }
	  local_copy_prop_count++;
	  return true;
	}
    }
  return false;
}

/* Do local const/copy propagation (i.e. within each basic block).  */

static int
local_cprop_pass (void)
{
  basic_block bb;
  rtx_insn *insn;
  bool changed = false;
  unsigned i;

  auto_vec<rtx_insn *> uncond_traps;

  cselib_init (0);
  FOR_EACH_BB_FN (bb, cfun)
    {
      FOR_BB_INSNS (bb, insn)
	{
	  if (INSN_P (insn))
	    {
	      bool was_uncond_trap
		= (GET_CODE (PATTERN (insn)) == TRAP_IF
		   && XEXP (PATTERN (insn), 0) == const1_rtx);
	      rtx note = find_reg_equal_equiv_note (insn);
	      do
		{
		  reg_use_count = 0;
		  note_uses (&PATTERN (insn), local_cprop_find_used_regs,
			     NULL);
		  if (note)
		    local_cprop_find_used_regs (&XEXP (note, 0), NULL);

		  for (i = 0; i < reg_use_count; i++)
		    {
		      if (do_local_cprop (reg_use_table[i], insn))
			{
			  if (!DEBUG_INSN_P (insn))
			    changed = true;
			  break;
			}
		    }
		  if (!was_uncond_trap
		      && GET_CODE (PATTERN (insn)) == TRAP_IF
		      && XEXP (PATTERN (insn), 0) == const1_rtx)
		    {
		      uncond_traps.safe_push (insn);
		      break;
		    }
		  if (insn->deleted ())
		    break;
		}
	      while (i < reg_use_count);
	    }
	  cselib_process_insn (insn);
	}

      /* Forget everything at the end of a basic block.  */
      cselib_clear_table ();
    }

  cselib_finish ();

  while (!uncond_traps.is_empty ())
    {
      rtx_insn *insn = uncond_traps.pop ();
      basic_block to_split = BLOCK_FOR_INSN (insn);
      remove_edge (split_block (to_split, insn));
      emit_barrier_after_bb (to_split);
    }

  return changed;
}

/* Similar to get_condition, only the resulting condition must be
   valid at JUMP, instead of at EARLIEST.

   This differs from noce_get_condition in ifcvt.c in that we prefer not to
   settle for the condition variable in the jump instruction being integral.
   We prefer to be able to record the value of a user variable, rather than
   the value of a temporary used in a condition.  This could be solved by
   recording the value of *every* register scanned by canonicalize_condition,
   but this would require some code reorganization.  */

rtx
fis_get_condition (rtx_insn *jump)
{
  return get_condition (jump, NULL, false, true);
}

/* Check the comparison COND to see if we can safely form an implicit
   set from it.  */

static bool
implicit_set_cond_p (const_rtx cond)
{
  machine_mode mode;
  rtx cst;

  /* COND must be either an EQ or NE comparison.  */
  if (GET_CODE (cond) != EQ && GET_CODE (cond) != NE)
    return false;

  /* The first operand of COND must be a register we can propagate.  */
  if (!cprop_reg_p (XEXP (cond, 0)))
    return false;

  /* The second operand of COND must be a suitable constant.  */
  mode = GET_MODE (XEXP (cond, 0));
  cst = XEXP (cond, 1);

  /* We can't perform this optimization if either operand might be or might
     contain a signed zero.  */
  if (HONOR_SIGNED_ZEROS (mode))
    {
      /* It is sufficient to check if CST is or contains a zero.  We must
	 handle float, complex, and vector.  If any subpart is a zero, then
	 the optimization can't be performed.  */
      /* ??? The complex and vector checks are not implemented yet.  We just
	 always return zero for them.  */
      if (CONST_DOUBLE_AS_FLOAT_P (cst)
	  && real_equal (CONST_DOUBLE_REAL_VALUE (cst), &dconst0))
	return 0;
      else
	return 0;
    }

  return cprop_constant_p (cst);
}

/* Find the implicit sets of a function.  An "implicit set" is a constraint
   on the value of a variable, implied by a conditional jump.  For example,
   following "if (x == 2)", the then branch may be optimized as though the
   conditional performed an "explicit set", in this example, "x = 2".  This
   function records the set patterns that are implicit at the start of each
   basic block.

   If an implicit set is found but the set is implicit on a critical edge,
   this critical edge is split.

   Return true if the CFG was modified, false otherwise.  */

static bool
find_implicit_sets (void)
{
  basic_block bb, dest;
  rtx cond, new_rtx;
  unsigned int count = 0;
  bool edges_split = false;
  size_t implicit_sets_size = last_basic_block_for_fn (cfun) + 10;

  implicit_sets = XCNEWVEC (rtx, implicit_sets_size);

  FOR_EACH_BB_FN (bb, cfun)
    {
      /* Check for more than one successor.  */
      if (EDGE_COUNT (bb->succs) <= 1)
	continue;

      cond = fis_get_condition (BB_END (bb));

      /* If no condition is found or if it isn't of a suitable form,
	 ignore it.  */
      if (! cond || ! implicit_set_cond_p (cond))
	continue;

      dest = GET_CODE (cond) == EQ
	? BRANCH_EDGE (bb)->dest : FALLTHRU_EDGE (bb)->dest;

      /* If DEST doesn't go anywhere, ignore it.  */
      if (! dest || dest == EXIT_BLOCK_PTR_FOR_FN (cfun))
	continue;

      /* We have found a suitable implicit set.  Try to record it now as
	 a SET in DEST.  If DEST has more than one predecessor, the edge
	 between BB and DEST is a critical edge and we must split it,
	 because we can only record one implicit set per DEST basic block.  */
      if (! single_pred_p (dest))
        {
	  dest = split_edge (find_edge (bb, dest));
	  edges_split = true;
	}

      if (implicit_sets_size <= (size_t) dest->index)
      {
        size_t old_implicit_sets_size = implicit_sets_size;
	implicit_sets_size *= 2;
	implicit_sets = XRESIZEVEC (rtx, implicit_sets, implicit_sets_size);
	memset (implicit_sets + old_implicit_sets_size, 0,
		(implicit_sets_size - old_implicit_sets_size) * sizeof (rtx));
      }

      new_rtx = gen_rtx_SET (XEXP (cond, 0), XEXP (cond, 1));
      implicit_sets[dest->index] = new_rtx;
      if (dump_file)
	{
	  fprintf (dump_file, "Implicit set of reg %d in ",
		   REGNO (XEXP (cond, 0)));
	  fprintf (dump_file, "basic block %d\n", dest->index);
	}
      count++;
    }

  if (dump_file)
    fprintf (dump_file, "Found %d implicit sets\n", count);

  /* Confess our sins.  */
  return edges_split;
}

/* Bypass conditional jumps.  */

/* The value of last_basic_block at the beginning of the jump_bypass
   pass.  The use of redirect_edge_and_branch_force may introduce new
   basic blocks, but the data flow analysis is only valid for basic
   block indices less than bypass_last_basic_block.  */

static int bypass_last_basic_block;

/* Find a set of REGNO to a constant that is available at the end of basic
   block BB.  Return NULL if no such set is found.  Based heavily upon
   find_avail_set.  */

static struct cprop_expr *
find_bypass_set (int regno, int bb)
{
  struct cprop_expr *result = 0;

  for (;;)
    {
      rtx src;
      struct cprop_expr *set = lookup_set (regno, &set_hash_table);

      while (set)
	{
	  if (bitmap_bit_p (cprop_avout[bb], set->bitmap_index))
	    break;
	  set = next_set (regno, set);
	}

      if (set == 0)
	break;

      src = set->src;
      if (cprop_constant_p (src))
	result = set;

      if (! REG_P (src))
	break;

      regno = REGNO (src);
    }
  return result;
}

/* Subroutine of bypass_block that checks whether a pseudo is killed by
   any of the instructions inserted on an edge.  Jump bypassing places
   condition code setters on CFG edges using insert_insn_on_edge.  This
   function is required to check that our data flow analysis is still
   valid prior to commit_edge_insertions.  */

static bool
reg_killed_on_edge (const_rtx reg, const_edge e)
{
  rtx_insn *insn;

  for (insn = e->insns.r; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn) && reg_set_p (reg, insn))
      return true;

  return false;
}

/* Subroutine of bypass_conditional_jumps that attempts to bypass the given
   basic block BB which has more than one predecessor.  If not NULL, SETCC
   is the first instruction of BB, which is immediately followed by JUMP_INSN
   JUMP.  Otherwise, SETCC is NULL, and JUMP is the first insn of BB.
   Returns nonzero if a change was made.

   During the jump bypassing pass, we may place copies of SETCC instructions
   on CFG edges.  The following routine must be careful to pay attention to
   these inserted insns when performing its transformations.  */

static int
bypass_block (basic_block bb, rtx_insn *setcc, rtx_insn *jump)
{
  rtx_insn *insn;
  rtx note;
  edge e, edest;
  int change;
  int may_be_loop_header = false;
  unsigned removed_p;
  unsigned i;
  edge_iterator ei;

  insn = (setcc != NULL) ? setcc : jump;

  /* Determine set of register uses in INSN.  */
  reg_use_count = 0;
  note_uses (&PATTERN (insn), find_used_regs, NULL);
  note = find_reg_equal_equiv_note (insn);
  if (note)
    find_used_regs (&XEXP (note, 0), NULL);

  if (current_loops)
    {
      /* If we are to preserve loop structure then do not bypass
         a loop header.  This will either rotate the loop, create
	 multiple entry loops or even irreducible regions.  */
      if (bb == bb->loop_father->header)
	return 0;
    }
  else
    {
      FOR_EACH_EDGE (e, ei, bb->preds)
	if (e->flags & EDGE_DFS_BACK)
	  {
	    may_be_loop_header = true;
	    break;
	  }
    }

  change = 0;
  for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); )
    {
      removed_p = 0;

      if (e->flags & EDGE_COMPLEX)
	{
	  ei_next (&ei);
	  continue;
	}

      /* We can't redirect edges from new basic blocks.  */
      if (e->src->index >= bypass_last_basic_block)
	{
	  ei_next (&ei);
	  continue;
	}

      /* The irreducible loops created by redirecting of edges entering the
	 loop from outside would decrease effectiveness of some of the
	 following optimizations, so prevent this.  */
      if (may_be_loop_header
	  && !(e->flags & EDGE_DFS_BACK))
	{
	  ei_next (&ei);
	  continue;
	}

      for (i = 0; i < reg_use_count; i++)
	{
	  rtx reg_used = reg_use_table[i];
	  unsigned int regno = REGNO (reg_used);
	  basic_block dest, old_dest;
	  struct cprop_expr *set;
	  rtx src, new_rtx;

	  set = find_bypass_set (regno, e->src->index);

	  if (! set)
	    continue;

	  /* Check the data flow is valid after edge insertions.  */
	  if (e->insns.r && reg_killed_on_edge (reg_used, e))
	    continue;

	  src = SET_SRC (pc_set (jump));

	  if (setcc != NULL)
	    src = simplify_replace_rtx (src,
					SET_DEST (PATTERN (setcc)),
					SET_SRC (PATTERN (setcc)));

	  new_rtx = simplify_replace_rtx (src, reg_used, set->src);

	  /* Jump bypassing may have already placed instructions on
	     edges of the CFG.  We can't bypass an outgoing edge that
	     has instructions associated with it, as these insns won't
	     get executed if the incoming edge is redirected.  */
	  if (new_rtx == pc_rtx)
	    {
	      edest = FALLTHRU_EDGE (bb);
	      dest = edest->insns.r ? NULL : edest->dest;
	    }
	  else if (GET_CODE (new_rtx) == LABEL_REF)
	    {
	      dest = BLOCK_FOR_INSN (XEXP (new_rtx, 0));
	      /* Don't bypass edges containing instructions.  */
	      edest = find_edge (bb, dest);
	      if (edest && edest->insns.r)
		dest = NULL;
	    }
	  else
	    dest = NULL;

	  /* Avoid unification of the edge with other edges from original
	     branch.  We would end up emitting the instruction on "both"
	     edges.  */
	  if (dest && setcc && !CC0_P (SET_DEST (PATTERN (setcc)))
	      && find_edge (e->src, dest))
	    dest = NULL;

	  old_dest = e->dest;
	  if (dest != NULL
	      && dest != old_dest
	      && dest != EXIT_BLOCK_PTR_FOR_FN (cfun))
            {
	      redirect_edge_and_branch_force (e, dest);

	      /* Copy the register setter to the redirected edge.
		 Don't copy CC0 setters, as CC0 is dead after jump.  */
	      if (setcc)
		{
		  rtx pat = PATTERN (setcc);
		  if (!CC0_P (SET_DEST (pat)))
		    insert_insn_on_edge (copy_insn (pat), e);
		}

	      if (dump_file != NULL)
		{
		  fprintf (dump_file, "JUMP-BYPASS: Proved reg %d "
				      "in jump_insn %d equals constant ",
			   regno, INSN_UID (jump));
		  print_rtl (dump_file, set->src);
		  fprintf (dump_file, "\n\t     when BB %d is entered from "
				      "BB %d.  Redirect edge %d->%d to %d.\n",
			   old_dest->index, e->src->index, e->src->index,
			   old_dest->index, dest->index);
		}
	      change = 1;
	      removed_p = 1;
	      break;
	    }
	}
      if (!removed_p)
	ei_next (&ei);
    }
  return change;
}

/* Find basic blocks with more than one predecessor that only contain a
   single conditional jump.  If the result of the comparison is known at
   compile-time from any incoming edge, redirect that edge to the
   appropriate target.  Return nonzero if a change was made.

   This function is now mis-named, because we also handle indirect jumps.  */

static int
bypass_conditional_jumps (void)
{
  basic_block bb;
  int changed;
  rtx_insn *setcc;
  rtx_insn *insn;
  rtx dest;

  /* Note we start at block 1.  */
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
    return 0;

  mark_dfs_back_edges ();

  changed = 0;
  FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->next_bb,
		  EXIT_BLOCK_PTR_FOR_FN (cfun), next_bb)
    {
      /* Check for more than one predecessor.  */
      if (!single_pred_p (bb))
	{
	  setcc = NULL;
	  FOR_BB_INSNS (bb, insn)
	    if (DEBUG_INSN_P (insn))
	      continue;
	    else if (NONJUMP_INSN_P (insn))
	      {
		if (setcc)
		  break;
		if (GET_CODE (PATTERN (insn)) != SET)
		  break;

		dest = SET_DEST (PATTERN (insn));
		if (REG_P (dest) || CC0_P (dest))
		  setcc = insn;
		else
		  break;
	      }
	    else if (JUMP_P (insn))
	      {
		if ((any_condjump_p (insn) || computed_jump_p (insn))
		    && onlyjump_p (insn))
		  changed |= bypass_block (bb, setcc, insn);
		break;
	      }
	    else if (INSN_P (insn))
	      break;
	}
    }

  /* If we bypassed any register setting insns, we inserted a
     copy on the redirected edge.  These need to be committed.  */
  if (changed)
    commit_edge_insertions ();

  return changed;
}

/* Main function for the CPROP pass.  */

static int
one_cprop_pass (void)
{
  int i;
  int changed = 0;

  /* Return if there's nothing to do, or it is too expensive.  */
  if (n_basic_blocks_for_fn (cfun) <= NUM_FIXED_BLOCKS + 1
      || gcse_or_cprop_is_too_expensive (_ ("const/copy propagation disabled")))
    return 0;

  global_const_prop_count = local_const_prop_count = 0;
  global_copy_prop_count = local_copy_prop_count = 0;

  bytes_used = 0;
  gcc_obstack_init (&cprop_obstack);

  /* Do a local const/copy propagation pass first.  The global pass
     only handles global opportunities.
     If the local pass changes something, remove any unreachable blocks
     because the CPROP global dataflow analysis may get into infinite
     loops for CFGs with unreachable blocks.

     FIXME: This local pass should not be necessary after CSE (but for
	    some reason it still is).  It is also (proven) not necessary
	    to run the local pass right after FWPWOP.

     FIXME: The global analysis would not get into infinite loops if it
	    would use the DF solver (via df_simple_dataflow) instead of
	    the solver implemented in this file.  */
  changed |= local_cprop_pass ();
  if (changed)
    delete_unreachable_blocks ();

  /* Determine implicit sets.  This may change the CFG (split critical
     edges if that exposes an implicit set).
     Note that find_implicit_sets() does not rely on up-to-date DF caches
     so that we do not have to re-run df_analyze() even if local CPROP
     changed something.
     ??? This could run earlier so that any uncovered implicit sets
	 sets could be exploited in local_cprop_pass() also.  Later.  */
  changed |= find_implicit_sets ();

  /* If local_cprop_pass() or find_implicit_sets() changed something,
     run df_analyze() to bring all insn caches up-to-date, and to take
     new basic blocks from edge splitting on the DF radar.
     NB: This also runs the fast DCE pass, because execute_rtl_cprop
     sets DF_LR_RUN_DCE.  */
  if (changed)
    df_analyze ();

  /* Initialize implicit_set_indexes array.  */
  implicit_set_indexes = XNEWVEC (int, last_basic_block_for_fn (cfun));
  for (i = 0; i < last_basic_block_for_fn (cfun); i++)
    implicit_set_indexes[i] = -1;

  alloc_hash_table (&set_hash_table);
  compute_hash_table (&set_hash_table);

  /* Free implicit_sets before peak usage.  */
  free (implicit_sets);
  implicit_sets = NULL;

  if (dump_file)
    dump_hash_table (dump_file, "SET", &set_hash_table);
  if (set_hash_table.n_elems > 0)
    {
      basic_block bb;
      auto_vec<rtx_insn *> uncond_traps;

      alloc_cprop_mem (last_basic_block_for_fn (cfun),
		       set_hash_table.n_elems);
      compute_cprop_data ();

      free (implicit_set_indexes);
      implicit_set_indexes = NULL;

      /* Allocate vars to track sets of regs.  */
      reg_set_bitmap = ALLOC_REG_SET (NULL);

      FOR_BB_BETWEEN (bb, ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->next_bb,
		      EXIT_BLOCK_PTR_FOR_FN (cfun),
		      next_bb)
	{
	  bool seen_uncond_trap = false;
	  rtx_insn *insn;

	  /* Reset tables used to keep track of what's still valid [since
	     the start of the block].  */
	  reset_opr_set_tables ();

	  FOR_BB_INSNS (bb, insn)
	    if (INSN_P (insn))
	      {
		bool was_uncond_trap
		  = (GET_CODE (PATTERN (insn)) == TRAP_IF
		     && XEXP (PATTERN (insn), 0) == const1_rtx);

		changed |= cprop_insn (insn);

		/* Keep track of everything modified by this insn.  */
		/* ??? Need to be careful w.r.t. mods done to INSN.
		       Don't call mark_oprs_set if we turned the
		       insn into a NOTE, or deleted the insn.  */
		if (! NOTE_P (insn) && ! insn->deleted ())
		  mark_oprs_set (insn);

		if (!was_uncond_trap
		    && GET_CODE (PATTERN (insn)) == TRAP_IF
		    && XEXP (PATTERN (insn), 0) == const1_rtx)
		  {
		    /* If we have already seen an unconditional trap
		       earlier, the rest of the bb is going to be removed
		       as unreachable.  Just turn it into a note, so that
		       RTL verification doesn't complain about it before
		       it is finally removed.  */
		    if (seen_uncond_trap)
		      set_insn_deleted (insn);
		    else
		      {
			seen_uncond_trap = true;
			uncond_traps.safe_push (insn);
		      }
		  }
	      }
	}

      /* Make sure bypass_conditional_jumps will ignore not just its new
	 basic blocks, but also the ones after unconditional traps (those are
	 unreachable and will be eventually removed as such).  */
      bypass_last_basic_block = last_basic_block_for_fn (cfun);

      while (!uncond_traps.is_empty ())
	{
	  rtx_insn *insn = uncond_traps.pop ();
	  basic_block to_split = BLOCK_FOR_INSN (insn);
	  remove_edge (split_block (to_split, insn));
	  emit_barrier_after_bb (to_split);
	}

      changed |= bypass_conditional_jumps ();

      FREE_REG_SET (reg_set_bitmap);
      free_cprop_mem ();
    }
  else
    {
      free (implicit_set_indexes);
      implicit_set_indexes = NULL;
    }

  free_hash_table (&set_hash_table);
  obstack_free (&cprop_obstack, NULL);

  if (dump_file)
    {
      fprintf (dump_file, "CPROP of %s, %d basic blocks, %d bytes needed, ",
	       current_function_name (), n_basic_blocks_for_fn (cfun),
	       bytes_used);
      fprintf (dump_file, "%d local const props, %d local copy props, ",
	       local_const_prop_count, local_copy_prop_count);
      fprintf (dump_file, "%d global const props, %d global copy props\n\n",
	       global_const_prop_count, global_copy_prop_count);
    }

  return changed;
}

/* All the passes implemented in this file.  Each pass has its
   own gate and execute function, and at the end of the file a
   pass definition for passes.c.

   We do not construct an accurate cfg in functions which call
   setjmp, so none of these passes runs if the function calls
   setjmp.
   FIXME: Should just handle setjmp via REG_SETJMP notes.  */

static unsigned int
execute_rtl_cprop (void)
{
  int changed;
  delete_unreachable_blocks ();
  df_set_flags (DF_LR_RUN_DCE);
  df_analyze ();
  changed = one_cprop_pass ();
  flag_rerun_cse_after_global_opts |= changed;
  if (changed)
    cleanup_cfg (CLEANUP_CFG_CHANGED);
  return 0;
}

namespace {

const pass_data pass_data_rtl_cprop =
{
  RTL_PASS, /* type */
  "cprop", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_CPROP, /* tv_id */
  PROP_cfglayout, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_rtl_cprop : public rtl_opt_pass
{
public:
  pass_rtl_cprop (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_rtl_cprop, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_rtl_cprop (m_ctxt); }
  virtual bool gate (function *fun)
    {
      return optimize > 0 && flag_gcse
	&& !fun->calls_setjmp
	&& dbg_cnt (cprop);
    }

  virtual unsigned int execute (function *) { return execute_rtl_cprop (); }

}; // class pass_rtl_cprop

} // anon namespace

rtl_opt_pass *
make_pass_rtl_cprop (gcc::context *ctxt)
{
  return new pass_rtl_cprop (ctxt);
}
