/* Register renaming for the GNU compiler.
   Copyright (C) 2000 Free Software Foundation, Inc.

   This file is part of GNU CC.

   GNU CC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GNU CC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GNU CC; see the file COPYING.  If not, write to
   the Free Software Foundation, 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "output.h"
#include "function.h"
#include "recog.h"
#include "resource.h"

static const char *const reg_class_names[] = REG_CLASS_NAMES;

/* ??? Consider a more sparse data structure? */
typedef struct def_uses
  {
    /* high bound of defs and uses */
    int high_bound;

    /* 1 if insn y defines a reg whose use crosses a call 
       y is the ordinal position of the insn within the block */
    sbitmap require_call_save_reg;

    /* REGNO x INSN y  1 if insn y sets reg x */
    sbitmap *defs;

    /* REGNO x INSN y  The register class for this def */
    enum reg_class *def_class;

    /* REGNO x INSN y  1 if insn y uses reg x */
    sbitmap *uses;

    /* REGNO x INSN y  The register class for this use */
    enum reg_class *use_class;
  }
def_uses;

#define DU_REG_CLASS(rc,r,high_bound,i) (rc[r * high_bound + i])

typedef struct ext_basic_blocks
  {
    /* n_basic_blocks x n_basic_blocks y  1 if bb y is in extended bb
       having entry x */
    sbitmap *basic_block;

    /* n_basic_blocks x n_basic_blocks y  1 if bb y is an exit block */
    sbitmap *exit;
  }
ext_basic_blocks;

#define UID_RUID_HIGH_BOUND 64
#define DESTINATION 1
#define SOURCE 2

static void build_def_use PARAMS ((int, ext_basic_blocks *, HARD_REG_SET *,
				   def_uses *, sbitmap *));
static int replace_reg_in_block
  PARAMS ((def_uses *, varray_type *, int, rtx, int));
static int consider_def PARAMS ((rtx, int, def_uses *, int));
static int consider_available PARAMS ((rtx, int, HARD_REG_SET *, int, def_uses *, int));
static rtx rr_replace_reg PARAMS ((rtx, rtx, rtx, int, rtx, int *));
static int consider_use PARAMS ((rtx, int, int, int));
static int condmove_p PARAMS ((rtx));
static void dump_def_use_chain PARAMS ((HARD_REG_SET *, def_uses *,
					varray_type *));
static void dump_ext_bb_info PARAMS ((int, ext_basic_blocks *));
static void find_ext_basic_blocks PARAMS ((ext_basic_blocks *));
static void find_one_ext_basic_block PARAMS ((int, basic_block, sbitmap *,
					      ext_basic_blocks *));
static enum reg_class get_reg_class PARAMS ((rtx, rtx, int, enum reg_class));
static rtx regno_first_use_in PARAMS ((int, rtx));

void
regrename_optimize ()
{
  int b, eb, i, inum, r, rc, replace_ok;
  rtx insn;
  def_uses du;
  ext_basic_blocks ebb;


  /* Registers used in a given class */
  HARD_REG_SET class_regs;

  /* Registers available for use as renaming registers */
  HARD_REG_SET avail_regs;

  /* Registers used in the block */
  HARD_REG_SET regs_used;

  /* Registers which have been used as renaming registers */
  HARD_REG_SET renamed_regs;

  HARD_REG_SET global_live_at_end, global_live_at_start;

  HARD_REG_SET null_bitmap, tmp_bitmap;

  /* 1 if insn y sets a register which is live at the end of the block */
  sbitmap defs_live_exit;

  /* Mapping from insn y (ordinal position in block) to INSN_UID */
  varray_type uid_ruid;

  /* Mapping from insn y (ordinal position in block) to block id */
  varray_type uid_rbid;

  /* Ordinal position in block of defining insn */
  int *def_idx;

  VARRAY_RTX_INIT (uid_ruid, UID_RUID_HIGH_BOUND + 1, "uid_ruid");
  VARRAY_LONG_INIT (uid_rbid, UID_RUID_HIGH_BOUND + 1, "uid_rbid");

  ebb.basic_block =
    sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  sbitmap_vector_zero (ebb.basic_block, n_basic_blocks);
  ebb.exit =
    sbitmap_vector_alloc (n_basic_blocks, n_basic_blocks);
  sbitmap_vector_zero (ebb.exit, n_basic_blocks);

  find_ext_basic_blocks (&ebb);

  du.def_class = du.use_class = 0;

  /* Build uid_ruid and uid_rbid for this extended basic block */
  for (b = 0; b < n_basic_blocks; b++)
    if (TEST_BIT (ebb.basic_block[b], b))
      {
	for (eb = du.high_bound = 0; eb < n_basic_blocks; eb++)
	  {
	    if (TEST_BIT (ebb.basic_block[b], eb))
	      {
		basic_block bb = BASIC_BLOCK (eb);
		/* Calculate high bound for uid_ruid and allocate if necessary */
		for (insn = bb->head;
		     insn != NEXT_INSN (bb->end);
		     du.high_bound++, insn = NEXT_INSN (insn))
		  {
		    int uid_ruid_high_bound = VARRAY_SIZE (uid_ruid);
		    if (du.high_bound + 4 >= uid_ruid_high_bound)
		      {
			VARRAY_GROW (uid_ruid, uid_ruid_high_bound * 2);
			VARRAY_GROW (uid_rbid, uid_ruid_high_bound * 2);
		      }
		    VARRAY_RTX (uid_ruid, du.high_bound) = insn;
		    VARRAY_LONG (uid_rbid, du.high_bound) = eb;
		  }
	      }
	  }

	CLEAR_HARD_REG_SET (null_bitmap);
	CLEAR_HARD_REG_SET (class_regs);
	CLEAR_HARD_REG_SET (regs_used);
	CLEAR_HARD_REG_SET (avail_regs);
	CLEAR_HARD_REG_SET (tmp_bitmap);
	CLEAR_HARD_REG_SET (renamed_regs);

	du.defs =
	  sbitmap_vector_alloc (FIRST_PSEUDO_REGISTER, du.high_bound + 1);
	sbitmap_vector_zero (du.defs, FIRST_PSEUDO_REGISTER);
	du.uses =
	  sbitmap_vector_alloc (FIRST_PSEUDO_REGISTER, du.high_bound + 1);
	sbitmap_vector_zero (du.uses, FIRST_PSEUDO_REGISTER);
	du.require_call_save_reg = sbitmap_alloc (du.high_bound + 1);
	sbitmap_zero (du.require_call_save_reg);
	defs_live_exit = sbitmap_alloc (du.high_bound + 1);
	sbitmap_zero (defs_live_exit);

	du.def_class = xrealloc
	  (du.def_class,
	   sizeof (enum reg_class) * FIRST_PSEUDO_REGISTER * du.high_bound);

	du.use_class = xrealloc
	  (du.use_class,
	   sizeof (enum reg_class) * FIRST_PSEUDO_REGISTER * du.high_bound);

	build_def_use (b, &ebb, &regs_used, &du,
		       &defs_live_exit);

	if (rtl_dump_file)
	  {
	    dump_ext_bb_info (b, &ebb);
	    dump_def_use_chain (&global_live_at_end, &du, &uid_ruid);
	  }

	/* Available registers are not: used in the block, live at the start,
	   live at the end, a register we've renamed to. */
	/* ??? The current algorithm is pessimistic for extended basic blocks
	   as it just treats them as a big basic block. */

	COPY_HARD_REG_SET (tmp_bitmap, regs_used);
	REG_SET_TO_HARD_REG_SET (global_live_at_start, BASIC_BLOCK (b)->global_live_at_start);
	IOR_HARD_REG_SET (tmp_bitmap, global_live_at_start);
	for (eb = 0; eb < n_basic_blocks; eb++)
	  {
	    if (TEST_BIT (ebb.basic_block[b], eb))
	      {
		basic_block bb = BASIC_BLOCK (eb);
		REG_SET_TO_HARD_REG_SET (global_live_at_end, bb->global_live_at_end);
		IOR_HARD_REG_SET (tmp_bitmap, global_live_at_end);
	      }
	  }

	def_idx = xcalloc (du.high_bound, sizeof (int));

	/* Only consider registers in this extended block and in this class
	   that are defined more than once.  Replace them if permissible. */
	for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
	  {
	    int avail_reg, ar_idx, def, def_cnt = 0, use_idx, call_idx;

	    if (!TEST_HARD_REG_BIT (regs_used, r)
		|| fixed_regs[r]
		|| r == FRAME_POINTER_REGNUM)
	      continue;

	    /* Find def_idx[N] where hbound of N is the number of 
	       definitions of this register in this block. and def_idx
	       is the ordinal position of this insn in the block. */
	    for (i = 0, def_idx[def_cnt] = 0;
		 i < du.high_bound;
		 i++)
	      {
		if (TEST_BIT (du.defs[r], i)
		    && consider_def (VARRAY_RTX (uid_ruid, i), r,
				     &du, i))
		  {
		    int first_use = 1;
		    def_idx[def_cnt] = i;

		    /* Only consider definitions that have a use. */
		    for (use_idx = i + 1; use_idx < du.high_bound;
			 use_idx++)
		      {
			if (TEST_BIT (du.uses[r], use_idx))
			  {
			    if (consider_use (VARRAY_RTX (uid_ruid, use_idx), r,
					      VARRAY_LONG (uid_rbid, i),
					   VARRAY_LONG (uid_rbid, use_idx)))
			      {
				if (first_use)
				  {
				    first_use = 0;
				    def_cnt++;
				  }
			      }
			    else
			      {
				/* Don't consider def if we don't want this use */
				if (!first_use)
				  def_cnt--;
				break;
			      }
			  }
			if (TEST_BIT (du.defs[r], use_idx))
			  break;
		      }
		    /* Scan until the next def to avoid renaming
		       parameter registers. */
		    /* ??? consider using CALL_INSN_FUNCTION_USAGE */
		    for (call_idx = i; call_idx <= use_idx; call_idx++)
		      if (VARRAY_RTX (uid_ruid, call_idx)
			  && GET_CODE (VARRAY_RTX (uid_ruid, call_idx))
			  == CALL_INSN)
			{
			  SET_BIT (du.require_call_save_reg, i);
			}
		  }
	      }
	    if (def_cnt < 2)
	      continue;

	    /* We have more than one def so rename until we exhaust
	       renaming registers. */
	    /* ??? Should we continue renaming round robin when we exhaust
	       renaming registers? */
	    for (def = 0; def < def_cnt - 1; def++)
	      {
		if (!TEST_BIT (defs_live_exit, def_idx[def])
		    && (GET_RTX_CLASS
			(GET_CODE (VARRAY_RTX (uid_ruid,
					       def_idx[def]))) == 'i'))
		  {
		    rtx reg_use = regno_first_use_in
		    (r, PATTERN (VARRAY_RTX (uid_ruid, def_idx[def])));

		    if (!reg_use)
		      break;
#ifdef STACK_REGS
		    /* Don't bother with stacked float registers */
		    if (GET_MODE_CLASS (GET_MODE (reg_use)) == MODE_FLOAT)
		      break;
#endif
		    rc = (int) DU_REG_CLASS (du.def_class,
				      r, du.high_bound, def_idx[def]);
		    COPY_HARD_REG_SET (avail_regs,
				   reg_class_contents[(enum reg_class) rc]);
		    AND_COMPL_HARD_REG_SET (avail_regs, tmp_bitmap);
		    AND_COMPL_HARD_REG_SET (avail_regs, renamed_regs);

		    /* No available registers in this class */
		    GO_IF_HARD_REG_EQUAL (avail_regs, null_bitmap,
					  no_available_regs);
		    for (ar_idx = 0; ar_idx < FIRST_PSEUDO_REGISTER
			 && TEST_HARD_REG_BIT (avail_regs, ar_idx); ar_idx++)
		      ;
		    if (ar_idx == FIRST_PSEUDO_REGISTER)
		      goto no_available_regs;

		    /* Only try register renaming if there is an available
		       register in this class. */
		    for (ar_idx = 0;
			 ar_idx < FIRST_PSEUDO_REGISTER;
			 ar_idx++)
		      {
			avail_reg = reg_alloc_order[ar_idx];
			if (consider_available (reg_use, avail_reg, &avail_regs,
						rc, &du, def_idx[def]))
			  break;
		      }

		    if (ar_idx == FIRST_PSEUDO_REGISTER)
		      {
			if (rtl_dump_file)
			  {
			    fprintf (rtl_dump_file,
				     "Register %s in class %s",
				     reg_names[r], reg_class_names[rc]);
			    fprintf (rtl_dump_file,
				     " in insn %d",
				     INSN_UID (VARRAY_RTX (uid_ruid,
							   def_idx[def])));

			    if (TEST_BIT (du.require_call_save_reg,
					  def_idx[def]))
			      fprintf (rtl_dump_file, " crosses a call");
			    fprintf (rtl_dump_file, ". No available registers\n");
			  }
			goto try_next_def;
		      }

		    SET_HARD_REG_BIT (renamed_regs, avail_reg);
		    CLEAR_HARD_REG_BIT (avail_regs, avail_reg);

		    /* Replace in destination.  Replace in source for
		       remainder of block until new register is defined
		       again */
		    replace_ok = replace_reg_in_block
		      (&du, &uid_ruid, def_idx[def], reg_use, avail_reg);
		    /* Replace failed, so restore previous register */
		    if (!replace_ok)
		      {
			replace_reg_in_block (&du, &uid_ruid, def_idx[def],
					    gen_rtx_REG (GET_MODE (reg_use),
							 avail_reg),
					      REGNO (reg_use));
			if (rtl_dump_file)
			  fprintf (rtl_dump_file,
				   "Register %s in class %s Renaming as %s would not satisfy constraints\n",
				   reg_names[r], reg_class_names[rc],
				   reg_names[avail_reg]);
		      }
		    else if (rtl_dump_file)
		      fprintf (rtl_dump_file,
		       "Register %s in class %s Renamed as %s at insn %d\n",
			       reg_names[r], reg_class_names[rc],
			       reg_names[avail_reg],
			    INSN_UID (VARRAY_RTX (uid_ruid, def_idx[def])));
		  }
	      try_next_def:
		continue;
	      }
	    sbitmap_zero (du.defs[r]);
	  no_available_regs:
	    continue;
	  }
	free (def_idx);
	sbitmap_vector_free (du.defs);
	sbitmap_vector_free (du.uses);
	sbitmap_free (du.require_call_save_reg);
	sbitmap_free (defs_live_exit);
	CLEAR_HARD_REG_SET (regs_used);
	CLEAR_HARD_REG_SET (renamed_regs);

	for (inum = 0; inum < (int) VARRAY_SIZE (uid_ruid); inum++)
	  VARRAY_RTX (uid_ruid, inum) = (rtx) 0;
      }

  sbitmap_vector_free (ebb.basic_block);
  sbitmap_vector_free (ebb.exit);
}

/* Build def/use chain DU for extended basic block EBB having root B.
   Also determine which regs are used, REGS_USED, and which insns define
   a live at exit def, DEFS_LIVE_EXIT */

static void
build_def_use (b, ebb, regs_used, du, defs_live_exit)
     int b;
     ext_basic_blocks *ebb;
     HARD_REG_SET *regs_used;
     def_uses *du;
     sbitmap *defs_live_exit;
{
  rtx insn;
  int eb, inum, r;

  inum = 0;
  for (eb = 0; eb < n_basic_blocks; eb++)
    {
      basic_block bb = BASIC_BLOCK (eb);

      if (!TEST_BIT (ebb->basic_block[b], eb))
	continue;

      for (insn = bb->head;
	   insn != NEXT_INSN (bb->end);
	   inum++, insn = NEXT_INSN (insn))
	{
	  struct resources insn_res;
	  struct resources insn_sets;

	  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	    continue;

	  CLEAR_RESOURCE (&insn_sets);
	  mark_set_resources (insn, &insn_sets, 0, MARK_DEST);

	  for (r = 0;
	       r < FIRST_PSEUDO_REGISTER;
	       r++)
	    {
	      if (!TEST_HARD_REG_BIT (insn_sets.regs, r))
		continue;

	      SET_HARD_REG_BIT (*regs_used, r);
	      if (REGNO_REG_SET_P (bb->global_live_at_end, r))
		SET_BIT (*defs_live_exit, inum);
	      if (!insn_sets.memory)
		SET_BIT (du->defs[r], inum);
	      DU_REG_CLASS (du->def_class, r, du->high_bound, inum) = get_reg_class
		(insn, regno_first_use_in (r, PATTERN (insn)),
		 DESTINATION, NO_REGS);
	    }

	  CLEAR_RESOURCE (&insn_res);
	  mark_referenced_resources (insn, &insn_res, 0);

	  for (r = 0;
	       r < FIRST_PSEUDO_REGISTER;
	       r++)
	    {
	      if (!TEST_HARD_REG_BIT (insn_res.regs, r))
		continue;

	      SET_HARD_REG_BIT (*regs_used, r);
	      SET_BIT (du->uses[r], inum);
	      DU_REG_CLASS (du->use_class, r, du->high_bound, inum) = get_reg_class
		(insn, regno_use_in (r, PATTERN (insn)),
		 SOURCE, NO_REGS);
	    }
	}
    }
  free_resource_info ();
}

/* Return nonzero if regno AVAIL_REG can replace REG_DEF for insns in UID_RUID
   starting at insn DEF in def/use chain DU. */

static int
replace_reg_in_block (du, uid_ruid, def, reg_def, avail_reg)
     def_uses *du;
     varray_type *uid_ruid;
     int def;
     rtx reg_def;
     int avail_reg;
{
  int du_idx, status = 1;
  int r = REGNO (reg_def);
  rtx death_note;
  rtx new_reg = gen_rtx_REG (GET_MODE (reg_def), avail_reg);


  rr_replace_reg (PATTERN (VARRAY_RTX (*uid_ruid, def)), reg_def,
		  new_reg, DESTINATION, VARRAY_RTX (*uid_ruid, def),
		  &status);
  if (!status)
    return status;

  death_note = find_reg_note (VARRAY_RTX (*uid_ruid, def), REG_DEAD, reg_def);
  if (!death_note)
    death_note = find_reg_note (VARRAY_RTX (*uid_ruid, def), REG_UNUSED, reg_def);
  if (death_note)
    rr_replace_reg (death_note, reg_def, new_reg, 0,
		    VARRAY_RTX (*uid_ruid, def), &status);

  for (du_idx = def + 1; du_idx < du->high_bound; du_idx++)
    {
      rtx reg_use;
      rtx new_reg;
      if (GET_RTX_CLASS (GET_CODE (VARRAY_RTX (*uid_ruid, du_idx))) != 'i')
	continue;
      reg_use = regno_use_in (r, PATTERN (VARRAY_RTX (*uid_ruid, du_idx)));

      if (reg_use && TEST_BIT (du->uses[r], du_idx))
	{
	  new_reg = gen_rtx_REG (GET_MODE (reg_use), avail_reg);
	  rr_replace_reg (PATTERN (VARRAY_RTX (*uid_ruid, du_idx)), reg_use,
			  new_reg, SOURCE, VARRAY_RTX (*uid_ruid, du_idx),
			  &status);
	  death_note = find_reg_note (VARRAY_RTX (*uid_ruid, du_idx),
				      REG_DEAD, reg_use);
	  if (!death_note)
	    death_note = find_reg_note (VARRAY_RTX (*uid_ruid, du_idx),
					REG_UNUSED, reg_use);
	  if (death_note)
	    rr_replace_reg (death_note, reg_use, new_reg, 0,
			    VARRAY_RTX (*uid_ruid, def), &status);
	  SET_BIT (du->uses[avail_reg], du_idx);
	  RESET_BIT (du->uses[r], du_idx);
	  if (!status)
	    return status;
	}
      if (TEST_BIT (du->defs[r], du_idx))
	break;
    }
  return status;
}

/* Try to replace REG_USE in X with REG_SUB if INSN has a REPLACE_TYPE.
   STATUS is zero if the resulting pattern is not valid. */

static rtx
rr_replace_reg (x, reg_use, reg_sub, replace_type, insn, status)
     rtx x;
     rtx reg_use;
     rtx reg_sub;
     int replace_type;
     rtx insn;
     int *status;
{
  enum rtx_code code;
  int i;
  const char *fmt;
  int n;

  if (x == 0)
    return x;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
      if (REGNO (x) == REGNO (reg_use))
	{
	  if (GET_MODE (x) == GET_MODE (reg_use))
	    return reg_sub;
	  else
	    return gen_rtx_REG (GET_MODE (x), REGNO (reg_use));
	}
      return x;

    case SET:
      if (replace_type == DESTINATION)
	SET_DEST (x) = rr_replace_reg (SET_DEST (x), reg_use, reg_sub,
				       replace_type, insn, status);
      else if (replace_type == SOURCE)
	{
	  int dest_subregno;

	  if (GET_CODE (SET_DEST (x)) == SUBREG)
	    dest_subregno = REGNO (XEXP (SET_DEST (x), 0));
	  else
	    dest_subregno = 0;

	  SET_SRC (x) = rr_replace_reg (SET_SRC (x), reg_use, reg_sub,
					replace_type, insn, status);
	  /* If the replacement register is not part of the source
	     then it may be part of a source mem operand. */
	  if (GET_CODE (SET_DEST (x)) == MEM
	      || GET_CODE (SET_DEST (x)) == ZERO_EXTRACT
	      || GET_CODE (SET_DEST (x)) == SIGN_EXTRACT
	      || GET_CODE (SET_DEST (x)) == STRICT_LOW_PART)
	    SET_DEST (x) = rr_replace_reg (SET_DEST (x), reg_use, reg_sub,
					   replace_type, insn, status);
	  /* shared rtl sanity check */
	  if (dest_subregno
	      && dest_subregno != REGNO (XEXP (SET_DEST (x), 0)))
	    {
	      *status = 0;
	      return x;
	    }
	}

      n = recog_memoized (insn);
      if (n >= 0)
	{
	  int id;
	  extract_insn (insn);

	  /* Any MATCH_DUP's which are REGs must still match */
	  for (id = insn_data[n].n_dups - 1; id >= 0; id--)
	    {
	      int opno = recog_data.dup_num[id];
	      if (GET_CODE (*recog_data.dup_loc[id]) == REG
		  && GET_CODE (*recog_data.operand_loc[opno]) == REG
		  && (REGNO (*recog_data.dup_loc[id]) !=
		      REGNO (*recog_data.operand_loc[opno])))
		*status = 0;
	    }

	  if (!constrain_operands (1))
	    {
	      *status = 0;
	      validate_replace_rtx (reg_sub, reg_use, insn);
	    }
	}
      else
	*status = 0;

      return x;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = rr_replace_reg (XEXP (x, i), reg_use, reg_sub,
				      replace_type, insn, status);
      if (fmt[i] == 'E')
	{
	  register int xv;
	  for (xv = 0; xv < XVECLEN (x, i); xv++)
	    {
	      XVECEXP (x, i, xv) = rr_replace_reg (XVECEXP (x, i, xv), reg_use,
						reg_sub, replace_type, insn,
						   status);
	      n = recog_memoized (insn);
	      if (n >= 0)
		{
		  extract_insn (insn);
		  if (!constrain_operands (1))
		    {
		      *status = 0;
		      validate_replace_rtx (reg_sub, reg_use, insn);
		    }
		}
	      else
		*status = 0;
	    }
	}
    }
  return x;
}

/* Can REGNO in INSN be considered for renaming, given def INUM in d/u
   chain DU? */

static int
consider_def (insn, regno, du, inum)
     rtx insn;
     int regno;
     def_uses *du;
     int inum;
{
  /* Don't rename windowed registers across a call */
#ifdef INCOMING_REGNO
  if (TEST_BIT (du->require_call_save_reg, inum)
      && INCOMING_REGNO (regno) != regno)
    return 0;
#endif

  /* Don't consider conditional moves.  Predicate architectures may
     use two complementary conditional moves and the regno shouldn't change */
  if (condmove_p (insn))
    return 0;

  /* Don't rename call used registers across a call */
  if (!(GET_CODE (insn) == CALL_INSN
	&& TEST_HARD_REG_BIT (call_used_reg_set, regno)))
    return 1;
  else
    return 0;
}

/* Can the use of REGNO in INSN of block USE_BLOCK be considered for renaming
   for a def in def_block? */

static int
consider_use (insn, regno, def_block, use_block)
     rtx insn;
     int regno;
     int def_block;
     int use_block;
{
  rtx reg_use;
  edge e;
  basic_block ub = BASIC_BLOCK (use_block);

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  /* If a use's basic block is different than the def's basic block, 
     then insure another predecessor does not also define this register */
  if (def_block != use_block)
    for (e = ub->pred; e; e = e->pred_next)
      {
	if (e->src->index != def_block
	    && e->src->index != -1
	    && REGNO_REG_SET_P (BASIC_BLOCK (e->src->index)->global_live_at_end, regno))
	  return 0;
      }

  /* Don't consider conditional moves.  Predicate architectures may
     use two complementary conditional moves and the regno shouldn't change */

  if (condmove_p (insn))
    return 0;

  reg_use = regno_first_use_in (regno, PATTERN (insn));
  if (reg_use)
    {
      /* Don't consider multi-reg values. */
      if (HARD_REGNO_NREGS (regno, GET_MODE (reg_use)) != 1
	  && GET_MODE (reg_use) != CCmode)
	return 0;

      /* Don't consider register if the only use is in a USE */
      if (reg_mentioned_p (gen_rtx_USE (VOIDmode, reg_use),
			   PATTERN (insn)))
	return 0;
      else
	return 1;
    }
  else
    return 0;
}

/* Can REG_USE be replaced by regno AVAIL_REG if it is in AVAIL_REGS
   and it is in regclass RC, given insn INUM of def/use chain DU? */

static int
consider_available (reg_use, avail_reg, avail_regs, rc, du, inum)
     rtx reg_use;
     int avail_reg;
     HARD_REG_SET *avail_regs;
     int rc;
     def_uses *du;
     int inum;
{
  if (!TEST_HARD_REG_BIT (*avail_regs, avail_reg))
    return 0;

  if (fixed_regs[avail_reg])
    return 0;

#ifdef HARD_REGNO_RENAME_OK
  if (!HARD_REGNO_RENAME_OK (REGNO (reg_use), avail_reg))
    return 0;
#endif

  /* Don't consider windowed leaf registers which will be renamed by
     leaf_renumber_regs */
#ifdef LEAF_REG_REMAP
  if (current_function_uses_only_leaf_regs)
    if (LEAF_REG_REMAP (avail_reg) < 0)
      return 0;
#endif

  /* A register is considered available if it is available at the beginning of
     the basic block.  We may want to refine this to when a register becomes
     available within the block.  We don't consider multi-reg values. */
  /* ??? Consider a representation that would allow multi-reg support? */
  if (!TEST_HARD_REG_BIT (reg_class_contents[(enum reg_class) rc], avail_reg)
      || !HARD_REGNO_MODE_OK (avail_reg, GET_MODE (reg_use))
      || (HARD_REGNO_NREGS (avail_reg, GET_MODE (reg_use)) != 1
	  && GET_MODE (reg_use) != CCmode)
      || (call_fixed_regs[avail_reg]
#ifdef HARD_REGNO_RENAME_OK
	  && !HARD_REGNO_RENAME_OK (REGNO (reg_use), avail_reg)
#endif
      )
      || (TEST_BIT (du->require_call_save_reg, inum)
	  && (call_used_regs[avail_reg] || call_used_regs[REGNO (reg_use)]
	  )))
    return 0;

  /* If register is a callee-saved register it must be saved in the frame. 
     call saved registers can not be added to regs_ever_live after reload,
     as it would invalidate most elimination offsets */
  if (regs_ever_live[avail_reg] || call_used_regs[avail_reg])
    return 1;

  return 0;
}

/* Return 1 if INSN is a conditional move */

static int
condmove_p (insn)
     rtx insn;
{
  if (GET_CODE (insn) == INSN
      && GET_CODE (PATTERN (insn)) == SET
      && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE)
    return 1;
  return 0;
}

/* Searches X for the first reference to REGNO, returning the rtx of the
   reference found if any.  Otherwise, returns NULL_RTX.  */

static rtx
regno_first_use_in (regno, x)
     int regno;
     rtx x;
{
  register const char *fmt;
  int i, j;
  rtx tem;

  if (GET_CODE (x) == REG && REGNO (x) == regno)
    return x;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = 0; i <= GET_RTX_LENGTH (GET_CODE (x)) - 1; i++)
    {
      if (fmt[i] == 'e')
	{
	  if ((tem = regno_first_use_in (regno, XEXP (x, i))))
	    return tem;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  if ((tem = regno_first_use_in (regno, XVECEXP (x, i, j))))
	    return tem;
    }

  return NULL_RTX;
}

/* Dump def/use chain DU to RTL_DUMP_FILE, given insns in UID_RUID and
   which regs are live at end, GLOBAL_LIVE_AT_END */

static void
dump_def_use_chain (global_live_at_end, du, uid_ruid)
     HARD_REG_SET *global_live_at_end;
     def_uses *du;
     varray_type *uid_ruid;
{
  int r, inum;

  for (r = 0; r < FIRST_PSEUDO_REGISTER; r++)
    {
      int set = 0;
      for (inum = 0;
	   inum <= du->high_bound;
	   inum++)
	{
	  rtx insn = VARRAY_RTX (*uid_ruid, inum);
#if 0
	  if (!insn
	      || GET_RTX_CLASS (GET_CODE
				(insn)) != 'i')
	    continue;
	  reg_use = regno_first_use_in (r, PATTERN (insn));
	  if (!reg_use)
	    continue;
#endif
	  if (!set && (TEST_BIT (du->defs[r], inum)
		       || TEST_BIT (du->uses[r], inum)))
	    {
	      fprintf (rtl_dump_file, "Register %s: ", reg_names[r]);
	      if (fixed_regs[r])
		fprintf (rtl_dump_file, "Fixed ");
	      else if (call_fixed_regs[r])
		fprintf (rtl_dump_file, "Call Fixed ");
	      if (TEST_HARD_REG_BIT (*global_live_at_end, r))
		fprintf (rtl_dump_file, "Live at Exit ");
	      set = 1;
	    }
	  if (TEST_BIT (du->defs[r], inum))
	    fprintf (rtl_dump_file, "=%d ", INSN_UID (insn));
	  if (TEST_BIT (du->uses[r], inum))
	    fprintf (rtl_dump_file, "%d ", INSN_UID (insn));
	}
      if (set)
	fprintf (rtl_dump_file, "\n");
    }
}

/* Dump info for extended basic block EBB having root EB */

static void
dump_ext_bb_info (eb, ebb)
     int eb;
     ext_basic_blocks *ebb;
{
  int b;

  {
    int have_ebb = 0;
    for (b = 0; b < n_basic_blocks; b++)
      {
	if (TEST_BIT (ebb->basic_block[eb], b))
	  {
	    if (!have_ebb)
	      {
#ifndef RENAME_EXTENDED_BLOCKS
		fprintf (rtl_dump_file, "\nBasic block %d: ", b);
#else
		fprintf (rtl_dump_file, "\nExtended basic block %d: ", b);
#endif
		have_ebb = 1;
	      }
	    fprintf (rtl_dump_file, "%d ", b);
	  }
	if (TEST_BIT (ebb->exit[eb], b))
	  fprintf (rtl_dump_file, "(exit) ");
      }
    if (have_ebb)
      fprintf (rtl_dump_file, "\n");
  }
}

/* Initialize EBB with extended basic block info if RENAME_EXTENDED_BLOCKS is
   defined.  Otherwise just use basic blocks */

static void
find_ext_basic_blocks (ebb)
     ext_basic_blocks *ebb;
{
  sbitmap bb_processed;
  int b;

  bb_processed = sbitmap_alloc (n_basic_blocks);
  sbitmap_zero (bb_processed);

#ifndef RENAME_EXTENDED_BLOCKS
  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block bb = BASIC_BLOCK (b);
      SET_BIT (ebb->basic_block[bb->index], bb->index);
    }
#else
  for (b = 0; b < n_basic_blocks; b++)
    {
      basic_block bb = BASIC_BLOCK (b);
      if (!TEST_BIT (bb_processed, b))
	{
	  find_one_ext_basic_block (bb->index, bb, &bb_processed, ebb);
	}
    }
#endif
  sbitmap_free (bb_processed);
}

/* Find one extended basic block EBB having root ENTRY containing block
   BB */

static void
find_one_ext_basic_block (entry, bb, bb_processed, ebb)
     int entry;
     basic_block bb;
     sbitmap *bb_processed;
     ext_basic_blocks *ebb;
{
  edge e;

  if (!TEST_BIT (*bb_processed, bb->index))
    {
      SET_BIT (ebb->basic_block[entry], bb->index);
      SET_BIT (*bb_processed, bb->index);
    }

  for (e = bb->succ; e; e = e->succ_next)
    if (!TEST_BIT (*bb_processed, e->dest->index))
      {
	if (!e->dest->pred->pred_next
	    && (!TEST_BIT (*bb_processed, e->dest->index)))
	  {
	    find_one_ext_basic_block (entry, e->dest, bb_processed, ebb);
	  }
	else
	  {
	    SET_BIT (ebb->exit[entry], bb->index);
	  }
      }
}

/* Find the register class for register REG_USE having TYPE (DESTINATION or
   SOURCE) in INSN.  Use DEFAULT_CLASS if we cannot determine a class. */

static enum reg_class
get_reg_class (insn, reg_use, type, default_class)
     rtx insn;
     rtx reg_use;
     int type;
     enum reg_class default_class;
{
  int alt, id = 0;

  extract_insn (insn);
  constrain_operands (1);
  alt = which_alternative;

  preprocess_constraints ();

  if (type == DESTINATION)
    for (id = 0; id < recog_data.n_operands; id++)
      {
	if (rtx_equal_p (recog_data.operand[id], reg_use))
	  break;
      }
  else if (type == SOURCE)
    for (id = recog_data.n_operands - 1; id >= 0; id--)
      {
	if (rtx_equal_p (recog_data.operand[id], reg_use))
	  break;
      }

  if (id == -1 || id == recog_data.n_operands)
    return default_class;

  return recog_op_alt[id][alt].class;
}
