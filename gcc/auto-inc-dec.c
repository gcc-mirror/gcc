/* Discovery of auto-inc and auto-dec instructions.
   Copyright (C) 2006-2015 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "predict.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "cfgrtl.h"
#include "basic-block.h"
#include "insn-config.h"
#include "regs.h"
#include "flags.h"
#include "except.h"
#include "diagnostic-core.h"
#include "recog.h"
#include "expmed.h"
#include "dojump.h"
#include "explow.h"
#include "calls.h"
#include "emit-rtl.h"
#include "varasm.h"
#include "stmt.h"
#include "expr.h"
#include "tree-pass.h"
#include "df.h"
#include "dbgcnt.h"
#include "target.h"

/* This pass was originally removed from flow.c. However there is
   almost nothing that remains of that code.

   There are (4) basic forms that are matched:

      (1) FORM_PRE_ADD
           a <- b + c
           ...
           *a

        becomes

           a <- b
           ...
           *(a += c) pre


      (2) FORM_PRE_INC
           a += c
           ...
           *a

        becomes

           *(a += c) pre


      (3) FORM_POST_ADD
           *a
           ...
           b <- a + c

	   (For this case to be true, b must not be assigned or used between
	   the *a and the assignment to b.  B must also be a Pmode reg.)

        becomes

           b <- a
           ...
           *(b += c) post


      (4) FORM_POST_INC
           *a
           ...
           a <- a + c

        becomes

           *(a += c) post

  There are three types of values of c.

    1) c is a constant equal to the width of the value being accessed by
       the pointer.  This is useful for machines that have
       HAVE_PRE_INCREMENT, HAVE_POST_INCREMENT, HAVE_PRE_DECREMENT or
       HAVE_POST_DECREMENT defined.

    2) c is a constant not equal to the width of the value being accessed
       by the pointer.  This is useful for machines that have
       HAVE_PRE_MODIFY_DISP, HAVE_POST_MODIFY_DISP defined.

    3) c is a register.  This is useful for machines that have
       HAVE_PRE_MODIFY_REG,  HAVE_POST_MODIFY_REG

  The is one special case: if a already had an offset equal to it +-
  its width and that offset is equal to -c when the increment was
  before the ref or +c if the increment was after the ref, then if we
  can do the combination but switch the pre/post bit.  */

#ifdef AUTO_INC_DEC

enum form
{
  FORM_PRE_ADD,
  FORM_PRE_INC,
  FORM_POST_ADD,
  FORM_POST_INC,
  FORM_last
};

/* The states of the second operands of mem refs and inc insns.  If no
   second operand of the mem_ref was found, it is assumed to just be
   ZERO.  SIZE is the size of the mode accessed in the memref.  The
   ANY is used for constants that are not +-size or 0.  REG is used if
   the forms are reg1 + reg2.  */

enum inc_state
{
  INC_ZERO,           /* == 0  */
  INC_NEG_SIZE,       /* == +size  */
  INC_POS_SIZE,       /* == -size */
  INC_NEG_ANY,        /* == some -constant  */
  INC_POS_ANY,        /* == some +constant  */
  INC_REG,            /* == some register  */
  INC_last
};

/* The eight forms that pre/post inc/dec can take.  */
enum gen_form
{
  NOTHING,
  SIMPLE_PRE_INC,     /* ++size  */
  SIMPLE_POST_INC,    /* size++  */
  SIMPLE_PRE_DEC,     /* --size  */
  SIMPLE_POST_DEC,    /* size--  */
  DISP_PRE,           /* ++con   */
  DISP_POST,          /* con++   */
  REG_PRE,            /* ++reg   */
  REG_POST            /* reg++   */
};

/* Tmp mem rtx for use in cost modeling.  */
static rtx mem_tmp;

static enum inc_state
set_inc_state (HOST_WIDE_INT val, int size)
{
  if (val == 0)
    return INC_ZERO;
  if (val < 0)
    return (val == -size) ? INC_NEG_SIZE : INC_NEG_ANY;
  else
    return (val == size) ? INC_POS_SIZE : INC_POS_ANY;
}

/* The DECISION_TABLE that describes what form, if any, the increment
   or decrement will take. It is a three dimensional table.  The first
   index is the type of constant or register found as the second
   operand of the inc insn.  The second index is the type of constant
   or register found as the second operand of the memory reference (if
   no second operand exists, 0 is used).  The third index is the form
   and location (relative to the mem reference) of inc insn.  */

static bool initialized = false;
static enum gen_form decision_table[INC_last][INC_last][FORM_last];

static void
init_decision_table (void)
{
  enum gen_form value;

  if (HAVE_PRE_INCREMENT || HAVE_PRE_MODIFY_DISP)
    {
      /* Prefer the simple form if both are available.  */
      value = (HAVE_PRE_INCREMENT) ? SIMPLE_PRE_INC : DISP_PRE;

      decision_table[INC_POS_SIZE][INC_ZERO][FORM_PRE_ADD] = value;
      decision_table[INC_POS_SIZE][INC_ZERO][FORM_PRE_INC] = value;

      decision_table[INC_POS_SIZE][INC_POS_SIZE][FORM_POST_ADD] = value;
      decision_table[INC_POS_SIZE][INC_POS_SIZE][FORM_POST_INC] = value;
    }

  if (HAVE_POST_INCREMENT || HAVE_POST_MODIFY_DISP)
    {
      /* Prefer the simple form if both are available.  */
      value = (HAVE_POST_INCREMENT) ? SIMPLE_POST_INC : DISP_POST;

      decision_table[INC_POS_SIZE][INC_ZERO][FORM_POST_ADD] = value;
      decision_table[INC_POS_SIZE][INC_ZERO][FORM_POST_INC] = value;

      decision_table[INC_POS_SIZE][INC_NEG_SIZE][FORM_PRE_ADD] = value;
      decision_table[INC_POS_SIZE][INC_NEG_SIZE][FORM_PRE_INC] = value;
    }

  if (HAVE_PRE_DECREMENT || HAVE_PRE_MODIFY_DISP)
    {
      /* Prefer the simple form if both are available.  */
      value = (HAVE_PRE_DECREMENT) ? SIMPLE_PRE_DEC : DISP_PRE;

      decision_table[INC_NEG_SIZE][INC_ZERO][FORM_PRE_ADD] = value;
      decision_table[INC_NEG_SIZE][INC_ZERO][FORM_PRE_INC] = value;

      decision_table[INC_NEG_SIZE][INC_NEG_SIZE][FORM_POST_ADD] = value;
      decision_table[INC_NEG_SIZE][INC_NEG_SIZE][FORM_POST_INC] = value;
    }

  if (HAVE_POST_DECREMENT || HAVE_POST_MODIFY_DISP)
    {
      /* Prefer the simple form if both are available.  */
      value = (HAVE_POST_DECREMENT) ? SIMPLE_POST_DEC : DISP_POST;

      decision_table[INC_NEG_SIZE][INC_ZERO][FORM_POST_ADD] = value;
      decision_table[INC_NEG_SIZE][INC_ZERO][FORM_POST_INC] = value;

      decision_table[INC_NEG_SIZE][INC_POS_SIZE][FORM_PRE_ADD] = value;
      decision_table[INC_NEG_SIZE][INC_POS_SIZE][FORM_PRE_INC] = value;
    }

  if (HAVE_PRE_MODIFY_DISP)
    {
      decision_table[INC_POS_ANY][INC_ZERO][FORM_PRE_ADD] = DISP_PRE;
      decision_table[INC_POS_ANY][INC_ZERO][FORM_PRE_INC] = DISP_PRE;

      decision_table[INC_POS_ANY][INC_POS_ANY][FORM_POST_ADD] = DISP_PRE;
      decision_table[INC_POS_ANY][INC_POS_ANY][FORM_POST_INC] = DISP_PRE;

      decision_table[INC_NEG_ANY][INC_ZERO][FORM_PRE_ADD] = DISP_PRE;
      decision_table[INC_NEG_ANY][INC_ZERO][FORM_PRE_INC] = DISP_PRE;

      decision_table[INC_NEG_ANY][INC_NEG_ANY][FORM_POST_ADD] = DISP_PRE;
      decision_table[INC_NEG_ANY][INC_NEG_ANY][FORM_POST_INC] = DISP_PRE;
    }

  if (HAVE_POST_MODIFY_DISP)
    {
      decision_table[INC_POS_ANY][INC_ZERO][FORM_POST_ADD] = DISP_POST;
      decision_table[INC_POS_ANY][INC_ZERO][FORM_POST_INC] = DISP_POST;

      decision_table[INC_POS_ANY][INC_NEG_ANY][FORM_PRE_ADD] = DISP_POST;
      decision_table[INC_POS_ANY][INC_NEG_ANY][FORM_PRE_INC] = DISP_POST;

      decision_table[INC_NEG_ANY][INC_ZERO][FORM_POST_ADD] = DISP_POST;
      decision_table[INC_NEG_ANY][INC_ZERO][FORM_POST_INC] = DISP_POST;

      decision_table[INC_NEG_ANY][INC_POS_ANY][FORM_PRE_ADD] = DISP_POST;
      decision_table[INC_NEG_ANY][INC_POS_ANY][FORM_PRE_INC] = DISP_POST;
    }

  /* This is much simpler than the other cases because we do not look
     for the reg1-reg2 case.  Note that we do not have a INC_POS_REG
     and INC_NEG_REG states.  Most of the use of such states would be
     on a target that had an R1 - R2 update address form.

     There is the remote possibility that you could also catch a = a +
     b; *(a - b) as a postdecrement of (a + b).  However, it is
     unclear if *(a - b) would ever be generated on a machine that did
     not have that kind of addressing mode.  The IA-64 and RS6000 will
     not do this, and I cannot speak for any other.  If any
     architecture does have an a-b update for, these cases should be
     added.  */
  if (HAVE_PRE_MODIFY_REG)
    {
      decision_table[INC_REG][INC_ZERO][FORM_PRE_ADD] = REG_PRE;
      decision_table[INC_REG][INC_ZERO][FORM_PRE_INC] = REG_PRE;

      decision_table[INC_REG][INC_REG][FORM_POST_ADD] = REG_PRE;
      decision_table[INC_REG][INC_REG][FORM_POST_INC] = REG_PRE;
    }

  if (HAVE_POST_MODIFY_REG)
    {
      decision_table[INC_REG][INC_ZERO][FORM_POST_ADD] = REG_POST;
      decision_table[INC_REG][INC_ZERO][FORM_POST_INC] = REG_POST;
    }

  initialized = true;
}

/* Parsed fields of an inc insn of the form "reg_res = reg0+reg1" or
   "reg_res = reg0+c".  */

static struct inc_insn
{
  rtx_insn *insn;     /* The insn being parsed.  */
  rtx pat;            /* The pattern of the insn.  */
  bool reg1_is_const; /* True if reg1 is const, false if reg1 is a reg.  */
  enum form form;
  rtx reg_res;
  rtx reg0;
  rtx reg1;
  enum inc_state reg1_state;/* The form of the const if reg1 is a const.  */
  HOST_WIDE_INT reg1_val;/* Value if reg1 is const.  */
} inc_insn;


/* Dump the parsed inc insn to FILE.  */

static void
dump_inc_insn (FILE *file)
{
  const char *f = ((inc_insn.form == FORM_PRE_ADD)
	      || (inc_insn.form == FORM_PRE_INC)) ? "pre" : "post";

  dump_insn_slim (file, inc_insn.insn);

  switch (inc_insn.form)
    {
    case FORM_PRE_ADD:
    case FORM_POST_ADD:
      if (inc_insn.reg1_is_const)
	fprintf (file, "found %s add(%d) r[%d]=r[%d]+%d\n",
		 f, INSN_UID (inc_insn.insn),
		 REGNO (inc_insn.reg_res),
		 REGNO (inc_insn.reg0), (int) inc_insn.reg1_val);
      else
	fprintf (file, "found %s add(%d) r[%d]=r[%d]+r[%d]\n",
		 f, INSN_UID (inc_insn.insn),
		 REGNO (inc_insn.reg_res),
		 REGNO (inc_insn.reg0), REGNO (inc_insn.reg1));
      break;

    case FORM_PRE_INC:
    case FORM_POST_INC:
      if (inc_insn.reg1_is_const)
	fprintf (file, "found %s inc(%d) r[%d]+=%d\n",
		 f, INSN_UID (inc_insn.insn),
		 REGNO (inc_insn.reg_res), (int) inc_insn.reg1_val);
      else
	fprintf (file, "found %s inc(%d) r[%d]+=r[%d]\n",
		 f, INSN_UID (inc_insn.insn),
		 REGNO (inc_insn.reg_res), REGNO (inc_insn.reg1));
      break;

    default:
      break;
    }
}


/* Parsed fields of a mem ref of the form "*(reg0+reg1)" or "*(reg0+c)".  */

static struct mem_insn
{
  rtx_insn *insn;     /* The insn being parsed.  */
  rtx pat;            /* The pattern of the insn.  */
  rtx *mem_loc;       /* The address of the field that holds the mem */
                      /* that is to be replaced.  */
  bool reg1_is_const; /* True if reg1 is const, false if reg1 is a reg.  */
  rtx reg0;
  rtx reg1;           /* This is either a reg or a const depending on
			 reg1_is_const.  */
  enum inc_state reg1_state;/* The form of the const if reg1 is a const.  */
  HOST_WIDE_INT reg1_val;/* Value if reg1 is const.  */
} mem_insn;


/* Dump the parsed mem insn to FILE.  */

static void
dump_mem_insn (FILE *file)
{
  dump_insn_slim (file, mem_insn.insn);

  if (mem_insn.reg1_is_const)
    fprintf (file, "found mem(%d) *(r[%d]+%d)\n",
	     INSN_UID (mem_insn.insn),
	     REGNO (mem_insn.reg0), (int) mem_insn.reg1_val);
  else
    fprintf (file, "found mem(%d) *(r[%d]+r[%d])\n",
	     INSN_UID (mem_insn.insn),
	     REGNO (mem_insn.reg0), REGNO (mem_insn.reg1));
}


/* The following three arrays contain pointers to instructions. They
   are indexed by REGNO.  At any point in the basic block where we are
   looking these three arrays contain, respectively, the next insn
   that uses REGNO, the next inc or add insn that uses REGNO and the
   next insn that sets REGNO.

   The arrays are not cleared when we move from block to block so
   whenever an insn is retrieved from these arrays, it's block number
   must be compared with the current block.
*/

static rtx_insn **reg_next_use = NULL;
static rtx_insn **reg_next_inc_use = NULL;
static rtx_insn **reg_next_def = NULL;


/* Move dead note that match PATTERN to TO_INSN from FROM_INSN.  We do
   not really care about moving any other notes from the inc or add
   insn.  Moving the REG_EQUAL and REG_EQUIV is clearly wrong and it
   does not appear that there are any other kinds of relevant notes.  */

static void
move_dead_notes (rtx_insn *to_insn, rtx_insn *from_insn, rtx pattern)
{
  rtx note;
  rtx next_note;
  rtx prev_note = NULL;

  for (note = REG_NOTES (from_insn); note; note = next_note)
    {
      next_note = XEXP (note, 1);

      if ((REG_NOTE_KIND (note) == REG_DEAD)
	  && pattern == XEXP (note, 0))
	{
	  XEXP (note, 1) = REG_NOTES (to_insn);
	  REG_NOTES (to_insn) = note;
	  if (prev_note)
	    XEXP (prev_note, 1) = next_note;
	  else
	    REG_NOTES (from_insn) = next_note;
	}
      else prev_note = note;
    }
}


/* Create a mov insn DEST_REG <- SRC_REG and insert it before
   NEXT_INSN.  */

static rtx_insn *
insert_move_insn_before (rtx_insn *next_insn, rtx dest_reg, rtx src_reg)
{
  rtx_insn *insns;

  start_sequence ();
  emit_move_insn (dest_reg, src_reg);
  insns = get_insns ();
  end_sequence ();
  emit_insn_before (insns, next_insn);
  return insns;
}


/* Change mem_insn.mem_loc so that uses NEW_ADDR which has an
   increment of INC_REG.  To have reached this point, the change is a
   legitimate one from a dataflow point of view.  The only questions
   are is this a valid change to the instruction and is this a
   profitable change to the instruction.  */

static bool
attempt_change (rtx new_addr, rtx inc_reg)
{
  /* There are four cases: For the two cases that involve an add
     instruction, we are going to have to delete the add and insert a
     mov.  We are going to assume that the mov is free.  This is
     fairly early in the backend and there are a lot of opportunities
     for removing that move later.  In particular, there is the case
     where the move may be dead, this is what dead code elimination
     passes are for.  The two cases where we have an inc insn will be
     handled mov free.  */

  basic_block bb = BLOCK_FOR_INSN (mem_insn.insn);
  rtx_insn *mov_insn = NULL;
  int regno;
  rtx mem = *mem_insn.mem_loc;
  machine_mode mode = GET_MODE (mem);
  rtx new_mem;
  int old_cost = 0;
  int new_cost = 0;
  bool speed = optimize_bb_for_speed_p (bb);

  PUT_MODE (mem_tmp, mode);
  XEXP (mem_tmp, 0) = new_addr;

  old_cost = (set_src_cost (mem, speed)
	      + set_rtx_cost (PATTERN (inc_insn.insn), speed));
  new_cost = set_src_cost (mem_tmp, speed);

  /* The first item of business is to see if this is profitable.  */
  if (old_cost < new_cost)
    {
      if (dump_file)
	fprintf (dump_file, "cost failure old=%d new=%d\n", old_cost, new_cost);
      return false;
    }

  /* Jump through a lot of hoops to keep the attributes up to date.  We
     do not want to call one of the change address variants that take
     an offset even though we know the offset in many cases.  These
     assume you are changing where the address is pointing by the
     offset.  */
  new_mem = replace_equiv_address_nv (mem, new_addr);
  if (! validate_change (mem_insn.insn, mem_insn.mem_loc, new_mem, 0))
    {
      if (dump_file)
	fprintf (dump_file, "validation failure\n");
      return false;
    }

  /* From here to the end of the function we are committed to the
     change, i.e. nothing fails.  Generate any necessary movs, move
     any regnotes, and fix up the reg_next_{use,inc_use,def}.  */
  switch (inc_insn.form)
    {
    case FORM_PRE_ADD:
      /* Replace the addition with a move.  Do it at the location of
	 the addition since the operand of the addition may change
	 before the memory reference.  */
      mov_insn = insert_move_insn_before (inc_insn.insn,
					  inc_insn.reg_res, inc_insn.reg0);
      move_dead_notes (mov_insn, inc_insn.insn, inc_insn.reg0);

      regno = REGNO (inc_insn.reg_res);
      reg_next_def[regno] = mov_insn;
      reg_next_use[regno] = NULL;
      regno = REGNO (inc_insn.reg0);
      reg_next_use[regno] = mov_insn;
      df_recompute_luids (bb);
      break;

    case FORM_POST_INC:
      regno = REGNO (inc_insn.reg_res);
      if (reg_next_use[regno] == reg_next_inc_use[regno])
	reg_next_inc_use[regno] = NULL;

      /* Fallthru.  */
    case FORM_PRE_INC:
      regno = REGNO (inc_insn.reg_res);
      reg_next_def[regno] = mem_insn.insn;
      reg_next_use[regno] = NULL;

      break;

    case FORM_POST_ADD:
      mov_insn = insert_move_insn_before (mem_insn.insn,
					  inc_insn.reg_res, inc_insn.reg0);
      move_dead_notes (mov_insn, inc_insn.insn, inc_insn.reg0);

      /* Do not move anything to the mov insn because the instruction
	 pointer for the main iteration has not yet hit that.  It is
	 still pointing to the mem insn. */
      regno = REGNO (inc_insn.reg_res);
      reg_next_def[regno] = mem_insn.insn;
      reg_next_use[regno] = NULL;

      regno = REGNO (inc_insn.reg0);
      reg_next_use[regno] = mem_insn.insn;
      if ((reg_next_use[regno] == reg_next_inc_use[regno])
	  || (reg_next_inc_use[regno] == inc_insn.insn))
	reg_next_inc_use[regno] = NULL;
      df_recompute_luids (bb);
      break;

    case FORM_last:
    default:
      gcc_unreachable ();
    }

  if (!inc_insn.reg1_is_const)
    {
      regno = REGNO (inc_insn.reg1);
      reg_next_use[regno] = mem_insn.insn;
      if ((reg_next_use[regno] == reg_next_inc_use[regno])
	  || (reg_next_inc_use[regno] == inc_insn.insn))
	reg_next_inc_use[regno] = NULL;
    }

  delete_insn (inc_insn.insn);

  if (dump_file && mov_insn)
    {
      fprintf (dump_file, "inserting mov ");
      dump_insn_slim (dump_file, mov_insn);
    }

  /* Record that this insn has an implicit side effect.  */
  add_reg_note (mem_insn.insn, REG_INC, inc_reg);

  if (dump_file)
    {
      fprintf (dump_file, "****success ");
      dump_insn_slim (dump_file, mem_insn.insn);
    }

  return true;
}


/* Try to combine the instruction in INC_INSN with the instruction in
   MEM_INSN.  First the form is determined using the DECISION_TABLE
   and the results of parsing the INC_INSN and the MEM_INSN.
   Assuming the form is ok, a prototype new address is built which is
   passed to ATTEMPT_CHANGE for final processing.  */

static bool
try_merge (void)
{
  enum gen_form gen_form;
  rtx mem = *mem_insn.mem_loc;
  rtx inc_reg = inc_insn.form == FORM_POST_ADD ?
    inc_insn.reg_res : mem_insn.reg0;

  /* The width of the mem being accessed.  */
  int size = GET_MODE_SIZE (GET_MODE (mem));
  rtx_insn *last_insn = NULL;
  machine_mode reg_mode = GET_MODE (inc_reg);

  switch (inc_insn.form)
    {
    case FORM_PRE_ADD:
    case FORM_PRE_INC:
      last_insn = mem_insn.insn;
      break;
    case FORM_POST_INC:
    case FORM_POST_ADD:
      last_insn = inc_insn.insn;
      break;
    case FORM_last:
    default:
      gcc_unreachable ();
    }

  /* Cannot handle auto inc of the stack.  */
  if (inc_reg == stack_pointer_rtx)
    {
      if (dump_file)
	fprintf (dump_file, "cannot inc stack %d failure\n", REGNO (inc_reg));
      return false;
    }

  /* Look to see if the inc register is dead after the memory
     reference.  If it is, do not do the combination.  */
  if (find_regno_note (last_insn, REG_DEAD, REGNO (inc_reg)))
    {
      if (dump_file)
	fprintf (dump_file, "dead failure %d\n", REGNO (inc_reg));
      return false;
    }

  mem_insn.reg1_state = (mem_insn.reg1_is_const)
    ? set_inc_state (mem_insn.reg1_val, size) : INC_REG;
  inc_insn.reg1_state = (inc_insn.reg1_is_const)
    ? set_inc_state (inc_insn.reg1_val, size) : INC_REG;

  /* Now get the form that we are generating.  */
  gen_form = decision_table
    [inc_insn.reg1_state][mem_insn.reg1_state][inc_insn.form];

  if (dbg_cnt (auto_inc_dec) == false)
    return false;

  switch (gen_form)
    {
    default:
    case NOTHING:
      return false;

    case SIMPLE_PRE_INC:     /* ++size  */
      if (dump_file)
	fprintf (dump_file, "trying SIMPLE_PRE_INC\n");
      return attempt_change (gen_rtx_PRE_INC (reg_mode, inc_reg), inc_reg);
      break;

    case SIMPLE_POST_INC:    /* size++  */
      if (dump_file)
	fprintf (dump_file, "trying SIMPLE_POST_INC\n");
      return attempt_change (gen_rtx_POST_INC (reg_mode, inc_reg), inc_reg);
      break;

    case SIMPLE_PRE_DEC:     /* --size  */
      if (dump_file)
	fprintf (dump_file, "trying SIMPLE_PRE_DEC\n");
      return attempt_change (gen_rtx_PRE_DEC (reg_mode, inc_reg), inc_reg);
      break;

    case SIMPLE_POST_DEC:    /* size--  */
      if (dump_file)
	fprintf (dump_file, "trying SIMPLE_POST_DEC\n");
      return attempt_change (gen_rtx_POST_DEC (reg_mode, inc_reg), inc_reg);
      break;

    case DISP_PRE:           /* ++con   */
      if (dump_file)
	fprintf (dump_file, "trying DISP_PRE\n");
      return attempt_change (gen_rtx_PRE_MODIFY (reg_mode,
						 inc_reg,
						 gen_rtx_PLUS (reg_mode,
							       inc_reg,
							       inc_insn.reg1)),
			     inc_reg);
      break;

    case DISP_POST:          /* con++   */
      if (dump_file)
	fprintf (dump_file, "trying POST_DISP\n");
      return attempt_change (gen_rtx_POST_MODIFY (reg_mode,
						  inc_reg,
						  gen_rtx_PLUS (reg_mode,
								inc_reg,
								inc_insn.reg1)),
			     inc_reg);
      break;

    case REG_PRE:            /* ++reg   */
      if (dump_file)
	fprintf (dump_file, "trying PRE_REG\n");
      return attempt_change (gen_rtx_PRE_MODIFY (reg_mode,
						 inc_reg,
						 gen_rtx_PLUS (reg_mode,
							       inc_reg,
							       inc_insn.reg1)),
			     inc_reg);
      break;

    case REG_POST:            /* reg++   */
      if (dump_file)
	fprintf (dump_file, "trying POST_REG\n");
      return attempt_change (gen_rtx_POST_MODIFY (reg_mode,
						  inc_reg,
						  gen_rtx_PLUS (reg_mode,
								inc_reg,
								inc_insn.reg1)),
			     inc_reg);
      break;
    }
}

/* Return the next insn that uses (if reg_next_use is passed in
   NEXT_ARRAY) or defines (if reg_next_def is passed in NEXT_ARRAY)
   REGNO in BB.  */

static rtx_insn *
get_next_ref (int regno, basic_block bb, rtx_insn **next_array)
{
  rtx_insn *insn = next_array[regno];

  /* Lazy about cleaning out the next_arrays.  */
  if (insn && BLOCK_FOR_INSN (insn) != bb)
    {
      next_array[regno] = NULL;
      insn = NULL;
    }

  return insn;
}


/* Return true if INSN is of a form "a = b op c" where a and b are
   regs.  op is + if c is a reg and +|- if c is a const.  Fill in
   INC_INSN with what is found.

   This function is called in two contexts, if BEFORE_MEM is true,
   this is called for each insn in the basic block.  If BEFORE_MEM is
   false, it is called for the instruction in the block that uses the
   index register for some memory reference that is currently being
   processed.  */

static bool
parse_add_or_inc (rtx_insn *insn, bool before_mem)
{
  rtx pat = single_set (insn);
  if (!pat)
    return false;

  /* Result must be single reg.  */
  if (!REG_P (SET_DEST (pat)))
    return false;

  if ((GET_CODE (SET_SRC (pat)) != PLUS)
      && (GET_CODE (SET_SRC (pat)) != MINUS))
    return false;

  if (!REG_P (XEXP (SET_SRC (pat), 0)))
    return false;

  inc_insn.insn = insn;
  inc_insn.pat = pat;
  inc_insn.reg_res = SET_DEST (pat);
  inc_insn.reg0 = XEXP (SET_SRC (pat), 0);
  if (rtx_equal_p (inc_insn.reg_res, inc_insn.reg0))
    inc_insn.form = before_mem ? FORM_PRE_INC : FORM_POST_INC;
  else
    inc_insn.form = before_mem ? FORM_PRE_ADD : FORM_POST_ADD;

  if (CONST_INT_P (XEXP (SET_SRC (pat), 1)))
    {
      /* Process a = b + c where c is a const.  */
      inc_insn.reg1_is_const = true;
      if (GET_CODE (SET_SRC (pat)) == PLUS)
	{
	  inc_insn.reg1 = XEXP (SET_SRC (pat), 1);
	  inc_insn.reg1_val = INTVAL (inc_insn.reg1);
	}
      else
	{
	  inc_insn.reg1_val = -INTVAL (XEXP (SET_SRC (pat), 1));
	  inc_insn.reg1 = GEN_INT (inc_insn.reg1_val);
	}
      return true;
    }
  else if ((HAVE_PRE_MODIFY_REG || HAVE_POST_MODIFY_REG)
	   && (REG_P (XEXP (SET_SRC (pat), 1)))
	   && GET_CODE (SET_SRC (pat)) == PLUS)
    {
      /* Process a = b + c where c is a reg.  */
      inc_insn.reg1 = XEXP (SET_SRC (pat), 1);
      inc_insn.reg1_is_const = false;

      if (inc_insn.form == FORM_PRE_INC
	  || inc_insn.form == FORM_POST_INC)
	return true;
      else if (rtx_equal_p (inc_insn.reg_res, inc_insn.reg1))
	{
	  /* Reverse the two operands and turn *_ADD into *_INC since
	     a = c + a.  */
	  std::swap (inc_insn.reg0, inc_insn.reg1);
	  inc_insn.form = before_mem ? FORM_PRE_INC : FORM_POST_INC;
	  return true;
	}
      else
	return true;
    }

  return false;
}


/* A recursive function that checks all of the mem uses in
   ADDRESS_OF_X to see if any single one of them is compatible with
   what has been found in inc_insn.

   -1 is returned for success.  0 is returned if nothing was found and
   1 is returned for failure. */

static int
find_address (rtx *address_of_x)
{
  rtx x = *address_of_x;
  enum rtx_code code = GET_CODE (x);
  const char *const fmt = GET_RTX_FORMAT (code);
  int i;
  int value = 0;
  int tem;

  if (code == MEM && rtx_equal_p (XEXP (x, 0), inc_insn.reg_res))
    {
      /* Match with *reg0.  */
      mem_insn.mem_loc = address_of_x;
      mem_insn.reg0 = inc_insn.reg_res;
      mem_insn.reg1_is_const = true;
      mem_insn.reg1_val = 0;
      mem_insn.reg1 = GEN_INT (0);
      return -1;
    }
  if (code == MEM && GET_CODE (XEXP (x, 0)) == PLUS
      && rtx_equal_p (XEXP (XEXP (x, 0), 0), inc_insn.reg_res))
    {
      rtx b = XEXP (XEXP (x, 0), 1);
      mem_insn.mem_loc = address_of_x;
      mem_insn.reg0 = inc_insn.reg_res;
      mem_insn.reg1 = b;
      mem_insn.reg1_is_const = inc_insn.reg1_is_const;
      if (CONST_INT_P (b))
	{
	  /* Match with *(reg0 + reg1) where reg1 is a const. */
	  HOST_WIDE_INT val = INTVAL (b);
	  if (inc_insn.reg1_is_const
	      && (inc_insn.reg1_val == val || inc_insn.reg1_val == -val))
	    {
	      mem_insn.reg1_val = val;
	      return -1;
	    }
	}
      else if (!inc_insn.reg1_is_const
	       && rtx_equal_p (inc_insn.reg1, b))
	/* Match with *(reg0 + reg1). */
	return -1;
    }

  if (code == SIGN_EXTRACT || code == ZERO_EXTRACT)
    {
      /* If REG occurs inside a MEM used in a bit-field reference,
	 that is unacceptable.  */
      if (find_address (&XEXP (x, 0)))
	return 1;
    }

  if (x == inc_insn.reg_res)
    return 1;

  /* Time for some deep diving.  */
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  tem = find_address (&XEXP (x, i));
	  /* If this is the first use, let it go so the rest of the
	     insn can be checked.  */
	  if (value == 0)
	    value = tem;
	  else if (tem != 0)
	    /* More than one match was found.  */
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      tem = find_address (&XVECEXP (x, i, j));
	      /* If this is the first use, let it go so the rest of
		 the insn can be checked.  */
	      if (value == 0)
		value = tem;
	      else if (tem != 0)
		/* More than one match was found.  */
		return 1;
	    }
	}
    }
  return value;
}

/* Once a suitable mem reference has been found and the MEM_INSN
   structure has been filled in, FIND_INC is called to see if there is
   a suitable add or inc insn that follows the mem reference and
   determine if it is suitable to merge.

   In the case where the MEM_INSN has two registers in the reference,
   this function may be called recursively.  The first time looking
   for an add of the first register, and if that fails, looking for an
   add of the second register.  The FIRST_TRY parameter is used to
   only allow the parameters to be reversed once.  */

static bool
find_inc (bool first_try)
{
  rtx_insn *insn;
  basic_block bb = BLOCK_FOR_INSN (mem_insn.insn);
  rtx_insn *other_insn;
  df_ref def;

  /* Make sure this reg appears only once in this insn.  */
  if (count_occurrences (PATTERN (mem_insn.insn), mem_insn.reg0, 1) != 1)
    {
      if (dump_file)
	fprintf (dump_file, "mem count failure\n");
      return false;
    }

  if (dump_file)
    dump_mem_insn (dump_file);

  /* Find the next use that is an inc.  */
  insn = get_next_ref (REGNO (mem_insn.reg0),
		       BLOCK_FOR_INSN (mem_insn.insn),
		       reg_next_inc_use);
  if (!insn)
    return false;

  /* Even though we know the next use is an add or inc because it came
     from the reg_next_inc_use, we must still reparse.  */
  if (!parse_add_or_inc (insn, false))
    {
      /* Next use was not an add.  Look for one extra case. It could be
	 that we have:

	 *(a + b)
	 ...= a;
	 ...= b + a

	 if we reverse the operands in the mem ref we would
	 find this.  Only try it once though.  */
      if (first_try && !mem_insn.reg1_is_const)
	{
	  std::swap (mem_insn.reg0, mem_insn.reg1);
	  return find_inc (false);
	}
      else
	return false;
    }

  /* Need to assure that none of the operands of the inc instruction are
     assigned to by the mem insn.  */
  FOR_EACH_INSN_DEF (def, mem_insn.insn)
    {
      unsigned int regno = DF_REF_REGNO (def);
      if ((regno == REGNO (inc_insn.reg0))
	  || (regno == REGNO (inc_insn.reg_res)))
	{
	  if (dump_file)
	    fprintf (dump_file, "inc conflicts with store failure.\n");
	  return false;
	}
      if (!inc_insn.reg1_is_const && (regno == REGNO (inc_insn.reg1)))
	{
	  if (dump_file)
	    fprintf (dump_file, "inc conflicts with store failure.\n");
	  return false;
	}
    }

  if (dump_file)
    dump_inc_insn (dump_file);

  if (inc_insn.form == FORM_POST_ADD)
    {
      /* Make sure that there is no insn that assigns to inc_insn.res
	 between the mem_insn and the inc_insn.  */
      rtx_insn *other_insn = get_next_ref (REGNO (inc_insn.reg_res),
					   BLOCK_FOR_INSN (mem_insn.insn),
					   reg_next_def);
      if (other_insn != inc_insn.insn)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "result of add is assigned to between mem and inc insns.\n");
	  return false;
	}

      other_insn = get_next_ref (REGNO (inc_insn.reg_res),
				 BLOCK_FOR_INSN (mem_insn.insn),
				 reg_next_use);
      if (other_insn
	  && (other_insn != inc_insn.insn)
	  && (DF_INSN_LUID (inc_insn.insn) > DF_INSN_LUID (other_insn)))
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "result of add is used between mem and inc insns.\n");
	  return false;
	}

      /* For the post_add to work, the result_reg of the inc must not be
	 used in the mem insn since this will become the new index
	 register.  */
      if (reg_overlap_mentioned_p (inc_insn.reg_res, PATTERN (mem_insn.insn)))
	{
	  if (dump_file)
	    fprintf (dump_file, "base reg replacement failure.\n");
	  return false;
	}
    }

  if (mem_insn.reg1_is_const)
    {
      if (mem_insn.reg1_val == 0)
	{
	  if (!inc_insn.reg1_is_const)
	    {
	      /* The mem looks like *r0 and the rhs of the add has two
		 registers.  */
	      int luid = DF_INSN_LUID (inc_insn.insn);
	      if (inc_insn.form == FORM_POST_ADD)
		{
		  /* The trick is that we are not going to increment r0,
		     we are going to increment the result of the add insn.
		     For this trick to be correct, the result reg of
		     the inc must be a valid addressing reg.  */
		  addr_space_t as = MEM_ADDR_SPACE (*mem_insn.mem_loc);
		  if (GET_MODE (inc_insn.reg_res)
		      != targetm.addr_space.address_mode (as))
		    {
		      if (dump_file)
			fprintf (dump_file, "base reg mode failure.\n");
		      return false;
		    }

		  /* We also need to make sure that the next use of
		     inc result is after the inc.  */
		  other_insn
		    = get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_use);
		  if (other_insn && luid > DF_INSN_LUID (other_insn))
		    return false;

		  if (!rtx_equal_p (mem_insn.reg0, inc_insn.reg0))
		    std::swap (inc_insn.reg0, inc_insn.reg1);
		}

	      other_insn
		= get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_def);
	      if (other_insn && luid > DF_INSN_LUID (other_insn))
		return false;
	    }
	}
      /* Both the inc/add and the mem have a constant.  Need to check
	 that the constants are ok. */
      else if ((mem_insn.reg1_val != inc_insn.reg1_val)
	       && (mem_insn.reg1_val != -inc_insn.reg1_val))
	return false;
    }
  else
    {
      /* The mem insn is of the form *(a + b) where a and b are both
	 regs.  It may be that in order to match the add or inc we
	 need to treat it as if it was *(b + a).  It may also be that
	 the add is of the form a + c where c does not match b and
	 then we just abandon this.  */

      int luid = DF_INSN_LUID (inc_insn.insn);
      rtx_insn *other_insn;

      /* Make sure this reg appears only once in this insn.  */
      if (count_occurrences (PATTERN (mem_insn.insn), mem_insn.reg1, 1) != 1)
	return false;

      if (inc_insn.form == FORM_POST_ADD)
	{
	  /* For this trick to be correct, the result reg of the inc
	     must be a valid addressing reg.  */
	  addr_space_t as = MEM_ADDR_SPACE (*mem_insn.mem_loc);
	  if (GET_MODE (inc_insn.reg_res)
	      != targetm.addr_space.address_mode (as))
	    {
	      if (dump_file)
		fprintf (dump_file, "base reg mode failure.\n");
	      return false;
	    }

	  if (rtx_equal_p (mem_insn.reg0, inc_insn.reg0))
	    {
	      if (!rtx_equal_p (mem_insn.reg1, inc_insn.reg1))
		{
		  /* See comment above on find_inc (false) call.  */
		  if (first_try)
		    {
		      std::swap (mem_insn.reg0, mem_insn.reg1);
		      return find_inc (false);
		    }
		  else
		    return false;
		}

	      /* Need to check that there are no assignments to b
		 before the add insn.  */
	      other_insn
		= get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_def);
	      if (other_insn && luid > DF_INSN_LUID (other_insn))
		return false;
	      /* All ok for the next step.  */
	    }
	  else
	    {
	      /* We know that mem_insn.reg0 must equal inc_insn.reg1
		 or else we would not have found the inc insn.  */
	      std::swap (mem_insn.reg0, mem_insn.reg1);
	      if (!rtx_equal_p (mem_insn.reg0, inc_insn.reg0))
		{
		  /* See comment above on find_inc (false) call.  */
		  if (first_try)
		    return find_inc (false);
		  else
		    return false;
		}
	      /* To have gotten here know that.
	       *(b + a)

	       ... = (b + a)

	       We also know that the lhs of the inc is not b or a.  We
	       need to make sure that there are no assignments to b
	       between the mem ref and the inc.  */

	      other_insn
		= get_next_ref (REGNO (inc_insn.reg0), bb, reg_next_def);
	      if (other_insn && luid > DF_INSN_LUID (other_insn))
		return false;
	    }

	  /* Need to check that the next use of the add result is later than
	     add insn since this will be the reg incremented.  */
	  other_insn
	    = get_next_ref (REGNO (inc_insn.reg_res), bb, reg_next_use);
	  if (other_insn && luid > DF_INSN_LUID (other_insn))
	    return false;
	}
      else /* FORM_POST_INC.  There is less to check here because we
	      know that operands must line up.  */
	{
	  if (!rtx_equal_p (mem_insn.reg1, inc_insn.reg1))
	    /* See comment above on find_inc (false) call.  */
	    {
	      if (first_try)
		{
		  std::swap (mem_insn.reg0, mem_insn.reg1);
		  return find_inc (false);
		}
	      else
		return false;
	    }

	  /* To have gotten here know that.
	   *(a + b)

	   ... = (a + b)

	   We also know that the lhs of the inc is not b.  We need to make
	   sure that there are no assignments to b between the mem ref and
	   the inc.  */
	  other_insn
	    = get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_def);
	  if (other_insn && luid > DF_INSN_LUID (other_insn))
	    return false;
	}
    }

  if (inc_insn.form == FORM_POST_INC)
    {
      other_insn
	= get_next_ref (REGNO (inc_insn.reg0), bb, reg_next_use);
      /* When we found inc_insn, we were looking for the
	 next add or inc, not the next insn that used the
	 reg.  Because we are going to increment the reg
	 in this form, we need to make sure that there
	 were no intervening uses of reg.  */
      if (inc_insn.insn != other_insn)
	return false;
    }

  return try_merge ();
}


/* A recursive function that walks ADDRESS_OF_X to find all of the mem
   uses in pat that could be used as an auto inc or dec.  It then
   calls FIND_INC for each one.  */

static bool
find_mem (rtx *address_of_x)
{
  rtx x = *address_of_x;
  enum rtx_code code = GET_CODE (x);
  const char *const fmt = GET_RTX_FORMAT (code);
  int i;

  if (code == MEM && REG_P (XEXP (x, 0)))
    {
      /* Match with *reg0.  */
      mem_insn.mem_loc = address_of_x;
      mem_insn.reg0 = XEXP (x, 0);
      mem_insn.reg1_is_const = true;
      mem_insn.reg1_val = 0;
      mem_insn.reg1 = GEN_INT (0);
      if (find_inc (true))
	return true;
    }
  if (code == MEM && GET_CODE (XEXP (x, 0)) == PLUS
      && REG_P (XEXP (XEXP (x, 0), 0)))
    {
      rtx reg1 = XEXP (XEXP (x, 0), 1);
      mem_insn.mem_loc = address_of_x;
      mem_insn.reg0 = XEXP (XEXP (x, 0), 0);
      mem_insn.reg1 = reg1;
      if (CONST_INT_P (reg1))
	{
	  mem_insn.reg1_is_const = true;
	  /* Match with *(reg0 + c) where c is a const. */
	  mem_insn.reg1_val = INTVAL (reg1);
	  if (find_inc (true))
	    return true;
	}
      else if (REG_P (reg1))
	{
	  /* Match with *(reg0 + reg1).  */
	  mem_insn.reg1_is_const = false;
	  if (find_inc (true))
	    return true;
	}
    }

  if (code == SIGN_EXTRACT || code == ZERO_EXTRACT)
    {
      /* If REG occurs inside a MEM used in a bit-field reference,
	 that is unacceptable.  */
      return false;
    }

  /* Time for some deep diving.  */
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (find_mem (&XEXP (x, i)))
	    return true;
	}
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (find_mem (&XVECEXP (x, i, j)))
	      return true;
	}
    }
  return false;
}


/* Try to combine all incs and decs by constant values with memory
   references in BB.  */

static void
merge_in_block (int max_reg, basic_block bb)
{
  rtx_insn *insn;
  rtx_insn *curr;
  int success_in_block = 0;

  if (dump_file)
    fprintf (dump_file, "\n\nstarting bb %d\n", bb->index);

  FOR_BB_INSNS_REVERSE_SAFE (bb, insn, curr)
    {
      bool insn_is_add_or_inc = true;

      if (!NONDEBUG_INSN_P (insn))
	continue;

      /* This continue is deliberate.  We do not want the uses of the
	 jump put into reg_next_use because it is not considered safe to
	 combine a preincrement with a jump.  */
      if (JUMP_P (insn))
	continue;

      if (dump_file)
	dump_insn_slim (dump_file, insn);

      /* Does this instruction increment or decrement a register?  */
      if (parse_add_or_inc (insn, true))
	{
	  int regno = REGNO (inc_insn.reg_res);
	  /* Cannot handle case where there are three separate regs
	     before a mem ref.  Too many moves would be needed to be
	     profitable.  */
	  if ((inc_insn.form == FORM_PRE_INC) || inc_insn.reg1_is_const)
	    {
	      mem_insn.insn = get_next_ref (regno, bb, reg_next_use);
	      if (mem_insn.insn)
		{
		  bool ok = true;
		  if (!inc_insn.reg1_is_const)
		    {
		      /* We are only here if we are going to try a
			 HAVE_*_MODIFY_REG type transformation.  c is a
			 reg and we must sure that the path from the
			 inc_insn to the mem_insn.insn is both def and use
			 clear of c because the inc insn is going to move
			 into the mem_insn.insn.  */
		      int luid = DF_INSN_LUID (mem_insn.insn);
		      rtx_insn *other_insn
			= get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_use);

		      if (other_insn && luid > DF_INSN_LUID (other_insn))
			ok = false;

		      other_insn
			= get_next_ref (REGNO (inc_insn.reg1), bb, reg_next_def);

		      if (other_insn && luid > DF_INSN_LUID (other_insn))
			ok = false;
		    }

		  if (dump_file)
		    dump_inc_insn (dump_file);

		  if (ok && find_address (&PATTERN (mem_insn.insn)) == -1)
		    {
		      if (dump_file)
			dump_mem_insn (dump_file);
		      if (try_merge ())
			{
			  success_in_block++;
			  insn_is_add_or_inc = false;
			}
		    }
		}
	    }
	}
      else
	{
	  insn_is_add_or_inc = false;
	  mem_insn.insn = insn;
	  if (find_mem (&PATTERN (insn)))
	    success_in_block++;
	}

      /* If the inc insn was merged with a mem, the inc insn is gone
	 and there is noting to update.  */
      if (df_insn_info *insn_info = DF_INSN_INFO_GET (insn))
	{
	  df_ref def, use;

	  /* Need to update next use.  */
	  FOR_EACH_INSN_INFO_DEF (def, insn_info)
	    {
	      reg_next_use[DF_REF_REGNO (def)] = NULL;
	      reg_next_inc_use[DF_REF_REGNO (def)] = NULL;
	      reg_next_def[DF_REF_REGNO (def)] = insn;
	    }

	  FOR_EACH_INSN_INFO_USE (use, insn_info)
	    {
	      reg_next_use[DF_REF_REGNO (use)] = insn;
	      if (insn_is_add_or_inc)
		reg_next_inc_use[DF_REF_REGNO (use)] = insn;
	      else
		reg_next_inc_use[DF_REF_REGNO (use)] = NULL;
	    }
	}
      else if (dump_file)
	fprintf (dump_file, "skipping update of deleted insn %d\n",
		 INSN_UID (insn));
    }

  /* If we were successful, try again.  There may have been several
     opportunities that were interleaved.  This is rare but
     gcc.c-torture/compile/pr17273.c actually exhibits this.  */
  if (success_in_block)
    {
      /* In this case, we must clear these vectors since the trick of
	 testing if the stale insn in the block will not work.  */
      memset (reg_next_use, 0, max_reg * sizeof (rtx));
      memset (reg_next_inc_use, 0, max_reg * sizeof (rtx));
      memset (reg_next_def, 0, max_reg * sizeof (rtx));
      df_recompute_luids (bb);
      merge_in_block (max_reg, bb);
    }
}

#endif

/* Discover auto-inc auto-dec instructions.  */

namespace {

const pass_data pass_data_inc_dec =
{
  RTL_PASS, /* type */
  "auto_inc_dec", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_AUTO_INC_DEC, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_df_finish, /* todo_flags_finish */
};

class pass_inc_dec : public rtl_opt_pass
{
public:
  pass_inc_dec (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_inc_dec, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
#ifdef AUTO_INC_DEC
      return (optimize > 0 && flag_auto_inc_dec);
#else
      return false;
#endif
    }


  unsigned int execute (function *);

}; // class pass_inc_dec

unsigned int
pass_inc_dec::execute (function *fun ATTRIBUTE_UNUSED)
{
#ifdef AUTO_INC_DEC
  basic_block bb;
  int max_reg = max_reg_num ();

  if (!initialized)
    init_decision_table ();

  mem_tmp = gen_rtx_MEM (Pmode, NULL_RTX);

  df_note_add_problem ();
  df_analyze ();

  reg_next_use = XCNEWVEC (rtx_insn *, max_reg);
  reg_next_inc_use = XCNEWVEC (rtx_insn *, max_reg);
  reg_next_def = XCNEWVEC (rtx_insn *, max_reg);
  FOR_EACH_BB_FN (bb, fun)
    merge_in_block (max_reg, bb);

  free (reg_next_use);
  free (reg_next_inc_use);
  free (reg_next_def);

  mem_tmp = NULL;
#endif
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_inc_dec (gcc::context *ctxt)
{
  return new pass_inc_dec (ctxt);
}
