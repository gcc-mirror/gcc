/* Register to Stack convert for GNU compiler.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* This pass converts stack-like registers from the "flat register
   file" model that gcc uses, to a stack convention that the 387 uses.

   * The form of the input:

   On input, the function consists of insn that have had their
   registers fully allocated to a set of "virtual" registers.  Note that
   the word "virtual" is used differently here than elsewhere in gcc: for
   each virtual stack reg, there is a hard reg, but the mapping between
   them is not known until this pass is run.  On output, hard register
   numbers have been substituted, and various pop and exchange insns have
   been emitted.  The hard register numbers and the virtual register
   numbers completely overlap - before this pass, all stack register
   numbers are virtual, and afterward they are all hard.

   The virtual registers can be manipulated normally by gcc, and their
   semantics are the same as for normal registers.  After the hard
   register numbers are substituted, the semantics of an insn containing
   stack-like regs are not the same as for an insn with normal regs: for
   instance, it is not safe to delete an insn that appears to be a no-op
   move.  In general, no insn containing hard regs should be changed
   after this pass is done.

   * The form of the output:

   After this pass, hard register numbers represent the distance from
   the current top of stack to the desired register.  A reference to
   FIRST_STACK_REG references the top of stack, FIRST_STACK_REG + 1,
   represents the register just below that, and so forth.  Also, REG_DEAD
   notes indicate whether or not a stack register should be popped.

   A "swap" insn looks like a parallel of two patterns, where each
   pattern is a SET: one sets A to B, the other B to A.

   A "push" or "load" insn is a SET whose SET_DEST is FIRST_STACK_REG
   and whose SET_DEST is REG or MEM.  Any other SET_DEST, such as PLUS,
   will replace the existing stack top, not push a new value.

   A store insn is a SET whose SET_DEST is FIRST_STACK_REG, and whose
   SET_SRC is REG or MEM.

   The case where the SET_SRC and SET_DEST are both FIRST_STACK_REG
   appears ambiguous.  As a special case, the presence of a REG_DEAD note
   for FIRST_STACK_REG differentiates between a load insn and a pop.

   If a REG_DEAD is present, the insn represents a "pop" that discards
   the top of the register stack.  If there is no REG_DEAD note, then the
   insn represents a "dup" or a push of the current top of stack onto the
   stack.

   * Methodology:

   Existing REG_DEAD and REG_UNUSED notes for stack registers are
   deleted and recreated from scratch.  REG_DEAD is never created for a
   SET_DEST, only REG_UNUSED.

   * asm_operands:

   There are several rules on the usage of stack-like regs in
   asm_operands insns.  These rules apply only to the operands that are
   stack-like regs:

   1. Given a set of input regs that die in an asm_operands, it is
      necessary to know which are implicitly popped by the asm, and
      which must be explicitly popped by gcc.

	An input reg that is implicitly popped by the asm must be
	explicitly clobbered, unless it is constrained to match an
	output operand.

   2. For any input reg that is implicitly popped by an asm, it is
      necessary to know how to adjust the stack to compensate for the pop.
      If any non-popped input is closer to the top of the reg-stack than
      the implicitly popped reg, it would not be possible to know what the
      stack looked like - it's not clear how the rest of the stack "slides
      up".

	All implicitly popped input regs must be closer to the top of
	the reg-stack than any input that is not implicitly popped.

   3. It is possible that if an input dies in an insn, reload might
      use the input reg for an output reload.  Consider this example:

		asm ("foo" : "=t" (a) : "f" (b));

      This asm says that input B is not popped by the asm, and that
      the asm pushes a result onto the reg-stack, ie, the stack is one
      deeper after the asm than it was before.  But, it is possible that
      reload will think that it can use the same reg for both the input and
      the output, if input B dies in this insn.

	If any input operand uses the "f" constraint, all output reg
	constraints must use the "&" earlyclobber.

      The asm above would be written as

		asm ("foo" : "=&t" (a) : "f" (b));

   4. Some operands need to be in particular places on the stack.  All
      output operands fall in this category - there is no other way to
      know which regs the outputs appear in unless the user indicates
      this in the constraints.

	Output operands must specifically indicate which reg an output
	appears in after an asm.  "=f" is not allowed: the operand
	constraints must select a class with a single reg.

   5. Output operands may not be "inserted" between existing stack regs.
      Since no 387 opcode uses a read/write operand, all output operands
      are dead before the asm_operands, and are pushed by the asm_operands.
      It makes no sense to push anywhere but the top of the reg-stack.

	Output operands must start at the top of the reg-stack: output
	operands may not "skip" a reg.

   6. Some asm statements may need extra stack space for internal
      calculations.  This can be guaranteed by clobbering stack registers
      unrelated to the inputs and outputs.

   Here are a couple of reasonable asms to want to write.  This asm
   takes one input, which is internally popped, and produces two outputs.

	asm ("fsincos" : "=t" (cos), "=u" (sin) : "0" (inp));

   This asm takes two inputs, which are popped by the fyl2xp1 opcode,
   and replaces them with one output.  The user must code the "st(1)"
   clobber for reg-stack.c to know that fyl2xp1 pops both inputs.

	asm ("fyl2xp1" : "=t" (result) : "0" (x), "u" (y) : "st(1)");

*/

#include "config.h"
#include "system.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "function.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "toplev.h"
#include "recog.h"
#include "output.h"
#include "basic-block.h"
#include "varray.h"
#include "reload.h"
#include "ggc.h"

/* We use this array to cache info about insns, because otherwise we
   spend too much time in stack_regs_mentioned_p.

   Indexed by insn UIDs.  A value of zero is uninitialized, one indicates
   the insn uses stack registers, two indicates the insn does not use
   stack registers.  */
static GTY(()) varray_type stack_regs_mentioned_data;

#ifdef STACK_REGS

#define REG_STACK_SIZE (LAST_STACK_REG - FIRST_STACK_REG + 1)

/* This is the basic stack record.  TOP is an index into REG[] such
   that REG[TOP] is the top of stack.  If TOP is -1 the stack is empty.

   If TOP is -2, REG[] is not yet initialized.  Stack initialization
   consists of placing each live reg in array `reg' and setting `top'
   appropriately.

   REG_SET indicates which registers are live.  */

typedef struct stack_def
{
  int top;			/* index to top stack element */
  HARD_REG_SET reg_set;		/* set of live registers */
  unsigned char reg[REG_STACK_SIZE];/* register - stack mapping */
} *stack;

/* This is used to carry information about basic blocks.  It is
   attached to the AUX field of the standard CFG block.  */

typedef struct block_info_def
{
  struct stack_def stack_in;	/* Input stack configuration.  */
  struct stack_def stack_out;	/* Output stack configuration.  */
  HARD_REG_SET out_reg_set;	/* Stack regs live on output.  */
  int done;			/* True if block already converted.  */
  int predecessors;		/* Number of predecessors that needs
				   to be visited.  */
} *block_info;

#define BLOCK_INFO(B)	((block_info) (B)->aux)

/* Passed to change_stack to indicate where to emit insns.  */
enum emit_where
{
  EMIT_AFTER,
  EMIT_BEFORE
};

/* The block we're currently working on.  */
static basic_block current_block;

/* This is the register file for all register after conversion.  */
static rtx
  FP_mode_reg[LAST_STACK_REG+1-FIRST_STACK_REG][(int) MAX_MACHINE_MODE];

#define FP_MODE_REG(regno,mode)	\
  (FP_mode_reg[(regno)-FIRST_STACK_REG][(int) (mode)])

/* Used to initialize uninitialized registers.  */
static rtx nan;

/* Forward declarations */

static int stack_regs_mentioned_p	PARAMS ((rtx pat));
static void straighten_stack		PARAMS ((rtx, stack));
static void pop_stack			PARAMS ((stack, int));
static rtx *get_true_reg		PARAMS ((rtx *));

static int check_asm_stack_operands	PARAMS ((rtx));
static int get_asm_operand_n_inputs	PARAMS ((rtx));
static rtx stack_result			PARAMS ((tree));
static void replace_reg			PARAMS ((rtx *, int));
static void remove_regno_note		PARAMS ((rtx, enum reg_note,
						 unsigned int));
static int get_hard_regnum		PARAMS ((stack, rtx));
static rtx emit_pop_insn		PARAMS ((rtx, stack, rtx,
					       enum emit_where));
static void emit_swap_insn		PARAMS ((rtx, stack, rtx));
static void move_for_stack_reg		PARAMS ((rtx, stack, rtx));
static int swap_rtx_condition_1		PARAMS ((rtx));
static int swap_rtx_condition		PARAMS ((rtx));
static void compare_for_stack_reg	PARAMS ((rtx, stack, rtx));
static void subst_stack_regs_pat	PARAMS ((rtx, stack, rtx));
static void subst_asm_stack_regs	PARAMS ((rtx, stack));
static void subst_stack_regs		PARAMS ((rtx, stack));
static void change_stack		PARAMS ((rtx, stack, stack,
					       enum emit_where));
static int convert_regs_entry		PARAMS ((void));
static void convert_regs_exit		PARAMS ((void));
static int convert_regs_1		PARAMS ((FILE *, basic_block));
static int convert_regs_2		PARAMS ((FILE *, basic_block));
static int convert_regs			PARAMS ((FILE *));
static void print_stack 		PARAMS ((FILE *, stack));
static rtx next_flags_user 		PARAMS ((rtx));
static void record_label_references	PARAMS ((rtx, rtx));
static bool compensate_edge		PARAMS ((edge, FILE *));

/* Return nonzero if any stack register is mentioned somewhere within PAT.  */

static int
stack_regs_mentioned_p (pat)
     rtx pat;
{
  const char *fmt;
  int i;

  if (STACK_REG_P (pat))
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
	    if (stack_regs_mentioned_p (XVECEXP (pat, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && stack_regs_mentioned_p (XEXP (pat, i)))
	return 1;
    }

  return 0;
}

/* Return nonzero if INSN mentions stacked registers, else return zero.  */

int
stack_regs_mentioned (insn)
     rtx insn;
{
  unsigned int uid, max;
  int test;

  if (! INSN_P (insn) || !stack_regs_mentioned_data)
    return 0;

  uid = INSN_UID (insn);
  max = VARRAY_SIZE (stack_regs_mentioned_data);
  if (uid >= max)
    {
      /* Allocate some extra size to avoid too many reallocs, but
	 do not grow too quickly.  */
      max = uid + uid / 20;
      VARRAY_GROW (stack_regs_mentioned_data, max);
    }

  test = VARRAY_CHAR (stack_regs_mentioned_data, uid);
  if (test == 0)
    {
      /* This insn has yet to be examined.  Do so now.  */
      test = stack_regs_mentioned_p (PATTERN (insn)) ? 1 : 2;
      VARRAY_CHAR (stack_regs_mentioned_data, uid) = test;
    }

  return test == 1;
}

static rtx ix86_flags_rtx;

static rtx
next_flags_user (insn)
     rtx insn;
{
  /* Search forward looking for the first use of this value.
     Stop at block boundaries.  */

  while (insn != current_block->end)
    {
      insn = NEXT_INSN (insn);

      if (INSN_P (insn) && reg_mentioned_p (ix86_flags_rtx, PATTERN (insn)))
	return insn;

      if (GET_CODE (insn) == CALL_INSN)
	return NULL_RTX;
    }
  return NULL_RTX;
}

/* Reorganize the stack into ascending numbers,
   after this insn.  */

static void
straighten_stack (insn, regstack)
     rtx insn;
     stack regstack;
{
  struct stack_def temp_stack;
  int top;

  /* If there is only a single register on the stack, then the stack is
     already in increasing order and no reorganization is needed.

     Similarly if the stack is empty.  */
  if (regstack->top <= 0)
    return;

  COPY_HARD_REG_SET (temp_stack.reg_set, regstack->reg_set);

  for (top = temp_stack.top = regstack->top; top >= 0; top--)
    temp_stack.reg[top] = FIRST_STACK_REG + temp_stack.top - top;

  change_stack (insn, regstack, &temp_stack, EMIT_AFTER);
}

/* Pop a register from the stack.  */

static void
pop_stack (regstack, regno)
     stack regstack;
     int   regno;
{
  int top = regstack->top;

  CLEAR_HARD_REG_BIT (regstack->reg_set, regno);
  regstack->top--;
  /* If regno was not at the top of stack then adjust stack.  */
  if (regstack->reg [top] != regno)
    {
      int i;
      for (i = regstack->top; i >= 0; i--)
	if (regstack->reg [i] == regno)
	  {
	    int j;
	    for (j = i; j < top; j++)
	      regstack->reg [j] = regstack->reg [j + 1];
	    break;
	  }
    }
}

/* Convert register usage from "flat" register file usage to a "stack
   register file.  FIRST is the first insn in the function, FILE is the
   dump file, if used.

   Construct a CFG and run life analysis.  Then convert each insn one
   by one.  Run a last cleanup_cfg pass, if optimizing, to eliminate
   code duplication created when the converter inserts pop insns on
   the edges.  */

void
reg_to_stack (first, file)
     rtx first;
     FILE *file;
{
  basic_block bb;
  int i;
  int max_uid;

  /* Clean up previous run.  */
  stack_regs_mentioned_data = 0;

  /* See if there is something to do.  Flow analysis is quite
     expensive so we might save some compilation time.  */
  for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
    if (regs_ever_live[i])
      break;
  if (i > LAST_STACK_REG)
    return;

  /* Ok, floating point instructions exist.  If not optimizing,
     build the CFG and run life analysis.  */
  if (!optimize)
    {
      count_or_remove_death_notes (NULL, 1);
      life_analysis (first, file, PROP_DEATH_NOTES);
    }
  mark_dfs_back_edges ();

  /* Set up block info for each basic block.  */
  alloc_aux_for_blocks (sizeof (struct block_info_def));
  FOR_EACH_BB_REVERSE (bb)
    {
      edge e;
      for (e = bb->pred; e; e=e->pred_next)
	if (!(e->flags & EDGE_DFS_BACK)
	    && e->src != ENTRY_BLOCK_PTR)
	  BLOCK_INFO (bb)->predecessors++;
    }

  /* Create the replacement registers up front.  */
  for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
    {
      enum machine_mode mode;
      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	FP_MODE_REG (i, mode) = gen_rtx_REG (mode, i);
      for (mode = GET_CLASS_NARROWEST_MODE (MODE_COMPLEX_FLOAT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	FP_MODE_REG (i, mode) = gen_rtx_REG (mode, i);
    }

  ix86_flags_rtx = gen_rtx_REG (CCmode, FLAGS_REG);

  /* A QNaN for initializing uninitialized variables.

     ??? We can't load from constant memory in PIC mode, because
     we're insertting these instructions before the prologue and
     the PIC register hasn't been set up.  In that case, fall back
     on zero, which we can get from `ldz'.  */

  if (flag_pic)
    nan = CONST0_RTX (SFmode);
  else
    {
      nan = gen_lowpart (SFmode, GEN_INT (0x7fc00000));
      nan = force_const_mem (SFmode, nan);
    }

  /* Allocate a cache for stack_regs_mentioned.  */
  max_uid = get_max_uid ();
  VARRAY_CHAR_INIT (stack_regs_mentioned_data, max_uid + 1,
		    "stack_regs_mentioned cache");

  convert_regs (file);

  free_aux_for_blocks ();
}

/* Check PAT, which is in INSN, for LABEL_REFs.  Add INSN to the
   label's chain of references, and note which insn contains each
   reference.  */

static void
record_label_references (insn, pat)
     rtx insn, pat;
{
  enum rtx_code code = GET_CODE (pat);
  int i;
  const char *fmt;

  if (code == LABEL_REF)
    {
      rtx label = XEXP (pat, 0);
      rtx ref;

      if (GET_CODE (label) != CODE_LABEL)
	abort ();

      /* If this is an undefined label, LABEL_REFS (label) contains
         garbage.  */
      if (INSN_UID (label) == 0)
	return;

      /* Don't make a duplicate in the code_label's chain.  */

      for (ref = LABEL_REFS (label);
	   ref && ref != label;
	   ref = LABEL_NEXTREF (ref))
	if (CONTAINING_INSN (ref) == insn)
	  return;

      CONTAINING_INSN (pat) = insn;
      LABEL_NEXTREF (pat) = LABEL_REFS (label);
      LABEL_REFS (label) = pat;

      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	record_label_references (insn, XEXP (pat, i));
      if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (pat, i); j++)
	    record_label_references (insn, XVECEXP (pat, i, j));
	}
    }
}

/* Return a pointer to the REG expression within PAT.  If PAT is not a
   REG, possible enclosed by a conversion rtx, return the inner part of
   PAT that stopped the search.  */

static rtx *
get_true_reg (pat)
     rtx *pat;
{
  for (;;)
    switch (GET_CODE (*pat))
      {
      case SUBREG:
	/* Eliminate FP subregister accesses in favor of the
	   actual FP register in use.  */
	{
	  rtx subreg;
	  if (FP_REG_P (subreg = SUBREG_REG (*pat)))
	    {
	      int regno_off = subreg_regno_offset (REGNO (subreg),
						   GET_MODE (subreg),
						   SUBREG_BYTE (*pat),
						   GET_MODE (*pat));
	      *pat = FP_MODE_REG (REGNO (subreg) + regno_off,
				  GET_MODE (subreg));
	    default:
	      return pat;
	    }
	}
      case FLOAT:
      case FIX:
      case FLOAT_EXTEND:
	pat = & XEXP (*pat, 0);
      }
}

/* Set if we find any malformed asms in a block.  */
static bool any_malformed_asm;

/* There are many rules that an asm statement for stack-like regs must
   follow.  Those rules are explained at the top of this file: the rule
   numbers below refer to that explanation.  */

static int
check_asm_stack_operands (insn)
     rtx insn;
{
  int i;
  int n_clobbers;
  int malformed_asm = 0;
  rtx body = PATTERN (insn);

  char reg_used_as_output[FIRST_PSEUDO_REGISTER];
  char implicitly_dies[FIRST_PSEUDO_REGISTER];
  int alt;

  rtx *clobber_reg = 0;
  int n_inputs, n_outputs;

  /* Find out what the constraints require.  If no constraint
     alternative matches, this asm is malformed.  */
  extract_insn (insn);
  constrain_operands (1);
  alt = which_alternative;

  preprocess_constraints ();

  n_inputs = get_asm_operand_n_inputs (body);
  n_outputs = recog_data.n_operands - n_inputs;

  if (alt < 0)
    {
      malformed_asm = 1;
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx_USE (VOIDmode, const0_rtx);
      return 0;
    }

  /* Strip SUBREGs here to make the following code simpler.  */
  for (i = 0; i < recog_data.n_operands; i++)
    if (GET_CODE (recog_data.operand[i]) == SUBREG
	&& GET_CODE (SUBREG_REG (recog_data.operand[i])) == REG)
      recog_data.operand[i] = SUBREG_REG (recog_data.operand[i]);

  /* Set up CLOBBER_REG.  */

  n_clobbers = 0;

  if (GET_CODE (body) == PARALLEL)
    {
      clobber_reg = (rtx *) alloca (XVECLEN (body, 0) * sizeof (rtx));

      for (i = 0; i < XVECLEN (body, 0); i++)
	if (GET_CODE (XVECEXP (body, 0, i)) == CLOBBER)
	  {
	    rtx clobber = XVECEXP (body, 0, i);
	    rtx reg = XEXP (clobber, 0);

	    if (GET_CODE (reg) == SUBREG && GET_CODE (SUBREG_REG (reg)) == REG)
	      reg = SUBREG_REG (reg);

	    if (STACK_REG_P (reg))
	      {
		clobber_reg[n_clobbers] = reg;
		n_clobbers++;
	      }
	  }
    }

  /* Enforce rule #4: Output operands must specifically indicate which
     reg an output appears in after an asm.  "=f" is not allowed: the
     operand constraints must select a class with a single reg.

     Also enforce rule #5: Output operands must start at the top of
     the reg-stack: output operands may not "skip" a reg.  */

  memset (reg_used_as_output, 0, sizeof (reg_used_as_output));
  for (i = 0; i < n_outputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	if (reg_class_size[(int) recog_op_alt[i][alt].class] != 1)
	  {
	    error_for_asm (insn, "output constraint %d must specify a single register", i);
	    malformed_asm = 1;
	  }
	else
	  {
	    int j;

	    for (j = 0; j < n_clobbers; j++)
	      if (REGNO (recog_data.operand[i]) == REGNO (clobber_reg[j]))
		{
		  error_for_asm (insn, "output constraint %d cannot be specified together with \"%s\" clobber",
				 i, reg_names [REGNO (clobber_reg[j])]);
		  malformed_asm = 1;
		  break;
		}
	    if (j == n_clobbers)
	      reg_used_as_output[REGNO (recog_data.operand[i])] = 1;
	  }
      }


  /* Search for first non-popped reg.  */
  for (i = FIRST_STACK_REG; i < LAST_STACK_REG + 1; i++)
    if (! reg_used_as_output[i])
      break;

  /* If there are any other popped regs, that's an error.  */
  for (; i < LAST_STACK_REG + 1; i++)
    if (reg_used_as_output[i])
      break;

  if (i != LAST_STACK_REG + 1)
    {
      error_for_asm (insn, "output regs must be grouped at top of stack");
      malformed_asm = 1;
    }

  /* Enforce rule #2: All implicitly popped input regs must be closer
     to the top of the reg-stack than any input that is not implicitly
     popped.  */

  memset (implicitly_dies, 0, sizeof (implicitly_dies));
  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	/* An input reg is implicitly popped if it is tied to an
	   output, or if there is a CLOBBER for it.  */
	int j;

	for (j = 0; j < n_clobbers; j++)
	  if (operands_match_p (clobber_reg[j], recog_data.operand[i]))
	    break;

	if (j < n_clobbers || recog_op_alt[i][alt].matches >= 0)
	  implicitly_dies[REGNO (recog_data.operand[i])] = 1;
      }

  /* Search for first non-popped reg.  */
  for (i = FIRST_STACK_REG; i < LAST_STACK_REG + 1; i++)
    if (! implicitly_dies[i])
      break;

  /* If there are any other popped regs, that's an error.  */
  for (; i < LAST_STACK_REG + 1; i++)
    if (implicitly_dies[i])
      break;

  if (i != LAST_STACK_REG + 1)
    {
      error_for_asm (insn,
		     "implicitly popped regs must be grouped at top of stack");
      malformed_asm = 1;
    }

  /* Enfore rule #3: If any input operand uses the "f" constraint, all
     output constraints must use the "&" earlyclobber.

     ??? Detect this more deterministically by having constrain_asm_operands
     record any earlyclobber.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (recog_op_alt[i][alt].matches == -1)
      {
	int j;

	for (j = 0; j < n_outputs; j++)
	  if (operands_match_p (recog_data.operand[j], recog_data.operand[i]))
	    {
	      error_for_asm (insn,
			     "output operand %d must use `&' constraint", j);
	      malformed_asm = 1;
	    }
      }

  if (malformed_asm)
    {
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx_USE (VOIDmode, const0_rtx);
      any_malformed_asm = true;
      return 0;
    }

  return 1;
}

/* Calculate the number of inputs and outputs in BODY, an
   asm_operands.  N_OPERANDS is the total number of operands, and
   N_INPUTS and N_OUTPUTS are pointers to ints into which the results are
   placed.  */

static int
get_asm_operand_n_inputs (body)
     rtx body;
{
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    return ASM_OPERANDS_INPUT_LENGTH (SET_SRC (body));

  else if (GET_CODE (body) == ASM_OPERANDS)
    return ASM_OPERANDS_INPUT_LENGTH (body);

  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    return ASM_OPERANDS_INPUT_LENGTH (SET_SRC (XVECEXP (body, 0, 0)));

  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    return ASM_OPERANDS_INPUT_LENGTH (XVECEXP (body, 0, 0));

  abort ();
}

/* If current function returns its result in an fp stack register,
   return the REG.  Otherwise, return 0.  */

static rtx
stack_result (decl)
     tree decl;
{
  rtx result;

  /* If the value is supposed to be returned in memory, then clearly
     it is not returned in a stack register.  */
  if (aggregate_value_p (DECL_RESULT (decl)))
    return 0;

  result = DECL_RTL_IF_SET (DECL_RESULT (decl));
  if (result != 0)
    {
#ifdef FUNCTION_OUTGOING_VALUE
      result
	= FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (decl)), decl);
#else
      result = FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (decl)), decl);
#endif
    }

  return result != 0 && STACK_REG_P (result) ? result : 0;
}


/*
 * This section deals with stack register substitution, and forms the second
 * pass over the RTL.
 */

/* Replace REG, which is a pointer to a stack reg RTX, with an RTX for
   the desired hard REGNO.  */

static void
replace_reg (reg, regno)
     rtx *reg;
     int regno;
{
  if (regno < FIRST_STACK_REG || regno > LAST_STACK_REG
      || ! STACK_REG_P (*reg))
    abort ();

  switch (GET_MODE_CLASS (GET_MODE (*reg)))
    {
    default: abort ();
    case MODE_FLOAT:
    case MODE_COMPLEX_FLOAT:;
    }

  *reg = FP_MODE_REG (regno, GET_MODE (*reg));
}

/* Remove a note of type NOTE, which must be found, for register
   number REGNO from INSN.  Remove only one such note.  */

static void
remove_regno_note (insn, note, regno)
     rtx insn;
     enum reg_note note;
     unsigned int regno;
{
  rtx *note_link, this;

  note_link = &REG_NOTES (insn);
  for (this = *note_link; this; this = XEXP (this, 1))
    if (REG_NOTE_KIND (this) == note
	&& REG_P (XEXP (this, 0)) && REGNO (XEXP (this, 0)) == regno)
      {
	*note_link = XEXP (this, 1);
	return;
      }
    else
      note_link = &XEXP (this, 1);

  abort ();
}

/* Find the hard register number of virtual register REG in REGSTACK.
   The hard register number is relative to the top of the stack.  -1 is
   returned if the register is not found.  */

static int
get_hard_regnum (regstack, reg)
     stack regstack;
     rtx reg;
{
  int i;

  if (! STACK_REG_P (reg))
    abort ();

  for (i = regstack->top; i >= 0; i--)
    if (regstack->reg[i] == REGNO (reg))
      break;

  return i >= 0 ? (FIRST_STACK_REG + regstack->top - i) : -1;
}

/* Emit an insn to pop virtual register REG before or after INSN.
   REGSTACK is the stack state after INSN and is updated to reflect this
   pop.  WHEN is either emit_insn_before or emit_insn_after.  A pop insn
   is represented as a SET whose destination is the register to be popped
   and source is the top of stack.  A death note for the top of stack
   cases the movdf pattern to pop.  */

static rtx
emit_pop_insn (insn, regstack, reg, where)
     rtx insn;
     stack regstack;
     rtx reg;
     enum emit_where where;
{
  rtx pop_insn, pop_rtx;
  int hard_regno;

  /* For complex types take care to pop both halves.  These may survive in
     CLOBBER and USE expressions.  */
  if (COMPLEX_MODE_P (GET_MODE (reg)))
    {
      rtx reg1 = FP_MODE_REG (REGNO (reg), DFmode);
      rtx reg2 = FP_MODE_REG (REGNO (reg) + 1, DFmode);

      pop_insn = NULL_RTX;
      if (get_hard_regnum (regstack, reg1) >= 0)
	pop_insn = emit_pop_insn (insn, regstack, reg1, where);
      if (get_hard_regnum (regstack, reg2) >= 0)
	pop_insn = emit_pop_insn (insn, regstack, reg2, where);
      if (!pop_insn)
	abort ();
      return pop_insn;
    }

  hard_regno = get_hard_regnum (regstack, reg);

  if (hard_regno < FIRST_STACK_REG)
    abort ();

  pop_rtx = gen_rtx_SET (VOIDmode, FP_MODE_REG (hard_regno, DFmode),
			 FP_MODE_REG (FIRST_STACK_REG, DFmode));

  if (where == EMIT_AFTER)
    pop_insn = emit_insn_after (pop_rtx, insn);
  else
    pop_insn = emit_insn_before (pop_rtx, insn);

  REG_NOTES (pop_insn)
    = gen_rtx_EXPR_LIST (REG_DEAD, FP_MODE_REG (FIRST_STACK_REG, DFmode),
			 REG_NOTES (pop_insn));

  regstack->reg[regstack->top - (hard_regno - FIRST_STACK_REG)]
    = regstack->reg[regstack->top];
  regstack->top -= 1;
  CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (reg));

  return pop_insn;
}

/* Emit an insn before or after INSN to swap virtual register REG with
   the top of stack.  REGSTACK is the stack state before the swap, and
   is updated to reflect the swap.  A swap insn is represented as a
   PARALLEL of two patterns: each pattern moves one reg to the other.

   If REG is already at the top of the stack, no insn is emitted.  */

static void
emit_swap_insn (insn, regstack, reg)
     rtx insn;
     stack regstack;
     rtx reg;
{
  int hard_regno;
  rtx swap_rtx;
  int tmp, other_reg;		/* swap regno temps */
  rtx i1;			/* the stack-reg insn prior to INSN */
  rtx i1set = NULL_RTX;		/* the SET rtx within I1 */

  hard_regno = get_hard_regnum (regstack, reg);

  if (hard_regno < FIRST_STACK_REG)
    abort ();
  if (hard_regno == FIRST_STACK_REG)
    return;

  other_reg = regstack->top - (hard_regno - FIRST_STACK_REG);

  tmp = regstack->reg[other_reg];
  regstack->reg[other_reg] = regstack->reg[regstack->top];
  regstack->reg[regstack->top] = tmp;

  /* Find the previous insn involving stack regs, but don't pass a
     block boundary.  */
  i1 = NULL;
  if (current_block && insn != current_block->head)
    {
      rtx tmp = PREV_INSN (insn);
      rtx limit = PREV_INSN (current_block->head);
      while (tmp != limit)
	{
	  if (GET_CODE (tmp) == CODE_LABEL
	      || GET_CODE (tmp) == CALL_INSN
	      || NOTE_INSN_BASIC_BLOCK_P (tmp)
	      || (GET_CODE (tmp) == INSN
		  && stack_regs_mentioned (tmp)))
	    {
	      i1 = tmp;
	      break;
	    }
	  tmp = PREV_INSN (tmp);
	}
    }

  if (i1 != NULL_RTX
      && (i1set = single_set (i1)) != NULL_RTX)
    {
      rtx i1src = *get_true_reg (&SET_SRC (i1set));
      rtx i1dest = *get_true_reg (&SET_DEST (i1set));

      /* If the previous register stack push was from the reg we are to
	 swap with, omit the swap.  */

      if (GET_CODE (i1dest) == REG && REGNO (i1dest) == FIRST_STACK_REG
	  && GET_CODE (i1src) == REG
	  && REGNO (i1src) == (unsigned) hard_regno - 1
	  && find_regno_note (i1, REG_DEAD, FIRST_STACK_REG) == NULL_RTX)
	return;

      /* If the previous insn wrote to the reg we are to swap with,
	 omit the swap.  */

      if (GET_CODE (i1dest) == REG && REGNO (i1dest) == (unsigned) hard_regno
	  && GET_CODE (i1src) == REG && REGNO (i1src) == FIRST_STACK_REG
	  && find_regno_note (i1, REG_DEAD, FIRST_STACK_REG) == NULL_RTX)
	return;
    }

  swap_rtx = gen_swapxf (FP_MODE_REG (hard_regno, XFmode),
			 FP_MODE_REG (FIRST_STACK_REG, XFmode));

  if (i1)
    emit_insn_after (swap_rtx, i1);
  else if (current_block)
    emit_insn_before (swap_rtx, current_block->head);
  else
    emit_insn_before (swap_rtx, insn);
}

/* Handle a move to or from a stack register in PAT, which is in INSN.
   REGSTACK is the current stack.  */

static void
move_for_stack_reg (insn, regstack, pat)
     rtx insn;
     stack regstack;
     rtx pat;
{
  rtx *psrc =  get_true_reg (&SET_SRC (pat));
  rtx *pdest = get_true_reg (&SET_DEST (pat));
  rtx src, dest;
  rtx note;

  src = *psrc; dest = *pdest;

  if (STACK_REG_P (src) && STACK_REG_P (dest))
    {
      /* Write from one stack reg to another.  If SRC dies here, then
	 just change the register mapping and delete the insn.  */

      note = find_regno_note (insn, REG_DEAD, REGNO (src));
      if (note)
	{
	  int i;

	  /* If this is a no-op move, there must not be a REG_DEAD note.  */
	  if (REGNO (src) == REGNO (dest))
	    abort ();

	  for (i = regstack->top; i >= 0; i--)
	    if (regstack->reg[i] == REGNO (src))
	      break;

	  /* The source must be live, and the dest must be dead.  */
	  if (i < 0 || get_hard_regnum (regstack, dest) >= FIRST_STACK_REG)
	    abort ();

	  /* It is possible that the dest is unused after this insn.
	     If so, just pop the src.  */

	  if (find_regno_note (insn, REG_UNUSED, REGNO (dest)))
	    {
	      emit_pop_insn (insn, regstack, src, EMIT_AFTER);

	      delete_insn (insn);
	      return;
	    }

	  regstack->reg[i] = REGNO (dest);

	  SET_HARD_REG_BIT (regstack->reg_set, REGNO (dest));
	  CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (src));

	  delete_insn (insn);

	  return;
	}

      /* The source reg does not die.  */

      /* If this appears to be a no-op move, delete it, or else it
	 will confuse the machine description output patterns. But if
	 it is REG_UNUSED, we must pop the reg now, as per-insn processing
	 for REG_UNUSED will not work for deleted insns.  */

      if (REGNO (src) == REGNO (dest))
	{
	  if (find_regno_note (insn, REG_UNUSED, REGNO (dest)))
	    emit_pop_insn (insn, regstack, dest, EMIT_AFTER);

	  delete_insn (insn);
	  return;
	}

      /* The destination ought to be dead.  */
      if (get_hard_regnum (regstack, dest) >= FIRST_STACK_REG)
	abort ();

      replace_reg (psrc, get_hard_regnum (regstack, src));

      regstack->reg[++regstack->top] = REGNO (dest);
      SET_HARD_REG_BIT (regstack->reg_set, REGNO (dest));
      replace_reg (pdest, FIRST_STACK_REG);
    }
  else if (STACK_REG_P (src))
    {
      /* Save from a stack reg to MEM, or possibly integer reg.  Since
	 only top of stack may be saved, emit an exchange first if
	 needs be.  */

      emit_swap_insn (insn, regstack, src);

      note = find_regno_note (insn, REG_DEAD, REGNO (src));
      if (note)
	{
	  replace_reg (&XEXP (note, 0), FIRST_STACK_REG);
	  regstack->top--;
	  CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (src));
	}
      else if ((GET_MODE (src) == XFmode || GET_MODE (src) == TFmode)
	       && regstack->top < REG_STACK_SIZE - 1)
	{
	  /* A 387 cannot write an XFmode value to a MEM without
	     clobbering the source reg.  The output code can handle
	     this by reading back the value from the MEM.
	     But it is more efficient to use a temp register if one is
	     available.  Push the source value here if the register
	     stack is not full, and then write the value to memory via
	     a pop.  */
	  rtx push_rtx, push_insn;
	  rtx top_stack_reg = FP_MODE_REG (FIRST_STACK_REG, GET_MODE (src));

	  if (GET_MODE (src) == TFmode)
	    push_rtx = gen_movtf (top_stack_reg, top_stack_reg);
	  else
	    push_rtx = gen_movxf (top_stack_reg, top_stack_reg);
	  push_insn = emit_insn_before (push_rtx, insn);
	  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_DEAD, top_stack_reg,
						REG_NOTES (insn));
	}

      replace_reg (psrc, FIRST_STACK_REG);
    }
  else if (STACK_REG_P (dest))
    {
      /* Load from MEM, or possibly integer REG or constant, into the
	 stack regs.  The actual target is always the top of the
	 stack. The stack mapping is changed to reflect that DEST is
	 now at top of stack.  */

      /* The destination ought to be dead.  */
      if (get_hard_regnum (regstack, dest) >= FIRST_STACK_REG)
	abort ();

      if (regstack->top >= REG_STACK_SIZE)
	abort ();

      regstack->reg[++regstack->top] = REGNO (dest);
      SET_HARD_REG_BIT (regstack->reg_set, REGNO (dest));
      replace_reg (pdest, FIRST_STACK_REG);
    }
  else
    abort ();
}

/* Swap the condition on a branch, if there is one.  Return true if we
   found a condition to swap.  False if the condition was not used as
   such.  */

static int
swap_rtx_condition_1 (pat)
     rtx pat;
{
  const char *fmt;
  int i, r = 0;

  if (GET_RTX_CLASS (GET_CODE (pat)) == '<')
    {
      PUT_CODE (pat, swap_condition (GET_CODE (pat)));
      r = 1;
    }
  else
    {
      fmt = GET_RTX_FORMAT (GET_CODE (pat));
      for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'E')
	    {
	      int j;

	      for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
		r |= swap_rtx_condition_1 (XVECEXP (pat, i, j));
	    }
	  else if (fmt[i] == 'e')
	    r |= swap_rtx_condition_1 (XEXP (pat, i));
	}
    }

  return r;
}

static int
swap_rtx_condition (insn)
     rtx insn;
{
  rtx pat = PATTERN (insn);

  /* We're looking for a single set to cc0 or an HImode temporary.  */

  if (GET_CODE (pat) == SET
      && GET_CODE (SET_DEST (pat)) == REG
      && REGNO (SET_DEST (pat)) == FLAGS_REG)
    {
      insn = next_flags_user (insn);
      if (insn == NULL_RTX)
	return 0;
      pat = PATTERN (insn);
    }

  /* See if this is, or ends in, a fnstsw, aka unspec 9.  If so, we're
     not doing anything with the cc value right now.  We may be able to
     search for one though.  */

  if (GET_CODE (pat) == SET
      && GET_CODE (SET_SRC (pat)) == UNSPEC
      && XINT (SET_SRC (pat), 1) == UNSPEC_FNSTSW)
    {
      rtx dest = SET_DEST (pat);

      /* Search forward looking for the first use of this value.
	 Stop at block boundaries.  */
      while (insn != current_block->end)
	{
	  insn = NEXT_INSN (insn);
	  if (INSN_P (insn) && reg_mentioned_p (dest, insn))
	    break;
	  if (GET_CODE (insn) == CALL_INSN)
	    return 0;
	}

      /* So we've found the insn using this value.  If it is anything
	 other than sahf, aka unspec 10, or the value does not die
	 (meaning we'd have to search further), then we must give up.  */
      pat = PATTERN (insn);
      if (GET_CODE (pat) != SET
	  || GET_CODE (SET_SRC (pat)) != UNSPEC
	  || XINT (SET_SRC (pat), 1) != UNSPEC_SAHF
	  || ! dead_or_set_p (insn, dest))
	return 0;

      /* Now we are prepared to handle this as a normal cc0 setter.  */
      insn = next_flags_user (insn);
      if (insn == NULL_RTX)
	return 0;
      pat = PATTERN (insn);
    }

  if (swap_rtx_condition_1 (pat))
    {
      int fail = 0;
      INSN_CODE (insn) = -1;
      if (recog_memoized (insn) == -1)
	fail = 1;
      /* In case the flags don't die here, recurse to try fix
         following user too.  */
      else if (! dead_or_set_p (insn, ix86_flags_rtx))
	{
	  insn = next_flags_user (insn);
	  if (!insn || !swap_rtx_condition (insn))
	    fail = 1;
	}
      if (fail)
	{
	  swap_rtx_condition_1 (pat);
	  return 0;
	}
      return 1;
    }
  return 0;
}

/* Handle a comparison.  Special care needs to be taken to avoid
   causing comparisons that a 387 cannot do correctly, such as EQ.

   Also, a pop insn may need to be emitted.  The 387 does have an
   `fcompp' insn that can pop two regs, but it is sometimes too expensive
   to do this - a `fcomp' followed by a `fstpl %st(0)' may be easier to
   set up.  */

static void
compare_for_stack_reg (insn, regstack, pat_src)
     rtx insn;
     stack regstack;
     rtx pat_src;
{
  rtx *src1, *src2;
  rtx src1_note, src2_note;
  rtx flags_user;

  src1 = get_true_reg (&XEXP (pat_src, 0));
  src2 = get_true_reg (&XEXP (pat_src, 1));
  flags_user = next_flags_user (insn);

  /* ??? If fxch turns out to be cheaper than fstp, give priority to
     registers that die in this insn - move those to stack top first.  */
  if ((! STACK_REG_P (*src1)
       || (STACK_REG_P (*src2)
	   && get_hard_regnum (regstack, *src2) == FIRST_STACK_REG))
      && swap_rtx_condition (insn))
    {
      rtx temp;
      temp = XEXP (pat_src, 0);
      XEXP (pat_src, 0) = XEXP (pat_src, 1);
      XEXP (pat_src, 1) = temp;

      src1 = get_true_reg (&XEXP (pat_src, 0));
      src2 = get_true_reg (&XEXP (pat_src, 1));

      INSN_CODE (insn) = -1;
    }

  /* We will fix any death note later.  */

  src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));

  if (STACK_REG_P (*src2))
    src2_note = find_regno_note (insn, REG_DEAD, REGNO (*src2));
  else
    src2_note = NULL_RTX;

  emit_swap_insn (insn, regstack, *src1);

  replace_reg (src1, FIRST_STACK_REG);

  if (STACK_REG_P (*src2))
    replace_reg (src2, get_hard_regnum (regstack, *src2));

  if (src1_note)
    {
      pop_stack (regstack, REGNO (XEXP (src1_note, 0)));
      replace_reg (&XEXP (src1_note, 0), FIRST_STACK_REG);
    }

  /* If the second operand dies, handle that.  But if the operands are
     the same stack register, don't bother, because only one death is
     needed, and it was just handled.  */

  if (src2_note
      && ! (STACK_REG_P (*src1) && STACK_REG_P (*src2)
	    && REGNO (*src1) == REGNO (*src2)))
    {
      /* As a special case, two regs may die in this insn if src2 is
	 next to top of stack and the top of stack also dies.  Since
	 we have already popped src1, "next to top of stack" is really
	 at top (FIRST_STACK_REG) now.  */

      if (get_hard_regnum (regstack, XEXP (src2_note, 0)) == FIRST_STACK_REG
	  && src1_note)
	{
	  pop_stack (regstack, REGNO (XEXP (src2_note, 0)));
	  replace_reg (&XEXP (src2_note, 0), FIRST_STACK_REG + 1);
	}
      else
	{
	  /* The 386 can only represent death of the first operand in
	     the case handled above.  In all other cases, emit a separate
	     pop and remove the death note from here.  */

	  /* link_cc0_insns (insn); */

	  remove_regno_note (insn, REG_DEAD, REGNO (XEXP (src2_note, 0)));

	  emit_pop_insn (insn, regstack, XEXP (src2_note, 0),
			 EMIT_AFTER);
	}
    }
}

/* Substitute new registers in PAT, which is part of INSN.  REGSTACK
   is the current register layout.  */

static void
subst_stack_regs_pat (insn, regstack, pat)
     rtx insn;
     stack regstack;
     rtx pat;
{
  rtx *dest, *src;

  switch (GET_CODE (pat))
    {
    case USE:
      /* Deaths in USE insns can happen in non optimizing compilation.
	 Handle them by popping the dying register.  */
      src = get_true_reg (&XEXP (pat, 0));
      if (STACK_REG_P (*src)
	  && find_regno_note (insn, REG_DEAD, REGNO (*src)))
	{
	  emit_pop_insn (insn, regstack, *src, EMIT_AFTER);
	  return;
	}
      /* ??? Uninitialized USE should not happen.  */
      else if (get_hard_regnum (regstack, *src) == -1)
	abort ();
      break;

    case CLOBBER:
      {
	rtx note;

	dest = get_true_reg (&XEXP (pat, 0));
	if (STACK_REG_P (*dest))
	  {
	    note = find_reg_note (insn, REG_DEAD, *dest);

	    if (pat != PATTERN (insn))
	      {
		/* The fix_truncdi_1 pattern wants to be able to allocate
		   it's own scratch register.  It does this by clobbering
		   an fp reg so that it is assured of an empty reg-stack
		   register.  If the register is live, kill it now.
		   Remove the DEAD/UNUSED note so we don't try to kill it
		   later too.  */

		if (note)
		  emit_pop_insn (insn, regstack, *dest, EMIT_BEFORE);
		else
		  {
		    note = find_reg_note (insn, REG_UNUSED, *dest);
		    if (!note)
		      abort ();
		  }
		remove_note (insn, note);
		replace_reg (dest, LAST_STACK_REG);
	      }
	    else
	      {
		/* A top-level clobber with no REG_DEAD, and no hard-regnum
		   indicates an uninitialized value.  Because reload removed
		   all other clobbers, this must be due to a function
		   returning without a value.  Load up a NaN.  */

		if (! note
		    && get_hard_regnum (regstack, *dest) == -1)
		  {
		    pat = gen_rtx_SET (VOIDmode,
				       FP_MODE_REG (REGNO (*dest), SFmode),
				       nan);
		    PATTERN (insn) = pat;
		    move_for_stack_reg (insn, regstack, pat);
		  }
		if (! note && COMPLEX_MODE_P (GET_MODE (*dest))
		    && get_hard_regnum (regstack, FP_MODE_REG (REGNO (*dest), DFmode)) == -1)
		  {
		    pat = gen_rtx_SET (VOIDmode,
				       FP_MODE_REG (REGNO (*dest) + 1, SFmode),
				       nan);
		    PATTERN (insn) = pat;
		    move_for_stack_reg (insn, regstack, pat);
		  }
	      }
	  }
	break;
      }

    case SET:
      {
	rtx *src1 = (rtx *) 0, *src2;
	rtx src1_note, src2_note;
	rtx pat_src;

	dest = get_true_reg (&SET_DEST (pat));
	src  = get_true_reg (&SET_SRC (pat));
	pat_src = SET_SRC (pat);

	/* See if this is a `movM' pattern, and handle elsewhere if so.  */
	if (STACK_REG_P (*src)
	    || (STACK_REG_P (*dest)
		&& (GET_CODE (*src) == REG || GET_CODE (*src) == MEM
		    || GET_CODE (*src) == CONST_DOUBLE)))
	  {
	    move_for_stack_reg (insn, regstack, pat);
	    break;
	  }

	switch (GET_CODE (pat_src))
	  {
	  case COMPARE:
	    compare_for_stack_reg (insn, regstack, pat_src);
	    break;

	  case CALL:
	    {
	      int count;
	      for (count = HARD_REGNO_NREGS (REGNO (*dest), GET_MODE (*dest));
		   --count >= 0;)
		{
		  regstack->reg[++regstack->top] = REGNO (*dest) + count;
		  SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest) + count);
		}
	    }
	    replace_reg (dest, FIRST_STACK_REG);
	    break;

	  case REG:
	    /* This is a `tstM2' case.  */
	    if (*dest != cc0_rtx)
	      abort ();
	    src1 = src;

	    /* Fall through.  */

	  case FLOAT_TRUNCATE:
	  case SQRT:
	  case ABS:
	  case NEG:
	    /* These insns only operate on the top of the stack. DEST might
	       be cc0_rtx if we're processing a tstM pattern. Also, it's
	       possible that the tstM case results in a REG_DEAD note on the
	       source.  */

	    if (src1 == 0)
	      src1 = get_true_reg (&XEXP (pat_src, 0));

	    emit_swap_insn (insn, regstack, *src1);

	    src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));

	    if (STACK_REG_P (*dest))
	      replace_reg (dest, FIRST_STACK_REG);

	    if (src1_note)
	      {
		replace_reg (&XEXP (src1_note, 0), FIRST_STACK_REG);
		regstack->top--;
		CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (*src1));
	      }

	    replace_reg (src1, FIRST_STACK_REG);
	    break;

	  case MINUS:
	  case DIV:
	    /* On i386, reversed forms of subM3 and divM3 exist for
	       MODE_FLOAT, so the same code that works for addM3 and mulM3
	       can be used.  */
	  case MULT:
	  case PLUS:
	    /* These insns can accept the top of stack as a destination
	       from a stack reg or mem, or can use the top of stack as a
	       source and some other stack register (possibly top of stack)
	       as a destination.  */

	    src1 = get_true_reg (&XEXP (pat_src, 0));
	    src2 = get_true_reg (&XEXP (pat_src, 1));

	    /* We will fix any death note later.  */

	    if (STACK_REG_P (*src1))
	      src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));
	    else
	      src1_note = NULL_RTX;
	    if (STACK_REG_P (*src2))
	      src2_note = find_regno_note (insn, REG_DEAD, REGNO (*src2));
	    else
	      src2_note = NULL_RTX;

	    /* If either operand is not a stack register, then the dest
	       must be top of stack.  */

	    if (! STACK_REG_P (*src1) || ! STACK_REG_P (*src2))
	      emit_swap_insn (insn, regstack, *dest);
	    else
	      {
		/* Both operands are REG.  If neither operand is already
		   at the top of stack, choose to make the one that is the dest
		   the new top of stack.  */

		int src1_hard_regnum, src2_hard_regnum;

		src1_hard_regnum = get_hard_regnum (regstack, *src1);
		src2_hard_regnum = get_hard_regnum (regstack, *src2);
		if (src1_hard_regnum == -1 || src2_hard_regnum == -1)
		  abort ();

		if (src1_hard_regnum != FIRST_STACK_REG
		    && src2_hard_regnum != FIRST_STACK_REG)
		  emit_swap_insn (insn, regstack, *dest);
	      }

	    if (STACK_REG_P (*src1))
	      replace_reg (src1, get_hard_regnum (regstack, *src1));
	    if (STACK_REG_P (*src2))
	      replace_reg (src2, get_hard_regnum (regstack, *src2));

	    if (src1_note)
	      {
		rtx src1_reg = XEXP (src1_note, 0);

		/* If the register that dies is at the top of stack, then
		   the destination is somewhere else - merely substitute it.
		   But if the reg that dies is not at top of stack, then
		   move the top of stack to the dead reg, as though we had
		   done the insn and then a store-with-pop.  */

		if (REGNO (src1_reg) == regstack->reg[regstack->top])
		  {
		    SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		    replace_reg (dest, get_hard_regnum (regstack, *dest));
		  }
		else
		  {
		    int regno = get_hard_regnum (regstack, src1_reg);

		    SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		    replace_reg (dest, regno);

		    regstack->reg[regstack->top - (regno - FIRST_STACK_REG)]
		      = regstack->reg[regstack->top];
		  }

		CLEAR_HARD_REG_BIT (regstack->reg_set,
				    REGNO (XEXP (src1_note, 0)));
		replace_reg (&XEXP (src1_note, 0), FIRST_STACK_REG);
		regstack->top--;
	      }
	    else if (src2_note)
	      {
		rtx src2_reg = XEXP (src2_note, 0);
		if (REGNO (src2_reg) == regstack->reg[regstack->top])
		  {
		    SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		    replace_reg (dest, get_hard_regnum (regstack, *dest));
		  }
		else
		  {
		    int regno = get_hard_regnum (regstack, src2_reg);

		    SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		    replace_reg (dest, regno);

		    regstack->reg[regstack->top - (regno - FIRST_STACK_REG)]
		      = regstack->reg[regstack->top];
		  }

		CLEAR_HARD_REG_BIT (regstack->reg_set,
				    REGNO (XEXP (src2_note, 0)));
		replace_reg (&XEXP (src2_note, 0), FIRST_STACK_REG);
		regstack->top--;
	      }
	    else
	      {
		SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		replace_reg (dest, get_hard_regnum (regstack, *dest));
	      }

	    /* Keep operand 1 maching with destination.  */
	    if (GET_RTX_CLASS (GET_CODE (pat_src)) == 'c'
		&& REG_P (*src1) && REG_P (*src2)
		&& REGNO (*src1) != REGNO (*dest))
	     {
		int tmp = REGNO (*src1);
		replace_reg (src1, REGNO (*src2));
		replace_reg (src2, tmp);
	     }
	    break;

	  case UNSPEC:
	    switch (XINT (pat_src, 1))
	      {
	      case UNSPEC_SIN:
	      case UNSPEC_COS:
		/* These insns only operate on the top of the stack.  */

		src1 = get_true_reg (&XVECEXP (pat_src, 0, 0));

		emit_swap_insn (insn, regstack, *src1);

		src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));

		if (STACK_REG_P (*dest))
		  replace_reg (dest, FIRST_STACK_REG);

		if (src1_note)
		  {
		    replace_reg (&XEXP (src1_note, 0), FIRST_STACK_REG);
		    regstack->top--;
		    CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (*src1));
		  }

		replace_reg (src1, FIRST_STACK_REG);
		break;

	      case UNSPEC_SAHF:
		/* (unspec [(unspec [(compare)] UNSPEC_FNSTSW)] UNSPEC_SAHF)
		   The combination matches the PPRO fcomi instruction.  */

		pat_src = XVECEXP (pat_src, 0, 0);
		if (GET_CODE (pat_src) != UNSPEC
		    || XINT (pat_src, 1) != UNSPEC_FNSTSW)
		  abort ();
		/* FALLTHRU */

	      case UNSPEC_FNSTSW:
		/* Combined fcomp+fnstsw generated for doing well with
		   CSE.  When optimizing this would have been broken
		   up before now.  */

		pat_src = XVECEXP (pat_src, 0, 0);
		if (GET_CODE (pat_src) != COMPARE)
		  abort ();

		compare_for_stack_reg (insn, regstack, pat_src);
		break;

	      default:
		abort ();
	      }
	    break;

	  case IF_THEN_ELSE:
	    /* This insn requires the top of stack to be the destination.  */

	    src1 = get_true_reg (&XEXP (pat_src, 1));
	    src2 = get_true_reg (&XEXP (pat_src, 2));

	    src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));
	    src2_note = find_regno_note (insn, REG_DEAD, REGNO (*src2));

	    /* If the comparison operator is an FP comparison operator,
	       it is handled correctly by compare_for_stack_reg () who
	       will move the destination to the top of stack. But if the
	       comparison operator is not an FP comparison operator, we
	       have to handle it here.  */
	    if (get_hard_regnum (regstack, *dest) >= FIRST_STACK_REG
		&& REGNO (*dest) != regstack->reg[regstack->top])
	      {
		/* In case one of operands is the top of stack and the operands
		   dies, it is safe to make it the destination operand by
		   reversing the direction of cmove and avoid fxch.  */
		if ((REGNO (*src1) == regstack->reg[regstack->top]
		     && src1_note)
		    || (REGNO (*src2) == regstack->reg[regstack->top]
			&& src2_note))
		  {
		    int idx1 = (get_hard_regnum (regstack, *src1)
				- FIRST_STACK_REG);
		    int idx2 = (get_hard_regnum (regstack, *src2)
				- FIRST_STACK_REG);

		    /* Make reg-stack believe that the operands are already
		       swapped on the stack */
		    regstack->reg[regstack->top - idx1] = REGNO (*src2);
		    regstack->reg[regstack->top - idx2] = REGNO (*src1);

		    /* Reverse condition to compensate the operand swap.
		       i386 do have comparison always reversible.  */
		    PUT_CODE (XEXP (pat_src, 0),
			      reversed_comparison_code (XEXP (pat_src, 0), insn));
		  }
		else
	          emit_swap_insn (insn, regstack, *dest);
	      }

	    {
	      rtx src_note [3];
	      int i;

	      src_note[0] = 0;
	      src_note[1] = src1_note;
	      src_note[2] = src2_note;

	      if (STACK_REG_P (*src1))
		replace_reg (src1, get_hard_regnum (regstack, *src1));
	      if (STACK_REG_P (*src2))
		replace_reg (src2, get_hard_regnum (regstack, *src2));

	      for (i = 1; i <= 2; i++)
		if (src_note [i])
		  {
		    int regno = REGNO (XEXP (src_note[i], 0));

		    /* If the register that dies is not at the top of
		       stack, then move the top of stack to the dead reg */
		    if (regno != regstack->reg[regstack->top])
		      {
			remove_regno_note (insn, REG_DEAD, regno);
			emit_pop_insn (insn, regstack, XEXP (src_note[i], 0),
				       EMIT_AFTER);
		      }
		    else
		      /* Top of stack never dies, as it is the
			 destination.  */
		      abort ();
		  }
	    }

	    /* Make dest the top of stack.  Add dest to regstack if
	       not present.  */
	    if (get_hard_regnum (regstack, *dest) < FIRST_STACK_REG)
	      regstack->reg[++regstack->top] = REGNO (*dest);
	    SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
	    replace_reg (dest, FIRST_STACK_REG);
	    break;

	  default:
	    abort ();
	  }
	break;
      }

    default:
      break;
    }
}

/* Substitute hard regnums for any stack regs in INSN, which has
   N_INPUTS inputs and N_OUTPUTS outputs.  REGSTACK is the stack info
   before the insn, and is updated with changes made here.

   There are several requirements and assumptions about the use of
   stack-like regs in asm statements.  These rules are enforced by
   record_asm_stack_regs; see comments there for details.  Any
   asm_operands left in the RTL at this point may be assume to meet the
   requirements, since record_asm_stack_regs removes any problem asm.  */

static void
subst_asm_stack_regs (insn, regstack)
     rtx insn;
     stack regstack;
{
  rtx body = PATTERN (insn);
  int alt;

  rtx *note_reg;		/* Array of note contents */
  rtx **note_loc;		/* Address of REG field of each note */
  enum reg_note *note_kind;	/* The type of each note */

  rtx *clobber_reg = 0;
  rtx **clobber_loc = 0;

  struct stack_def temp_stack;
  int n_notes;
  int n_clobbers;
  rtx note;
  int i;
  int n_inputs, n_outputs;

  if (! check_asm_stack_operands (insn))
    return;

  /* Find out what the constraints required.  If no constraint
     alternative matches, that is a compiler bug: we should have caught
     such an insn in check_asm_stack_operands.  */
  extract_insn (insn);
  constrain_operands (1);
  alt = which_alternative;

  preprocess_constraints ();

  n_inputs = get_asm_operand_n_inputs (body);
  n_outputs = recog_data.n_operands - n_inputs;

  if (alt < 0)
    abort ();

  /* Strip SUBREGs here to make the following code simpler.  */
  for (i = 0; i < recog_data.n_operands; i++)
    if (GET_CODE (recog_data.operand[i]) == SUBREG
	&& GET_CODE (SUBREG_REG (recog_data.operand[i])) == REG)
      {
	recog_data.operand_loc[i] = & SUBREG_REG (recog_data.operand[i]);
	recog_data.operand[i] = SUBREG_REG (recog_data.operand[i]);
      }

  /* Set up NOTE_REG, NOTE_LOC and NOTE_KIND.  */

  for (i = 0, note = REG_NOTES (insn); note; note = XEXP (note, 1))
    i++;

  note_reg = (rtx *) alloca (i * sizeof (rtx));
  note_loc = (rtx **) alloca (i * sizeof (rtx *));
  note_kind = (enum reg_note *) alloca (i * sizeof (enum reg_note));

  n_notes = 0;
  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
    {
      rtx reg = XEXP (note, 0);
      rtx *loc = & XEXP (note, 0);

      if (GET_CODE (reg) == SUBREG && GET_CODE (SUBREG_REG (reg)) == REG)
	{
	  loc = & SUBREG_REG (reg);
	  reg = SUBREG_REG (reg);
	}

      if (STACK_REG_P (reg)
	  && (REG_NOTE_KIND (note) == REG_DEAD
	      || REG_NOTE_KIND (note) == REG_UNUSED))
	{
	  note_reg[n_notes] = reg;
	  note_loc[n_notes] = loc;
	  note_kind[n_notes] = REG_NOTE_KIND (note);
	  n_notes++;
	}
    }

  /* Set up CLOBBER_REG and CLOBBER_LOC.  */

  n_clobbers = 0;

  if (GET_CODE (body) == PARALLEL)
    {
      clobber_reg = (rtx *) alloca (XVECLEN (body, 0) * sizeof (rtx));
      clobber_loc = (rtx **) alloca (XVECLEN (body, 0) * sizeof (rtx *));

      for (i = 0; i < XVECLEN (body, 0); i++)
	if (GET_CODE (XVECEXP (body, 0, i)) == CLOBBER)
	  {
	    rtx clobber = XVECEXP (body, 0, i);
	    rtx reg = XEXP (clobber, 0);
	    rtx *loc = & XEXP (clobber, 0);

	    if (GET_CODE (reg) == SUBREG && GET_CODE (SUBREG_REG (reg)) == REG)
	      {
		loc = & SUBREG_REG (reg);
		reg = SUBREG_REG (reg);
	      }

	    if (STACK_REG_P (reg))
	      {
		clobber_reg[n_clobbers] = reg;
		clobber_loc[n_clobbers] = loc;
		n_clobbers++;
	      }
	  }
    }

  temp_stack = *regstack;

  /* Put the input regs into the desired place in TEMP_STACK.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_data.operand[i])
	&& reg_class_subset_p (recog_op_alt[i][alt].class,
			       FLOAT_REGS)
	&& recog_op_alt[i][alt].class != FLOAT_REGS)
      {
	/* If an operand needs to be in a particular reg in
	   FLOAT_REGS, the constraint was either 't' or 'u'.  Since
	   these constraints are for single register classes, and
	   reload guaranteed that operand[i] is already in that class,
	   we can just use REGNO (recog_data.operand[i]) to know which
	   actual reg this operand needs to be in.  */

	int regno = get_hard_regnum (&temp_stack, recog_data.operand[i]);

	if (regno < 0)
	  abort ();

	if ((unsigned int) regno != REGNO (recog_data.operand[i]))
	  {
	    /* recog_data.operand[i] is not in the right place.  Find
	       it and swap it with whatever is already in I's place.
	       K is where recog_data.operand[i] is now.  J is where it
	       should be.  */
	    int j, k, temp;

	    k = temp_stack.top - (regno - FIRST_STACK_REG);
	    j = (temp_stack.top
		 - (REGNO (recog_data.operand[i]) - FIRST_STACK_REG));

	    temp = temp_stack.reg[k];
	    temp_stack.reg[k] = temp_stack.reg[j];
	    temp_stack.reg[j] = temp;
	  }
      }

  /* Emit insns before INSN to make sure the reg-stack is in the right
     order.  */

  change_stack (insn, regstack, &temp_stack, EMIT_BEFORE);

  /* Make the needed input register substitutions.  Do death notes and
     clobbers too, because these are for inputs, not outputs.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	int regnum = get_hard_regnum (regstack, recog_data.operand[i]);

	if (regnum < 0)
	  abort ();

	replace_reg (recog_data.operand_loc[i], regnum);
      }

  for (i = 0; i < n_notes; i++)
    if (note_kind[i] == REG_DEAD)
      {
	int regnum = get_hard_regnum (regstack, note_reg[i]);

	if (regnum < 0)
	  abort ();

	replace_reg (note_loc[i], regnum);
      }

  for (i = 0; i < n_clobbers; i++)
    {
      /* It's OK for a CLOBBER to reference a reg that is not live.
         Don't try to replace it in that case.  */
      int regnum = get_hard_regnum (regstack, clobber_reg[i]);

      if (regnum >= 0)
	{
	  /* Sigh - clobbers always have QImode.  But replace_reg knows
	     that these regs can't be MODE_INT and will abort.  Just put
	     the right reg there without calling replace_reg.  */

	  *clobber_loc[i] = FP_MODE_REG (regnum, DFmode);
	}
    }

  /* Now remove from REGSTACK any inputs that the asm implicitly popped.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	/* An input reg is implicitly popped if it is tied to an
	   output, or if there is a CLOBBER for it.  */
	int j;

	for (j = 0; j < n_clobbers; j++)
	  if (operands_match_p (clobber_reg[j], recog_data.operand[i]))
	    break;

	if (j < n_clobbers || recog_op_alt[i][alt].matches >= 0)
	  {
	    /* recog_data.operand[i] might not be at the top of stack.
	       But that's OK, because all we need to do is pop the
	       right number of regs off of the top of the reg-stack.
	       record_asm_stack_regs guaranteed that all implicitly
	       popped regs were grouped at the top of the reg-stack.  */

	    CLEAR_HARD_REG_BIT (regstack->reg_set,
				regstack->reg[regstack->top]);
	    regstack->top--;
	  }
      }

  /* Now add to REGSTACK any outputs that the asm implicitly pushed.
     Note that there isn't any need to substitute register numbers.
     ???  Explain why this is true.  */

  for (i = LAST_STACK_REG; i >= FIRST_STACK_REG; i--)
    {
      /* See if there is an output for this hard reg.  */
      int j;

      for (j = 0; j < n_outputs; j++)
	if (STACK_REG_P (recog_data.operand[j])
	    && REGNO (recog_data.operand[j]) == (unsigned) i)
	  {
	    regstack->reg[++regstack->top] = i;
	    SET_HARD_REG_BIT (regstack->reg_set, i);
	    break;
	  }
    }

  /* Now emit a pop insn for any REG_UNUSED output, or any REG_DEAD
     input that the asm didn't implicitly pop.  If the asm didn't
     implicitly pop an input reg, that reg will still be live.

     Note that we can't use find_regno_note here: the register numbers
     in the death notes have already been substituted.  */

  for (i = 0; i < n_outputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	int j;

	for (j = 0; j < n_notes; j++)
	  if (REGNO (recog_data.operand[i]) == REGNO (note_reg[j])
	      && note_kind[j] == REG_UNUSED)
	    {
	      insn = emit_pop_insn (insn, regstack, recog_data.operand[i],
				    EMIT_AFTER);
	      break;
	    }
      }

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_data.operand[i]))
      {
	int j;

	for (j = 0; j < n_notes; j++)
	  if (REGNO (recog_data.operand[i]) == REGNO (note_reg[j])
	      && note_kind[j] == REG_DEAD
	      && TEST_HARD_REG_BIT (regstack->reg_set,
				    REGNO (recog_data.operand[i])))
	    {
	      insn = emit_pop_insn (insn, regstack, recog_data.operand[i],
				    EMIT_AFTER);
	      break;
	    }
      }
}

/* Substitute stack hard reg numbers for stack virtual registers in
   INSN.  Non-stack register numbers are not changed.  REGSTACK is the
   current stack content.  Insns may be emitted as needed to arrange the
   stack for the 387 based on the contents of the insn.  */

static void
subst_stack_regs (insn, regstack)
     rtx insn;
     stack regstack;
{
  rtx *note_link, note;
  int i;

  if (GET_CODE (insn) == CALL_INSN)
    {
      int top = regstack->top;

      /* If there are any floating point parameters to be passed in
	 registers for this call, make sure they are in the right
	 order.  */

      if (top >= 0)
	{
	  straighten_stack (PREV_INSN (insn), regstack);

	  /* Now mark the arguments as dead after the call.  */

	  while (regstack->top >= 0)
	    {
	      CLEAR_HARD_REG_BIT (regstack->reg_set, FIRST_STACK_REG + regstack->top);
	      regstack->top--;
	    }
	}
    }

  /* Do the actual substitution if any stack regs are mentioned.
     Since we only record whether entire insn mentions stack regs, and
     subst_stack_regs_pat only works for patterns that contain stack regs,
     we must check each pattern in a parallel here.  A call_value_pop could
     fail otherwise.  */

  if (stack_regs_mentioned (insn))
    {
      int n_operands = asm_noperands (PATTERN (insn));
      if (n_operands >= 0)
	{
	  /* This insn is an `asm' with operands.  Decode the operands,
	     decide how many are inputs, and do register substitution.
	     Any REG_UNUSED notes will be handled by subst_asm_stack_regs.  */

	  subst_asm_stack_regs (insn, regstack);
	  return;
	}

      if (GET_CODE (PATTERN (insn)) == PARALLEL)
	for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	  {
	    if (stack_regs_mentioned_p (XVECEXP (PATTERN (insn), 0, i)))
	      subst_stack_regs_pat (insn, regstack,
				    XVECEXP (PATTERN (insn), 0, i));
	  }
      else
	subst_stack_regs_pat (insn, regstack, PATTERN (insn));
    }

  /* subst_stack_regs_pat may have deleted a no-op insn.  If so, any
     REG_UNUSED will already have been dealt with, so just return.  */

  if (GET_CODE (insn) == NOTE || INSN_DELETED_P (insn))
    return;

  /* If there is a REG_UNUSED note on a stack register on this insn,
     the indicated reg must be popped.  The REG_UNUSED note is removed,
     since the form of the newly emitted pop insn references the reg,
     making it no longer `unset'.  */

  note_link = &REG_NOTES (insn);
  for (note = *note_link; note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_UNUSED && STACK_REG_P (XEXP (note, 0)))
      {
	*note_link = XEXP (note, 1);
	insn = emit_pop_insn (insn, regstack, XEXP (note, 0), EMIT_AFTER);
      }
    else
      note_link = &XEXP (note, 1);
}

/* Change the organization of the stack so that it fits a new basic
   block.  Some registers might have to be popped, but there can never be
   a register live in the new block that is not now live.

   Insert any needed insns before or after INSN, as indicated by
   WHERE.  OLD is the original stack layout, and NEW is the desired
   form.  OLD is updated to reflect the code emitted, ie, it will be
   the same as NEW upon return.

   This function will not preserve block_end[].  But that information
   is no longer needed once this has executed.  */

static void
change_stack (insn, old, new, where)
     rtx insn;
     stack old;
     stack new;
     enum emit_where where;
{
  int reg;
  int update_end = 0;

  /* We will be inserting new insns "backwards".  If we are to insert
     after INSN, find the next insn, and insert before it.  */

  if (where == EMIT_AFTER)
    {
      if (current_block && current_block->end == insn)
	update_end = 1;
      insn = NEXT_INSN (insn);
    }

  /* Pop any registers that are not needed in the new block.  */

  for (reg = old->top; reg >= 0; reg--)
    if (! TEST_HARD_REG_BIT (new->reg_set, old->reg[reg]))
      emit_pop_insn (insn, old, FP_MODE_REG (old->reg[reg], DFmode),
		     EMIT_BEFORE);

  if (new->top == -2)
    {
      /* If the new block has never been processed, then it can inherit
	 the old stack order.  */

      new->top = old->top;
      memcpy (new->reg, old->reg, sizeof (new->reg));
    }
  else
    {
      /* This block has been entered before, and we must match the
	 previously selected stack order.  */

      /* By now, the only difference should be the order of the stack,
	 not their depth or liveliness.  */

      GO_IF_HARD_REG_EQUAL (old->reg_set, new->reg_set, win);
      abort ();
    win:
      if (old->top != new->top)
	abort ();

      /* If the stack is not empty (new->top != -1), loop here emitting
	 swaps until the stack is correct.

	 The worst case number of swaps emitted is N + 2, where N is the
	 depth of the stack.  In some cases, the reg at the top of
	 stack may be correct, but swapped anyway in order to fix
	 other regs.  But since we never swap any other reg away from
	 its correct slot, this algorithm will converge.  */

      if (new->top != -1)
	do
	  {
	    /* Swap the reg at top of stack into the position it is
	       supposed to be in, until the correct top of stack appears.  */

	    while (old->reg[old->top] != new->reg[new->top])
	      {
		for (reg = new->top; reg >= 0; reg--)
		  if (new->reg[reg] == old->reg[old->top])
		    break;

		if (reg == -1)
		  abort ();

		emit_swap_insn (insn, old,
				FP_MODE_REG (old->reg[reg], DFmode));
	      }

	    /* See if any regs remain incorrect.  If so, bring an
	     incorrect reg to the top of stack, and let the while loop
	     above fix it.  */

	    for (reg = new->top; reg >= 0; reg--)
	      if (new->reg[reg] != old->reg[reg])
		{
		  emit_swap_insn (insn, old,
				  FP_MODE_REG (old->reg[reg], DFmode));
		  break;
		}
	  } while (reg >= 0);

      /* At this point there must be no differences.  */

      for (reg = old->top; reg >= 0; reg--)
	if (old->reg[reg] != new->reg[reg])
	  abort ();
    }

  if (update_end)
    current_block->end = PREV_INSN (insn);
}

/* Print stack configuration.  */

static void
print_stack (file, s)
     FILE *file;
     stack s;
{
  if (! file)
    return;

  if (s->top == -2)
    fprintf (file, "uninitialized\n");
  else if (s->top == -1)
    fprintf (file, "empty\n");
  else
    {
      int i;
      fputs ("[ ", file);
      for (i = 0; i <= s->top; ++i)
	fprintf (file, "%d ", s->reg[i]);
      fputs ("]\n", file);
    }
}

/* This function was doing life analysis.  We now let the regular live
   code do it's job, so we only need to check some extra invariants
   that reg-stack expects.  Primary among these being that all registers
   are initialized before use.

   The function returns true when code was emitted to CFG edges and
   commit_edge_insertions needs to be called.  */

static int
convert_regs_entry ()
{
  int inserted = 0;
  edge e;
  basic_block block;

  FOR_EACH_BB_REVERSE (block)
    {
      block_info bi = BLOCK_INFO (block);
      int reg;

      /* Set current register status at last instruction `uninitialized'.  */
      bi->stack_in.top = -2;

      /* Copy live_at_end and live_at_start into temporaries.  */
      for (reg = FIRST_STACK_REG; reg <= LAST_STACK_REG; reg++)
	{
	  if (REGNO_REG_SET_P (block->global_live_at_end, reg))
	    SET_HARD_REG_BIT (bi->out_reg_set, reg);
	  if (REGNO_REG_SET_P (block->global_live_at_start, reg))
	    SET_HARD_REG_BIT (bi->stack_in.reg_set, reg);
	}
    }

  /* Load something into each stack register live at function entry.
     Such live registers can be caused by uninitialized variables or
     functions not returning values on all paths.  In order to keep
     the push/pop code happy, and to not scrog the register stack, we
     must put something in these registers.  Use a QNaN.

     Note that we are insertting converted code here.  This code is
     never seen by the convert_regs pass.  */

  for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
    {
      basic_block block = e->dest;
      block_info bi = BLOCK_INFO (block);
      int reg, top = -1;

      for (reg = LAST_STACK_REG; reg >= FIRST_STACK_REG; --reg)
	if (TEST_HARD_REG_BIT (bi->stack_in.reg_set, reg))
	  {
	    rtx init;

	    bi->stack_in.reg[++top] = reg;

	    init = gen_rtx_SET (VOIDmode,
				FP_MODE_REG (FIRST_STACK_REG, SFmode),
				nan);
	    insert_insn_on_edge (init, e);
	    inserted = 1;
	  }

      bi->stack_in.top = top;
    }

  return inserted;
}

/* Construct the desired stack for function exit.  This will either
   be `empty', or the function return value at top-of-stack.  */

static void
convert_regs_exit ()
{
  int value_reg_low, value_reg_high;
  stack output_stack;
  rtx retvalue;

  retvalue = stack_result (current_function_decl);
  value_reg_low = value_reg_high = -1;
  if (retvalue)
    {
      value_reg_low = REGNO (retvalue);
      value_reg_high = value_reg_low
	+ HARD_REGNO_NREGS (value_reg_low, GET_MODE (retvalue)) - 1;
    }

  output_stack = &BLOCK_INFO (EXIT_BLOCK_PTR)->stack_in;
  if (value_reg_low == -1)
    output_stack->top = -1;
  else
    {
      int reg;

      output_stack->top = value_reg_high - value_reg_low;
      for (reg = value_reg_low; reg <= value_reg_high; ++reg)
	{
	  output_stack->reg[value_reg_high - reg] = reg;
	  SET_HARD_REG_BIT (output_stack->reg_set, reg);
	}
    }
}

/* Adjust the stack of this block on exit to match the stack of the
   target block, or copy stack info into the stack of the successor
   of the successor hasn't been processed yet.  */
static bool
compensate_edge (e, file)
    edge e;
    FILE *file;
{
  basic_block block = e->src, target = e->dest;
  block_info bi = BLOCK_INFO (block);
  struct stack_def regstack, tmpstack;
  stack target_stack = &BLOCK_INFO (target)->stack_in;
  int reg;

  current_block = block;
  regstack = bi->stack_out;
  if (file)
    fprintf (file, "Edge %d->%d: ", block->index, target->index);

  if (target_stack->top == -2)
    {
      /* The target block hasn't had a stack order selected.
         We need merely ensure that no pops are needed.  */
      for (reg = regstack.top; reg >= 0; --reg)
	if (!TEST_HARD_REG_BIT (target_stack->reg_set, regstack.reg[reg]))
	  break;

      if (reg == -1)
	{
	  if (file)
	    fprintf (file, "new block; copying stack position\n");

	  /* change_stack kills values in regstack.  */
	  tmpstack = regstack;

	  change_stack (block->end, &tmpstack, target_stack, EMIT_AFTER);
	  return false;
	}

      if (file)
	fprintf (file, "new block; pops needed\n");
    }
  else
    {
      if (target_stack->top == regstack.top)
	{
	  for (reg = target_stack->top; reg >= 0; --reg)
	    if (target_stack->reg[reg] != regstack.reg[reg])
	      break;

	  if (reg == -1)
	    {
	      if (file)
		fprintf (file, "no changes needed\n");
	      return false;
	    }
	}

      if (file)
	{
	  fprintf (file, "correcting stack to ");
	  print_stack (file, target_stack);
	}
    }

  /* Care for non-call EH edges specially.  The normal return path have
     values in registers.  These will be popped en masse by the unwind
     library.  */
  if ((e->flags & (EDGE_EH | EDGE_ABNORMAL_CALL)) == EDGE_EH)
    target_stack->top = -1;

  /* Other calls may appear to have values live in st(0), but the
     abnormal return path will not have actually loaded the values.  */
  else if (e->flags & EDGE_ABNORMAL_CALL)
    {
      /* Assert that the lifetimes are as we expect -- one value
         live at st(0) on the end of the source block, and no
         values live at the beginning of the destination block.  */
      HARD_REG_SET tmp;

      CLEAR_HARD_REG_SET (tmp);
      GO_IF_HARD_REG_EQUAL (target_stack->reg_set, tmp, eh1);
      abort ();
    eh1:

      /* We are sure that there is st(0) live, otherwise we won't compensate.
	 For complex return values, we may have st(1) live as well.  */
      SET_HARD_REG_BIT (tmp, FIRST_STACK_REG);
      if (TEST_HARD_REG_BIT (regstack.reg_set, FIRST_STACK_REG + 1))
        SET_HARD_REG_BIT (tmp, FIRST_STACK_REG + 1);
      GO_IF_HARD_REG_EQUAL (regstack.reg_set, tmp, eh2);
      abort ();
    eh2:

      target_stack->top = -1;
    }

  /* It is better to output directly to the end of the block
     instead of to the edge, because emit_swap can do minimal
     insn scheduling.  We can do this when there is only one
     edge out, and it is not abnormal.  */
  else if (block->succ->succ_next == NULL && !(e->flags & EDGE_ABNORMAL))
    {
      /* change_stack kills values in regstack.  */
      tmpstack = regstack;

      change_stack (block->end, &tmpstack, target_stack,
		    (GET_CODE (block->end) == JUMP_INSN
		     ? EMIT_BEFORE : EMIT_AFTER));
    }
  else
    {
      rtx seq, after;

      /* We don't support abnormal edges.  Global takes care to
         avoid any live register across them, so we should never
         have to insert instructions on such edges.  */
      if (e->flags & EDGE_ABNORMAL)
	abort ();

      current_block = NULL;
      start_sequence ();

      /* ??? change_stack needs some point to emit insns after.  */
      after = emit_note (NULL, NOTE_INSN_DELETED);

      tmpstack = regstack;
      change_stack (after, &tmpstack, target_stack, EMIT_BEFORE);

      seq = get_insns ();
      end_sequence ();

      insert_insn_on_edge (seq, e);
      return true;
    }
  return false;
}

/* Convert stack register references in one block.  */

static int
convert_regs_1 (file, block)
     FILE *file;
     basic_block block;
{
  struct stack_def regstack;
  block_info bi = BLOCK_INFO (block);
  int inserted, reg;
  rtx insn, next;
  edge e, beste = NULL;

  inserted = 0;
  any_malformed_asm = false;

  /* Find the edge we will copy stack from.  It should be the most frequent
     one as it will get cheapest after compensation code is generated,
     if multiple such exists, take one with largest count, prefer critical
     one (as splitting critical edges is more expensive), or one with lowest
     index, to avoid random changes with different orders of the edges.  */
  for (e = block->pred; e ; e = e->pred_next)
    {
      if (e->flags & EDGE_DFS_BACK)
	;
      else if (! beste)
	beste = e;
      else if (EDGE_FREQUENCY (beste) < EDGE_FREQUENCY (e))
	beste = e;
      else if (EDGE_FREQUENCY (beste) > EDGE_FREQUENCY (e))
	;
      else if (beste->count < e->count)
	beste = e;
      else if (beste->count > e->count)
	;
      else if ((EDGE_CRITICAL_P (e) != 0)
	       != (EDGE_CRITICAL_P (beste) != 0))
	{
	  if (EDGE_CRITICAL_P (e))
	    beste = e;
	}
      else if (e->src->index < beste->src->index)
	beste = e;
    }

  /* Entry block does have stack already initialized.  */
  if (bi->stack_in.top == -2)
    inserted |= compensate_edge (beste, file);
  else
    beste = NULL;

  current_block = block;

  if (file)
    {
      fprintf (file, "\nBasic block %d\nInput stack: ", block->index);
      print_stack (file, &bi->stack_in);
    }

  /* Process all insns in this block.  Keep track of NEXT so that we
     don't process insns emitted while substituting in INSN.  */
  next = block->head;
  regstack = bi->stack_in;
  do
    {
      insn = next;
      next = NEXT_INSN (insn);

      /* Ensure we have not missed a block boundary.  */
      if (next == NULL)
	abort ();
      if (insn == block->end)
	next = NULL;

      /* Don't bother processing unless there is a stack reg
	 mentioned or if it's a CALL_INSN.  */
      if (stack_regs_mentioned (insn)
	  || GET_CODE (insn) == CALL_INSN)
	{
	  if (file)
	    {
	      fprintf (file, "  insn %d input stack: ",
		       INSN_UID (insn));
	      print_stack (file, &regstack);
	    }
	  subst_stack_regs (insn, &regstack);
	}
    }
  while (next);

  if (file)
    {
      fprintf (file, "Expected live registers [");
      for (reg = FIRST_STACK_REG; reg <= LAST_STACK_REG; ++reg)
	if (TEST_HARD_REG_BIT (bi->out_reg_set, reg))
	  fprintf (file, " %d", reg);
      fprintf (file, " ]\nOutput stack: ");
      print_stack (file, &regstack);
    }

  insn = block->end;
  if (GET_CODE (insn) == JUMP_INSN)
    insn = PREV_INSN (insn);

  /* If the function is declared to return a value, but it returns one
     in only some cases, some registers might come live here.  Emit
     necessary moves for them.  */

  for (reg = FIRST_STACK_REG; reg <= LAST_STACK_REG; ++reg)
    {
      if (TEST_HARD_REG_BIT (bi->out_reg_set, reg)
	  && ! TEST_HARD_REG_BIT (regstack.reg_set, reg))
	{
	  rtx set;

	  if (file)
	    {
	      fprintf (file, "Emitting insn initializing reg %d\n",
		       reg);
	    }

	  set = gen_rtx_SET (VOIDmode, FP_MODE_REG (reg, SFmode),
			     nan);
	  insn = emit_insn_after (set, insn);
	  subst_stack_regs (insn, &regstack);
	}
    }

  /* Something failed if the stack lives don't match.  If we had malformed
     asms, we zapped the instruction itself, but that didn't produce the
     same pattern of register kills as before.  */
  GO_IF_HARD_REG_EQUAL (regstack.reg_set, bi->out_reg_set, win);
  if (!any_malformed_asm)
    abort ();
 win:
  bi->stack_out = regstack;

  /* Compensate the back edges, as those wasn't visited yet.  */
  for (e = block->succ; e ; e = e->succ_next)
    {
      if (e->flags & EDGE_DFS_BACK
	  || (e->dest == EXIT_BLOCK_PTR))
	{
	  if (!BLOCK_INFO (e->dest)->done
	      && e->dest != block)
	    abort ();
	  inserted |= compensate_edge (e, file);
	}
    }
  for (e = block->pred; e ; e = e->pred_next)
    {
      if (e != beste && !(e->flags & EDGE_DFS_BACK)
	  && e->src != ENTRY_BLOCK_PTR)
	{
	  if (!BLOCK_INFO (e->src)->done)
	    abort ();
	  inserted |= compensate_edge (e, file);
	}
    }

  return inserted;
}

/* Convert registers in all blocks reachable from BLOCK.  */

static int
convert_regs_2 (file, block)
     FILE *file;
     basic_block block;
{
  basic_block *stack, *sp;
  int inserted;

  stack = (basic_block *) xmalloc (sizeof (*stack) * n_basic_blocks);
  sp = stack;

  *sp++ = block;

  inserted = 0;
  do
    {
      edge e;

      block = *--sp;
      inserted |= convert_regs_1 (file, block);
      BLOCK_INFO (block)->done = 1;

      for (e = block->succ; e ; e = e->succ_next)
	if (! (e->flags & EDGE_DFS_BACK))
	  {
	    BLOCK_INFO (e->dest)->predecessors--;
	    if (!BLOCK_INFO (e->dest)->predecessors)
	       *sp++ = e->dest;
	  }
    }
  while (sp != stack);

  return inserted;
}

/* Traverse all basic blocks in a function, converting the register
   references in each insn from the "flat" register file that gcc uses,
   to the stack-like registers the 387 uses.  */

static int
convert_regs (file)
     FILE *file;
{
  int inserted;
  basic_block b;
  edge e;

  /* Initialize uninitialized registers on function entry.  */
  inserted = convert_regs_entry ();

  /* Construct the desired stack for function exit.  */
  convert_regs_exit ();
  BLOCK_INFO (EXIT_BLOCK_PTR)->done = 1;

  /* ??? Future: process inner loops first, and give them arbitrary
     initial stacks which emit_swap_insn can modify.  This ought to
     prevent double fxch that aften appears at the head of a loop.  */

  /* Process all blocks reachable from all entry points.  */
  for (e = ENTRY_BLOCK_PTR->succ; e ; e = e->succ_next)
    inserted |= convert_regs_2 (file, e->dest);

  /* ??? Process all unreachable blocks.  Though there's no excuse
     for keeping these even when not optimizing.  */
  FOR_EACH_BB (b)
    {
      block_info bi = BLOCK_INFO (b);

      if (! bi->done)
	{
	  int reg;

	  /* Create an arbitrary input stack.  */
	  bi->stack_in.top = -1;
	  for (reg = LAST_STACK_REG; reg >= FIRST_STACK_REG; --reg)
	    if (TEST_HARD_REG_BIT (bi->stack_in.reg_set, reg))
	      bi->stack_in.reg[++bi->stack_in.top] = reg;

	  inserted |= convert_regs_2 (file, b);
	}
    }
  clear_aux_for_blocks ();

  fixup_abnormal_edges ();
  if (inserted)
    commit_edge_insertions ();

  if (file)
    fputc ('\n', file);

  return inserted;
}
#endif /* STACK_REGS */

#include "gt-reg-stack.h"
