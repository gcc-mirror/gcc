/* Register to Stack convert for GNU compiler.
   Copyright (C) 1992, 93-98, 1999 Free Software Foundation, Inc.

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
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "insn-flags.h"
#include "recog.h"
#include "toplev.h"
#include "varray.h"

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
  char reg[REG_STACK_SIZE];	/* register - stack mapping */
} *stack;

/* highest instruction uid */
static int max_uid = 0;

/* Number of basic blocks in the current function.  */
static int blocks;

/* Element N is first insn in basic block N.
   This info lasts until we finish compiling the function.  */
static rtx *block_begin;

/* Element N is last insn in basic block N.
   This info lasts until we finish compiling the function.  */
static rtx *block_end;

/* Element N is nonzero if control can drop into basic block N */
static char *block_drops_in;

/* Element N says all about the stack at entry block N */
static stack block_stack_in;

/* Element N says all about the stack life at the end of block N */
static HARD_REG_SET *block_out_reg_set;

/* This is where the BLOCK_NUM values are really stored.  This is set
   up by find_blocks and used there and in life_analysis.  It can be used
   later, but only to look up an insn that is the head or tail of some
   block.  life_analysis and the stack register conversion process can
   add insns within a block.  */
static int *block_number;

/* We use this array to cache info about insns, because otherwise we
   spend too much time in stack_regs_mentioned_p. 

   Indexed by insn UIDs.  A value of zero is uninitialized, one indicates
   the insn uses stack registers, two indicates the insn does not use
   stack registers.  */
static varray_type stack_regs_mentioned_data;

/* This is the register file for all register after conversion */
static rtx
  FP_mode_reg[LAST_STACK_REG+1-FIRST_STACK_REG][(int) MAX_MACHINE_MODE];

#define FP_MODE_REG(regno,mode)	\
  (FP_mode_reg[(regno)-FIRST_STACK_REG][(int)(mode)])

/* Get the basic block number of an insn.  See note at block_number
   definition are validity of this information.  */

static int BLOCK_NUM PROTO((rtx));

#ifdef __GNUC__
__inline__
#endif
static int
BLOCK_NUM(insn)
     rtx insn;
{
  int tmp = INSN_UID (insn);
  if (tmp > max_uid)
    abort ();
  tmp = block_number[tmp];
  if (tmp < 0)
    abort ();
  return tmp;
}

extern rtx forced_labels;

/* Forward declarations */

static void mark_regs_pat		PROTO((rtx, HARD_REG_SET *));
static void straighten_stack		PROTO((rtx, stack));
static void pop_stack			PROTO((stack, int));
static void record_label_references	PROTO((rtx, rtx));
static rtx *get_true_reg		PROTO((rtx *));

static void record_asm_reg_life		PROTO((rtx, stack));
static void record_reg_life_pat		PROTO((rtx, HARD_REG_SET *,
					       HARD_REG_SET *, int));
static int get_asm_operand_n_inputs	PROTO((rtx));
static void record_reg_life		PROTO((rtx, int, stack));
static void find_blocks			PROTO((rtx));
static rtx stack_result			PROTO((tree));
static void stack_reg_life_analysis	PROTO((rtx, HARD_REG_SET *));
static void replace_reg			PROTO((rtx *, int));
static void remove_regno_note		PROTO((rtx, enum reg_note, int));
static int get_hard_regnum		PROTO((stack, rtx));
static void delete_insn_for_stacker	PROTO((rtx));
static rtx emit_pop_insn		PROTO((rtx, stack, rtx, rtx (*) ()));
static void emit_swap_insn		PROTO((rtx, stack, rtx));
static void move_for_stack_reg		PROTO((rtx, stack, rtx));
static void swap_rtx_condition		PROTO((rtx));
static void compare_for_stack_reg	PROTO((rtx, stack, rtx));
static void subst_stack_regs_pat	PROTO((rtx, stack, rtx));
static void subst_asm_stack_regs	PROTO((rtx, stack));
static void subst_stack_regs		PROTO((rtx, stack));
static void change_stack		PROTO((rtx, stack, stack, rtx (*) ()));

static void goto_block_pat		PROTO((rtx, stack, rtx));
static void convert_regs		PROTO((void));
static void print_blocks		PROTO((FILE *, rtx, rtx));
static void dump_stack_info		PROTO((FILE *));
static int check_stack_regs_mentioned	PROTO((rtx insn));

/* Initialize stack_regs_mentioned_data for INSN (growing the virtual array
   if needed.  Return nonzero if INSN mentions stacked registers.  */

static int
check_stack_regs_mentioned (insn)
     rtx insn;
{
  unsigned int uid = INSN_UID (insn);
  if (uid >= VARRAY_SIZE (stack_regs_mentioned_data))
    /* Allocate some extra size to avoid too many reallocs, but
       do not grow exponentially.  */
    VARRAY_GROW (stack_regs_mentioned_data, uid + uid / 20);
  if (stack_regs_mentioned_p (PATTERN (insn)))
    {
      VARRAY_CHAR (stack_regs_mentioned_data, uid) = 1;
      return 1;
    }
  else
    VARRAY_CHAR (stack_regs_mentioned_data, uid) = 2;
  return 0;
}

/* Return nonzero if INSN mentions stacked registers, else return
   zero.  */

int
stack_regs_mentioned (insn)
     rtx insn;
{
  unsigned int uid;
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;
  uid = INSN_UID (insn);
  if (uid >= VARRAY_SIZE (stack_regs_mentioned_data)
      || ! VARRAY_CHAR (stack_regs_mentioned_data, uid))
    return (check_stack_regs_mentioned (insn));
  return VARRAY_CHAR (stack_regs_mentioned_data, uid) == 1;
}


/* Mark all registers needed for this pattern.  */

static void
mark_regs_pat (pat, set)
     rtx pat;
     HARD_REG_SET *set;
{
  enum machine_mode mode;
  register int regno;
  register int count;

  if (GET_CODE (pat) == SUBREG)
   {
     mode = GET_MODE (pat);
     regno = SUBREG_WORD (pat);
     regno += REGNO (SUBREG_REG (pat));
   }
  else
     regno = REGNO (pat), mode = GET_MODE (pat);

  for (count = HARD_REGNO_NREGS (regno, mode);
       count; count--, regno++)
     SET_HARD_REG_BIT (*set, regno);
}

/* Reorganise the stack into ascending numbers,
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

  temp_stack.reg_set = regstack->reg_set;

  for (top = temp_stack.top = regstack->top; top >= 0; top--)
     temp_stack.reg[top] = FIRST_STACK_REG + temp_stack.top - top;
  
  change_stack (insn, regstack, &temp_stack, emit_insn_after);
}

/* Pop a register from the stack */

static void
pop_stack (regstack, regno)
     stack regstack;
     int   regno;
{
  int top = regstack->top;

  CLEAR_HARD_REG_BIT (regstack->reg_set, regno);
  regstack->top--;
  /* If regno was not at the top of stack then adjust stack */
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

/* Return non-zero if any stack register is mentioned somewhere within PAT.  */

int
stack_regs_mentioned_p (pat)
     rtx pat;
{
  register char *fmt;
  register int i;

  if (STACK_REG_P (pat))
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
	    if (stack_regs_mentioned_p (XVECEXP (pat, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && stack_regs_mentioned_p (XEXP (pat, i)))
	return 1;
    }

  return 0;
}

/* Convert register usage from "flat" register file usage to a "stack
   register file.  FIRST is the first insn in the function, FILE is the
   dump file, if used.

   First compute the beginning and end of each basic block.  Do a
   register life analysis on the stack registers, recording the result
   for the head and tail of each basic block.  The convert each insn one
   by one.  Run a last jump_optimize() pass, if optimizing, to eliminate
   any cross-jumping created when the converter inserts pop insns.*/

void
reg_to_stack (first, file)
     rtx first;
     FILE *file;
{
  register rtx insn;
  register int i;
  int stack_reg_seen = 0;
  enum machine_mode mode;
  HARD_REG_SET stackentry;

  max_uid = get_max_uid ();
  VARRAY_CHAR_INIT (stack_regs_mentioned_data, max_uid + 1,
		    "stack_regs_mentioned cache");

  CLEAR_HARD_REG_SET (stackentry);

   {
     static int initialised;
     if (!initialised)
      {
#if 0
	initialised = 1;	/* This array can not have been previously
				   initialised, because the rtx's are
				   thrown away between compilations of
				   functions.  */
#endif
        for (i = FIRST_STACK_REG; i <= LAST_STACK_REG; i++)
         {
           for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); mode != VOIDmode;
               mode = GET_MODE_WIDER_MODE (mode))
              FP_MODE_REG (i, mode) = gen_rtx_REG (mode, i);
           for (mode = GET_CLASS_NARROWEST_MODE (MODE_COMPLEX_FLOAT); mode != VOIDmode;
               mode = GET_MODE_WIDER_MODE (mode))
              FP_MODE_REG (i, mode) = gen_rtx_REG (mode, i);
         }
      }
   }

  /* Count the basic blocks.  Also find maximum insn uid.  */
  {
    register RTX_CODE prev_code = BARRIER;
    register RTX_CODE code;
    register int before_function_beg = 1;

    max_uid = 0;
    blocks = 0;
    for (insn = first; insn; insn = NEXT_INSN (insn))
      {
	/* Note that this loop must select the same block boundaries
	   as code in find_blocks.  Also note that this code is not the
	   same as that used in flow.c.  */

	if (INSN_UID (insn) > max_uid)
	  max_uid = INSN_UID (insn);

	code = GET_CODE (insn);

	if (code == CODE_LABEL
	    || (prev_code != INSN
		&& prev_code != CALL_INSN
		&& prev_code != CODE_LABEL
		&& GET_RTX_CLASS (code) == 'i'))
	  blocks++;

	if (code == NOTE && NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
	   before_function_beg = 0;

	/* Remember whether or not this insn mentions an FP regs.
	   Check JUMP_INSNs too, in case someone creates a funny PARALLEL.  */

	if (GET_RTX_CLASS (code) == 'i'
	    && stack_regs_mentioned_p (PATTERN (insn)))
	  {
	    stack_reg_seen = 1;
	    VARRAY_CHAR (stack_regs_mentioned_data, INSN_UID (insn)) = 1;

	    /* Note any register passing parameters.  */

	    if (before_function_beg && code == INSN
	        && GET_CODE (PATTERN (insn)) == USE)
              record_reg_life_pat (PATTERN (insn), (HARD_REG_SET *) 0,
				   &stackentry, 1);
	  }
	else
	  VARRAY_CHAR (stack_regs_mentioned_data, INSN_UID (insn)) = 2;

	if (code == CODE_LABEL)
	  LABEL_REFS (insn) = insn; /* delete old chain */

	if (code != NOTE)
	  prev_code = code;
      }
  }

  /* If no stack register reference exists in this insn, there isn't
     anything to convert.  */

  if (! stack_reg_seen)
    {
      VARRAY_FREE (stack_regs_mentioned_data);
      return;
    }

  /* If there are stack registers, there must be at least one block.  */

  if (! blocks)
    abort ();

  /* Allocate some tables that last till end of compiling this function
     and some needed only in find_blocks and life_analysis.  */

  block_begin = (rtx *) alloca (blocks * sizeof (rtx));
  block_end = (rtx *) alloca (blocks * sizeof (rtx));
  block_drops_in = (char *) alloca (blocks);

  block_stack_in = (stack) alloca (blocks * sizeof (struct stack_def));
  block_out_reg_set = (HARD_REG_SET *) alloca (blocks * sizeof (HARD_REG_SET));
  bzero ((char *) block_stack_in, blocks * sizeof (struct stack_def));
  bzero ((char *) block_out_reg_set, blocks * sizeof (HARD_REG_SET));

  block_number = (int *) alloca ((max_uid + 1) * sizeof (int));
  memset (block_number, -1, (max_uid + 1) * sizeof (int));

  find_blocks (first);
  stack_reg_life_analysis (first, &stackentry);

  /* Dump the life analysis debug information before jump
     optimization, as that will destroy the LABEL_REFS we keep the
     information in.  */

  if (file)
    dump_stack_info (file);

  convert_regs ();

  if (optimize)
    jump_optimize (first, 2, 0, 0);

  VARRAY_FREE (stack_regs_mentioned_data);
}

/* Check PAT, which is in INSN, for LABEL_REFs.  Add INSN to the
   label's chain of references, and note which insn contains each
   reference.  */

static void
record_label_references (insn, pat)
     rtx insn, pat;
{
  register enum rtx_code code = GET_CODE (pat);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (pat, 0);
      register rtx ref;

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
	  register int j;
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
		/* eliminate FP subregister accesses in favour of the
		   actual FP register in use.  */
	 {
	   rtx subreg;
	   if (FP_REG_P (subreg = SUBREG_REG (*pat)))
	    {
	      *pat = FP_MODE_REG (REGNO (subreg) + SUBREG_WORD (*pat),
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

/* Record the life info of each stack reg in INSN, updating REGSTACK.
   N_INPUTS is the number of inputs; N_OUTPUTS the outputs.
   OPERANDS is an array of all operands for the insn, and is assumed to
   contain all output operands, then all inputs operands.

   There are many rules that an asm statement for stack-like regs must
   follow.  Those rules are explained at the top of this file: the rule
   numbers below refer to that explanation.  */

static void
record_asm_reg_life (insn, regstack)
     rtx insn;
     stack regstack;
{
  int i;
  int n_clobbers;
  int malformed_asm = 0;
  rtx body = PATTERN (insn);

  int reg_used_as_output[FIRST_PSEUDO_REGISTER];
  int implicitly_dies[FIRST_PSEUDO_REGISTER];
  int alt;

  rtx *clobber_reg;
  int n_inputs, n_outputs;

  /* Find out what the constraints require.  If no constraint
     alternative matches, this asm is malformed.  */
  extract_insn (insn);
  constrain_operands (1);
  alt = which_alternative;

  preprocess_constraints ();

  n_inputs = get_asm_operand_n_inputs (body);
  n_outputs = recog_n_operands - n_inputs;

  if (alt < 0)
    {
      malformed_asm = 1;
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx_USE (VOIDmode, const0_rtx);
      VARRAY_CHAR (stack_regs_mentioned_data, INSN_UID (insn)) = 2;
      return;
    }

  /* Strip SUBREGs here to make the following code simpler.  */
  for (i = 0; i < recog_n_operands; i++)
    if (GET_CODE (recog_operand[i]) == SUBREG
	&& GET_CODE (SUBREG_REG (recog_operand[i])) == REG)
      recog_operand[i] = SUBREG_REG (recog_operand[i]);

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

  bzero ((char *) reg_used_as_output, sizeof (reg_used_as_output));
  for (i = 0; i < n_outputs; i++)
    if (STACK_REG_P (recog_operand[i]))
      {
	if (reg_class_size[(int) recog_op_alt[i][alt].class] != 1)
	  {
	    error_for_asm (insn, "Output constraint %d must specify a single register", i);
	    malformed_asm = 1;
	  }
        else
	  reg_used_as_output[REGNO (recog_operand[i])] = 1;
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
      error_for_asm (insn, "Output regs must be grouped at top of stack");
      malformed_asm = 1;
    }

  /* Enforce rule #2: All implicitly popped input regs must be closer
     to the top of the reg-stack than any input that is not implicitly
     popped.  */

  bzero ((char *) implicitly_dies, sizeof (implicitly_dies));
  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_operand[i]))
      {
	/* An input reg is implicitly popped if it is tied to an
	   output, or if there is a CLOBBER for it.  */
	int j;

	for (j = 0; j < n_clobbers; j++)
	  if (operands_match_p (clobber_reg[j], recog_operand[i]))
	    break;

	if (j < n_clobbers || recog_op_alt[i][alt].matches >= 0)
	  implicitly_dies[REGNO (recog_operand[i])] = 1;
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
		     "Implicitly popped regs must be grouped at top of stack");
      malformed_asm = 1;
    }

  /* Enfore rule #3: If any input operand uses the "f" constraint, all
     output constraints must use the "&" earlyclobber.

     ???  Detect this more deterministically by having constraint_asm_operands
     record any earlyclobber.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (recog_op_alt[i][alt].matches == -1)
      {
	int j;

	for (j = 0; j < n_outputs; j++)
	  if (operands_match_p (recog_operand[j], recog_operand[i]))
	    {
	      error_for_asm (insn,
			     "Output operand %d must use `&' constraint", j);
	      malformed_asm = 1;
	    }
      }

  if (malformed_asm)
    {
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx_USE (VOIDmode, const0_rtx);
      VARRAY_CHAR (stack_regs_mentioned_data, INSN_UID (insn)) = 2;
      return;
    }

  /* Process all outputs */
  for (i = 0; i < n_outputs; i++)
    {
      rtx op = recog_operand[i];

      if (! STACK_REG_P (op))
	{
	  if (stack_regs_mentioned_p (op))
	    abort ();
	  else
	    continue;
	}

      /* Each destination is dead before this insn.  If the
	 destination is not used after this insn, record this with
	 REG_UNUSED.  */

      if (! TEST_HARD_REG_BIT (regstack->reg_set, REGNO (op)))
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_UNUSED, op,
					      REG_NOTES (insn));

      CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (op));
    }

  /* Process all inputs */
  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    {
      rtx op = recog_operand[i];
      if (! STACK_REG_P (op))
	{
	  if (stack_regs_mentioned_p (op))
	    abort ();
	  else
	    continue;
	}

      /* If an input is dead after the insn, record a death note.
	 But don't record a death note if there is already a death note,
	 or if the input is also an output.  */

      if (! TEST_HARD_REG_BIT (regstack->reg_set, REGNO (op))
	  && recog_op_alt[i][alt].matches == -1
	  && find_regno_note (insn, REG_DEAD, REGNO (op)) == NULL_RTX)
	REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_DEAD, op, REG_NOTES (insn));

      SET_HARD_REG_BIT (regstack->reg_set, REGNO (op));
    }
}

/* Scan PAT, which is part of INSN, and record registers appearing in
   a SET_DEST in DEST, and other registers in SRC.

   This function does not know about SET_DESTs that are both input and
   output (such as ZERO_EXTRACT) - this cannot happen on a 387.  */

static void
record_reg_life_pat (pat, src, dest, douse)
     rtx pat;
     HARD_REG_SET *src, *dest;
     int douse;
{
  register char *fmt;
  register int i;

  if (STACK_REG_P (pat)
      || (GET_CODE (pat) == SUBREG && STACK_REG_P (SUBREG_REG (pat))))
    {
      if (src)
	 mark_regs_pat (pat, src);

      if (dest)
	 mark_regs_pat (pat, dest);

      return;
    }

  if (GET_CODE (pat) == SET)
    {
      record_reg_life_pat (XEXP (pat, 0), NULL_PTR, dest, 0);
      record_reg_life_pat (XEXP (pat, 1), src, NULL_PTR, 0);
      return;
    }

  /* We don't need to consider either of these cases.  */
  if ((GET_CODE (pat) == USE && !douse) || GET_CODE (pat) == CLOBBER)
    return;

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
	    record_reg_life_pat (XVECEXP (pat, i, j), src, dest, 0);
	}
      else if (fmt[i] == 'e')
	record_reg_life_pat (XEXP (pat, i), src, dest, 0);
    }
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

/* Scan INSN, which is in BLOCK, and record the life & death of stack
   registers in REGSTACK.  This function is called to process insns from
   the last insn in a block to the first.  The actual scanning is done in
   record_reg_life_pat.

   If a register is live after a CALL_INSN, but is not a value return
   register for that CALL_INSN, then code is emitted to initialize that
   register.  The block_end[] data is kept accurate.

   Existing death and unset notes for stack registers are deleted
   before processing the insn.  */

static void
record_reg_life (insn, block, regstack)
     rtx insn;
     int block;
     stack regstack;
{
  rtx note, *note_link;
  int n_operands;

  if ((GET_CODE (insn) != INSN && GET_CODE (insn) != CALL_INSN)
      || INSN_DELETED_P (insn))
    return;

  /* Strip death notes for stack regs from this insn */

  note_link = &REG_NOTES(insn);
  for (note = *note_link; note; note = XEXP (note, 1))
    if (STACK_REG_P (XEXP (note, 0))
	&& (REG_NOTE_KIND (note) == REG_DEAD
	    || REG_NOTE_KIND (note) == REG_UNUSED))
      *note_link = XEXP (note, 1);
    else
      note_link = &XEXP (note, 1);

  /* Process all patterns in the insn.  */

  n_operands = asm_noperands (PATTERN (insn));
  if (n_operands >= 0)
    {
      record_asm_reg_life (insn, regstack);
      return;
    }

    {
      HARD_REG_SET src, dest;
      int regno;

      CLEAR_HARD_REG_SET (src);
      CLEAR_HARD_REG_SET (dest);

      if (GET_CODE (insn) == CALL_INSN)
	 for (note = CALL_INSN_FUNCTION_USAGE (insn);
	      note;
	      note = XEXP (note, 1))
	   if (GET_CODE (XEXP (note, 0)) == USE)
	     record_reg_life_pat (SET_DEST (XEXP (note, 0)), &src, NULL_PTR, 0);

      record_reg_life_pat (PATTERN (insn), &src, &dest, 0);
      for (regno = FIRST_STACK_REG; regno <= LAST_STACK_REG; regno++)
	if (! TEST_HARD_REG_BIT (regstack->reg_set, regno))
	  {
	    if (TEST_HARD_REG_BIT (src, regno)
		&& ! TEST_HARD_REG_BIT (dest, regno))
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_DEAD,
						    FP_MODE_REG (regno, DFmode),
						    REG_NOTES (insn));
	    else if (TEST_HARD_REG_BIT (dest, regno))
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_UNUSED,
						    FP_MODE_REG (regno, DFmode),
						    REG_NOTES (insn));
	  }

      if (GET_CODE (insn) == CALL_INSN)
        {
	  int reg;

          /* There might be a reg that is live after a function call.
             Initialize it to zero so that the program does not crash.  See
	     comment towards the end of stack_reg_life_analysis().  */

          for (reg = FIRST_STACK_REG; reg <= LAST_STACK_REG; reg++)
	    if (! TEST_HARD_REG_BIT (dest, reg)
	        && TEST_HARD_REG_BIT (regstack->reg_set, reg))
	      {
	        rtx init, pat;

	        /* The insn will use virtual register numbers, and so
	           convert_regs is expected to process these.  But BLOCK_NUM
	           cannot be used on these insns, because they do not appear in
	           block_number[].  */

	        pat = gen_rtx_SET (VOIDmode, FP_MODE_REG (reg, DFmode),
				   CONST0_RTX (DFmode));
	        init = emit_insn_after (pat, insn);

	        CLEAR_HARD_REG_BIT (regstack->reg_set, reg);

	        /* If the CALL_INSN was the end of a block, move the
	           block_end to point to the new insn.  */

	        if (block_end[block] == insn)
	          block_end[block] = init;
	      }

	  /* Some regs do not survive a CALL */
          AND_COMPL_HARD_REG_SET (regstack->reg_set, call_used_reg_set);
	}

      AND_COMPL_HARD_REG_SET (regstack->reg_set, dest);
      IOR_HARD_REG_SET (regstack->reg_set, src);
    }
}

/* Find all basic blocks of the function, which starts with FIRST.
   For each JUMP_INSN, build the chain of LABEL_REFS on each CODE_LABEL.  */

static void
find_blocks (first)
     rtx first;
{
  register rtx insn;
  register int block;
  register RTX_CODE prev_code = BARRIER;
  register RTX_CODE code;
  rtx label_value_list = 0;

  /* Record where all the blocks start and end.
     Record which basic blocks control can drop in to.  */

  block = -1;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      /* Note that this loop must select the same block boundaries
	 as code in reg_to_stack, but that these are not the same
	 as those selected in flow.c.  */

      code = GET_CODE (insn);

      if (code == CODE_LABEL
	  || (prev_code != INSN
	      && prev_code != CALL_INSN
	      && prev_code != CODE_LABEL
	      && GET_RTX_CLASS (code) == 'i'))
	{
	  block_begin[++block] = insn;
	  block_end[block] = insn;
	  block_drops_in[block] = prev_code != BARRIER;
	}
      else if (GET_RTX_CLASS (code) == 'i')
	block_end[block] = insn;

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx note;

	  /* Make a list of all labels referred to other than by jumps.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_LABEL)
	      label_value_list = gen_rtx_EXPR_LIST (VOIDmode, XEXP (note, 0),
						    label_value_list);
	}

      block_number[INSN_UID (insn)] = block;

      if (code != NOTE)
	prev_code = code;
    }

  if (block + 1 != blocks)
    abort ();

  /* generate all label references to the corresponding jump insn */
  for (block = 0; block < blocks; block++)
    {
      insn = block_end[block];

      if (GET_CODE (insn) == JUMP_INSN)
	{
	  rtx pat = PATTERN (insn);
	  rtx x;

	  if (computed_jump_p (insn))
	    {
	      for (x = label_value_list; x; x = XEXP (x, 1))
		record_label_references (insn,
					 gen_rtx_LABEL_REF (VOIDmode,
							    XEXP (x, 0)));

	      for (x = forced_labels; x; x = XEXP (x, 1))
		record_label_references (insn,
					 gen_rtx_LABEL_REF (VOIDmode,
							    XEXP (x, 0)));
	    }

	  record_label_references (insn, pat);
	}
    }
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

  result = DECL_RTL (DECL_RESULT (decl));
  /* ?!?  What is this code supposed to do?  Can this code actually
     trigger if we kick out aggregates above?  */
  if (result != 0
      && ! (GET_CODE (result) == REG
	    && REGNO (result) < FIRST_PSEUDO_REGISTER))
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

/* Determine the which registers are live at the start of each basic
   block of the function whose first insn is FIRST.

   First, if the function returns a real_type, mark the function
   return type as live at each return point, as the RTL may not give any
   hint that the register is live.

   Then, start with the last block and work back to the first block.
   Similarly, work backwards within each block, insn by insn, recording
   which regs are dead and which are used (and therefore live) in the
   hard reg set of block_stack_in[].

   After processing each basic block, if there is a label at the start
   of the block, propagate the live registers to all jumps to this block.

   As a special case, if there are regs live in this block, that are
   not live in a block containing a jump to this label, and the block
   containing the jump has already been processed, we must propagate this
   block's entry register life back to the block containing the jump, and
   restart life analysis from there.

   In the worst case, this function may traverse the insns
   REG_STACK_SIZE times.  This is necessary, since a jump towards the end
   of the insns may not know that a reg is live at a target that is early
   in the insns.  So we back up and start over with the new reg live.

   If there are registers that are live at the start of the function,
   insns are emitted to initialize these registers.  Something similar is
   done after CALL_INSNs in record_reg_life.  */

static void
stack_reg_life_analysis (first, stackentry)
     rtx first;
     HARD_REG_SET *stackentry;
{
  int reg, block;
  struct stack_def regstack;

   {
     rtx retvalue;

     if ((retvalue = stack_result (current_function_decl)))
      {
        /* Find all RETURN insns and mark them.  */

        for (block = blocks - 1; --block >= 0;)
	   if (GET_CODE (block_end[block]) == JUMP_INSN
	       && returnjump_p (block_end[block]))
	      mark_regs_pat (retvalue, block_out_reg_set+block);

        /* Mark off the end of last block if we "fall off" the end of the
	   function into the epilogue.  */

        if (GET_CODE (block_end[blocks-1]) != JUMP_INSN
	    || returnjump_p (block_end[blocks-1]))
	  mark_regs_pat (retvalue, block_out_reg_set+blocks-1);
      }
   }

  /* now scan all blocks backward for stack register use */

  block = blocks - 1;
  while (block >= 0)
    {
      register rtx insn, prev;

      /* current register status at last instruction */

      COPY_HARD_REG_SET (regstack.reg_set, block_out_reg_set[block]);

      prev = block_end[block];
      do
	{
	  insn = prev;
	  prev = PREV_INSN (insn);

	  /* If the insn is a CALL_INSN, we need to ensure that
	     everything dies.  But otherwise don't process unless there
	     are some stack regs present.  */

	  if (stack_regs_mentioned (insn) || GET_CODE (insn) == CALL_INSN)
	    record_reg_life (insn, block, &regstack);

	} while (insn != block_begin[block]);

      /* Set the state at the start of the block.  Mark that no
	 register mapping information known yet.  */

      COPY_HARD_REG_SET (block_stack_in[block].reg_set, regstack.reg_set);
      block_stack_in[block].top = -2;

      /* If there is a label, propagate our register life to all jumps
	 to this label.  */

      if (GET_CODE (insn) == CODE_LABEL)
	{
	  register rtx label;
	  int must_restart = 0;

	  for (label = LABEL_REFS (insn); label != insn;
	       label = LABEL_NEXTREF (label))
	    {
	      int jump_block = BLOCK_NUM (CONTAINING_INSN (label));

	      if (jump_block < block)
		IOR_HARD_REG_SET (block_out_reg_set[jump_block],
				  block_stack_in[block].reg_set);
	      else
		{
		  /* The block containing the jump has already been
		     processed.  If there are registers that were not known
		     to be live then, but are live now, we must back up
		     and restart life analysis from that point with the new
		     life information.  */

		  GO_IF_HARD_REG_SUBSET (block_stack_in[block].reg_set,
					 block_out_reg_set[jump_block],
					 win);

		  IOR_HARD_REG_SET (block_out_reg_set[jump_block],
				    block_stack_in[block].reg_set);

		  block = jump_block;
		  must_restart = 1;
		  break;

		win:
		  ;
		}
	    }
	  if (must_restart)
	    continue;
	}

      if (block_drops_in[block])
	IOR_HARD_REG_SET (block_out_reg_set[block-1],
			  block_stack_in[block].reg_set);

      block -= 1;
    }

    /* If any reg is live at the start of the first block of a
       function, then we must guarantee that the reg holds some value by
       generating our own "load" of that register.  Otherwise a 387 would
       fault trying to access an empty register.  */

  /* Load zero into each live register.  The fact that a register
     appears live at the function start necessarily implies an error
     in the user program: it means that (unless the offending code is *never*
     executed) this program is using uninitialised floating point
     variables.  In order to keep broken code like this happy, we initialise
     those variables with zero.

     Note that we are inserting virtual register references here:
     these insns must be processed by convert_regs later.  Also, these
     insns will not be in block_number, so BLOCK_NUM() will fail for them.  */

  for (reg = LAST_STACK_REG; reg >= FIRST_STACK_REG; reg--)
    if (TEST_HARD_REG_BIT (block_stack_in[0].reg_set, reg)
        && ! TEST_HARD_REG_BIT (*stackentry, reg))
      {
	rtx init_rtx;

	init_rtx = gen_rtx_SET (VOIDmode, FP_MODE_REG(reg, DFmode),
				CONST0_RTX (DFmode));
	block_begin[0] = emit_insn_after (init_rtx, first);

	CLEAR_HARD_REG_BIT (block_stack_in[0].reg_set, reg);
      }
}

/*****************************************************************************
   This section deals with stack register substitution, and forms the second
   pass over the RTL.
 *****************************************************************************/

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
     int regno;
{
  register rtx *note_link, this;

  note_link = &REG_NOTES(insn);
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

/* Delete INSN from the RTL.  Mark the insn, but don't remove it from
   the chain of insns.  Doing so could confuse block_begin and block_end
   if this were the only insn in the block.  */

static void
delete_insn_for_stacker (insn)
     rtx insn;
{
  int i;

  /* Ensure that the side effects were clobbers when deleting a PARALLEL.  */
  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    for (i = 1; i < XVECLEN (PATTERN (insn), 0); i++)
      if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) != CLOBBER)
	abort ();

  PUT_CODE (insn, NOTE);
  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (insn) = 0;
}

/* Emit an insn to pop virtual register REG before or after INSN.
   REGSTACK is the stack state after INSN and is updated to reflect this
   pop.  WHEN is either emit_insn_before, emit_insn_after or NULL. 
   in case WHEN is NULL we don't really emit the insn, just modify stack 
   information.  Caller is expected to emit insn himself.

   A pop insn is represented as a SET whose destination is the register to
   be popped and source is the top of stack.  A death note for the top of stack
   cases the movdf pattern to pop.  */

static rtx
emit_pop_insn (insn, regstack, reg, when)
     rtx insn;
     stack regstack;
     rtx reg;
     rtx (*when)();
{
  rtx pop_insn, pop_rtx;
  int hard_regno;

  hard_regno = get_hard_regnum (regstack, reg);

  if (hard_regno < FIRST_STACK_REG)
    abort ();

  if (when)
   {
     pop_rtx = gen_rtx_SET (VOIDmode, FP_MODE_REG (hard_regno, DFmode),
			    FP_MODE_REG (FIRST_STACK_REG, DFmode));

     pop_insn = (*when) (pop_rtx, insn);

     REG_NOTES (pop_insn) = gen_rtx_EXPR_LIST (REG_DEAD,
					       FP_MODE_REG (FIRST_STACK_REG,
							    DFmode),
					       REG_NOTES (pop_insn));
   }

  regstack->reg[regstack->top - (hard_regno - FIRST_STACK_REG)]
    = regstack->reg[regstack->top];
  regstack->top -= 1;
  CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (reg));

  return pop_insn;
}

/* Emit an insn before or after INSN to swap virtual register REG with the
   top of stack.  WHEN should be `emit_insn_before' or `emit_insn_before'
   REGSTACK is the stack state before the swap, and is updated to reflect
   the swap.  A swap insn is represented as a PARALLEL of two patterns:
   each pattern moves one reg to the other.

   If REG is already at the top of the stack, no insn is emitted.  */

static void
emit_swap_insn (insn, regstack, reg)
     rtx insn;
     stack regstack;
     rtx reg;
{
  int hard_regno;
  rtx gen_swapdf();
  rtx swap_rtx, swap_insn;
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

  /* Find the previous insn involving stack regs, but don't go past
     any labels, calls or jumps.  */
  i1 = prev_nonnote_insn (insn);
  while (i1 && GET_CODE (i1) == INSN && !stack_regs_mentioned (i1))
    i1 = prev_nonnote_insn (i1);

  if (i1)
    i1set = single_set (i1);

  if (i1set)
    {
      rtx i1src = *get_true_reg (&SET_SRC (i1set));
      rtx i1dest = *get_true_reg (&SET_DEST (i1set));

      /* If the previous register stack push was from the reg we are to
	 swap with, omit the swap.  */

      if (GET_CODE (i1dest) == REG && REGNO (i1dest) == FIRST_STACK_REG
	  && GET_CODE (i1src) == REG && REGNO (i1src) == hard_regno - 1
	  && find_regno_note (i1, REG_DEAD, FIRST_STACK_REG) == NULL_RTX)
	return;

      /* If the previous insn wrote to the reg we are to swap with,
	 omit the swap.  */

      if (GET_CODE (i1dest) == REG && REGNO (i1dest) == hard_regno
	  && GET_CODE (i1src) == REG && REGNO (i1src) == FIRST_STACK_REG
	  && find_regno_note (i1, REG_DEAD, FIRST_STACK_REG) == NULL_RTX)
	return;
    }

  if (GET_RTX_CLASS (GET_CODE (i1)) == 'i' && sets_cc0_p (PATTERN (i1)))
    {
      i1 = next_nonnote_insn (i1);
      if (i1 == insn)
	abort ();
    }

  swap_rtx = gen_swapdf (FP_MODE_REG (hard_regno, DFmode),
			 FP_MODE_REG (FIRST_STACK_REG, DFmode));
  swap_insn = emit_insn_after (swap_rtx, i1);
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
	      emit_pop_insn (insn, regstack, src, emit_insn_after);

	      delete_insn_for_stacker (insn);
	      return;
	    }

	  regstack->reg[i] = REGNO (dest);

	  SET_HARD_REG_BIT (regstack->reg_set, REGNO (dest));
	  CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (src));

	  delete_insn_for_stacker (insn);

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
	    emit_pop_insn (insn, regstack, dest, emit_insn_after);

	  delete_insn_for_stacker (insn);
	  return;
	}

      /* The destination ought to be dead */
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
      else if (GET_MODE (src) == XFmode && regstack->top < REG_STACK_SIZE - 1)
	{
	  /* A 387 cannot write an XFmode value to a MEM without
	     clobbering the source reg.  The output code can handle
	     this by reading back the value from the MEM.
	     But it is more efficient to use a temp register if one is
	     available.  Push the source value here if the register
	     stack is not full, and then write the value to memory via
	     a pop.  */
	  rtx push_rtx, push_insn;
	  rtx top_stack_reg = FP_MODE_REG (FIRST_STACK_REG, XFmode);

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

      /* The destination ought to be dead */
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

static void
swap_rtx_condition (pat)
     rtx pat;
{
  register char *fmt;
  register int i;

  if (GET_RTX_CLASS (GET_CODE (pat)) == '<')
    {
      PUT_CODE (pat, swap_condition (GET_CODE (pat)));
      return;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;

	  for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
	    swap_rtx_condition (XVECEXP (pat, i, j));
	}
      else if (fmt[i] == 'e')
	swap_rtx_condition (XEXP (pat, i));
    }
}

/* Handle a comparison.  Special care needs to be taken to avoid
   causing comparisons that a 387 cannot do correctly, such as EQ.

   Also, a fstp instruction may need to be emitted.  The 387 does have an
   `fcompp' insn that can pop two regs, but it is sometimes too expensive
   to do this - a `fcomp' followed by a `fstpl %st(0)' may be easier to
   set up. 
 
   We can not handle this by emiting fpop instruction after compare, because
   it appears between cc0 setter and user.  So we emit only
   REG_DEAD note and handle it as a special case in machine description.
 
   This code used trick with delay_slot filling to emit pop insn after
   comparsion but it didn't worked because it caused confusion with cc_status
   in final pass. */

static void
compare_for_stack_reg (insn, regstack, pat)
     rtx insn;
     stack regstack;
     rtx pat;
{
  rtx *src1, *src2;
  rtx src1_note, src2_note;
  rtx cc0_user;
  int have_cmove; 
  int hard_regno;

  src1 = get_true_reg (&XEXP (SET_SRC (pat), 0));
  src2 = get_true_reg (&XEXP (SET_SRC (pat), 1));
  cc0_user = next_cc0_user (insn);

  /* If the insn that uses cc0 is an FP-conditional move, then the destination
     must be the top of stack */
  if (GET_CODE (PATTERN (cc0_user)) == SET
      && SET_DEST (PATTERN (cc0_user)) != pc_rtx
      && GET_CODE (SET_SRC (PATTERN (cc0_user))) == IF_THEN_ELSE
      && (GET_MODE_CLASS (GET_MODE (SET_DEST (PATTERN (cc0_user))))
	  == MODE_FLOAT))
    {
      rtx *dest;
      
      dest = get_true_reg (&SET_DEST (PATTERN (cc0_user)));

      have_cmove = 1;
      if (get_hard_regnum (regstack, *dest) >= FIRST_STACK_REG
	  && REGNO (*dest) != regstack->reg[regstack->top])
	{
	  emit_swap_insn (insn, regstack, *dest);	
	}
    }
  else
    have_cmove = 0;

  /* ??? If fxch turns out to be cheaper than fstp, give priority to
     registers that die in this insn - move those to stack top first.  */
  if (! STACK_REG_P (*src1)
      || (STACK_REG_P (*src2)
	  && get_hard_regnum (regstack, *src2) == FIRST_STACK_REG))
    {
      rtx temp, next;

      temp = XEXP (SET_SRC (pat), 0);
      XEXP (SET_SRC (pat), 0) = XEXP (SET_SRC (pat), 1);
      XEXP (SET_SRC (pat), 1) = temp;

      src1 = get_true_reg (&XEXP (SET_SRC (pat), 0));
      src2 = get_true_reg (&XEXP (SET_SRC (pat), 1));

      next = next_cc0_user (insn);
      if (next == NULL_RTX)
	abort ();

      swap_rtx_condition (PATTERN (next));
      INSN_CODE (next) = -1;
      INSN_CODE (insn) = -1;
    }

  /* We will fix any death note later.  */

  src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));

  if (STACK_REG_P (*src2))
    src2_note = find_regno_note (insn, REG_DEAD, REGNO (*src2));
  else
    src2_note = NULL_RTX;

  if (! have_cmove)
     emit_swap_insn (insn, regstack, *src1);

  replace_reg (src1, FIRST_STACK_REG);

  if (STACK_REG_P (*src2))
    {
      hard_regno = get_hard_regnum (regstack, *src2);
      replace_reg (src2, hard_regno);
    }

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
	  /* Pop of second operand is handled using special REG_DEAD note
	     because we can't emit pop insn after cc0 setter.  */

	  emit_pop_insn (insn, regstack, XEXP (src2_note, 0), NULL);
	  replace_reg (&XEXP (src2_note, 0), hard_regno);
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
  rtx *src1 = (rtx *) NULL_PTR, *src2;
  rtx src1_note, src2_note;

  if (GET_CODE (pat) != SET)
    return;

  dest = get_true_reg (&SET_DEST (pat));
  src  = get_true_reg (&SET_SRC (pat));

  /* See if this is a `movM' pattern, and handle elsewhere if so.  */

  if (*dest != cc0_rtx
      && (STACK_REG_P (*src)
	  || (STACK_REG_P (*dest)
	      && (GET_CODE (*src) == REG || GET_CODE (*src) == MEM
		  || GET_CODE (*src) == CONST_DOUBLE))))
    move_for_stack_reg (insn, regstack, pat);
  else
    switch (GET_CODE (SET_SRC (pat)))
      {
      case COMPARE:
	compare_for_stack_reg (insn, regstack, pat);
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
	  src1 = get_true_reg (&XEXP (SET_SRC (pat), 0));

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

	src1 = get_true_reg (&XEXP (SET_SRC (pat), 0));
	src2 = get_true_reg (&XEXP (SET_SRC (pat), 1));

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
	    /* If the register that dies is at the top of stack, then
	       the destination is somewhere else - merely substitute it.
	       But if the reg that dies is not at top of stack, then
	       move the top of stack to the dead reg, as though we had
	       done the insn and then a store-with-pop.  */

	    if (REGNO (XEXP (src1_note, 0)) == regstack->reg[regstack->top])
	      {
		SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		replace_reg (dest, get_hard_regnum (regstack, *dest));
	      }
	    else
	      {
		int regno = get_hard_regnum (regstack, XEXP (src1_note, 0));

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
	    if (REGNO (XEXP (src2_note, 0)) == regstack->reg[regstack->top])
	      {
		SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
		replace_reg (dest, get_hard_regnum (regstack, *dest));
	      }
	    else
	      {
		int regno = get_hard_regnum (regstack, XEXP (src2_note, 0));

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

	break;

      case UNSPEC:
	switch (XINT (SET_SRC (pat), 1))
	  {
	  case 1: /* sin */
	  case 2: /* cos */
	    /* These insns only operate on the top of the stack.  */

	    src1 = get_true_reg (&XVECEXP (SET_SRC (pat), 0, 0));

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

	  default:
	    abort ();
	  }
	break;

      case IF_THEN_ELSE:
	/* dest has to be on stack. */
	if (get_hard_regnum (regstack, *dest) < FIRST_STACK_REG)
	  abort ();

	/* This insn requires the top of stack to be the destination. */

	/* If the comparison operator is an FP comparison operator,
	   it is handled correctly by compare_for_stack_reg () who
	   will move the destination to the top of stack. But if the
	   comparison operator is not an FP comparison operator, we
	   have to handle it here. */
	if (get_hard_regnum (regstack, *dest) >= FIRST_STACK_REG
	    && REGNO (*dest) != regstack->reg[regstack->top])
	  emit_swap_insn (insn, regstack, *dest);	

	src1 = get_true_reg (&XEXP (SET_SRC (pat), 1));
	src2 = get_true_reg (&XEXP (SET_SRC (pat), 2));

	src1_note = find_regno_note (insn, REG_DEAD, REGNO (*src1));
	src2_note = find_regno_note (insn, REG_DEAD, REGNO (*src2));

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
		/* If the register that dies is not at the top of stack, then
		   move the top of stack to the dead reg */
		if (REGNO (XEXP (src_note[i], 0))
		    != regstack->reg[regstack->top])
		  {
		    remove_regno_note (insn, REG_DEAD,
				       REGNO (XEXP (src_note [i], 0)));
		    emit_pop_insn (insn, regstack, XEXP (src_note[i], 0),
				   emit_insn_after);
		  }
		else
		  {
		    CLEAR_HARD_REG_BIT (regstack->reg_set,
					REGNO (XEXP (src_note[i], 0)));
		    replace_reg (&XEXP (src_note[i], 0), FIRST_STACK_REG);
		    regstack->top--;
		  }
	      }
	}

	/* Make dest the top of stack. */
	SET_HARD_REG_BIT (regstack->reg_set, REGNO (*dest));
	replace_reg (dest, FIRST_STACK_REG);

	break;

      default:
	abort ();
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

  rtx *clobber_reg;
  rtx **clobber_loc;

  struct stack_def temp_stack;
  int n_notes;
  int n_clobbers;
  rtx note;
  int i;
  int n_inputs, n_outputs;

  /* Find out what the constraints required.  If no constraint
     alternative matches, that is a compiler bug: we should have caught
     such an insn during the life analysis pass (and reload should have
     caught it regardless).  */
  extract_insn (insn);
  constrain_operands (1);
  alt = which_alternative;

  preprocess_constraints ();

  n_inputs = get_asm_operand_n_inputs (body);
  n_outputs = recog_n_operands - n_inputs;
  
  if (alt < 0)
    abort ();

  /* Strip SUBREGs here to make the following code simpler.  */
  for (i = 0; i < recog_n_operands; i++)
    if (GET_CODE (recog_operand[i]) == SUBREG
	&& GET_CODE (SUBREG_REG (recog_operand[i])) == REG)
      {
	recog_operand_loc[i] = & SUBREG_REG (recog_operand[i]);
	recog_operand[i] = SUBREG_REG (recog_operand[i]);
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

  bcopy ((char *) regstack, (char *) &temp_stack, sizeof (temp_stack));

  /* Put the input regs into the desired place in TEMP_STACK.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_operand[i])
	&& reg_class_subset_p (recog_op_alt[i][alt].class,
			       FLOAT_REGS)
	&& recog_op_alt[i][alt].class != FLOAT_REGS)
      {
	/* If an operand needs to be in a particular reg in
	   FLOAT_REGS, the constraint was either 't' or 'u'.  Since
	   these constraints are for single register classes, and reload
	   guaranteed that operand[i] is already in that class, we can
	   just use REGNO (recog_operand[i]) to know which actual reg this
	   operand needs to be in.  */

	int regno = get_hard_regnum (&temp_stack, recog_operand[i]);

	if (regno < 0)
	  abort ();

	if (regno != REGNO (recog_operand[i]))
	  {
	    /* recog_operand[i] is not in the right place.  Find it
	       and swap it with whatever is already in I's place.
	       K is where recog_operand[i] is now.  J is where it should
	       be.  */
	    int j, k, temp;

	    k = temp_stack.top - (regno - FIRST_STACK_REG);
	    j = (temp_stack.top
		 - (REGNO (recog_operand[i]) - FIRST_STACK_REG));

	    temp = temp_stack.reg[k];
	    temp_stack.reg[k] = temp_stack.reg[j];
	    temp_stack.reg[j] = temp;
	  }
      }

  /* emit insns before INSN to make sure the reg-stack is in the right
     order.  */

  change_stack (insn, regstack, &temp_stack, emit_insn_before);

  /* Make the needed input register substitutions.  Do death notes and
     clobbers too, because these are for inputs, not outputs.  */

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_operand[i]))
      {
	int regnum = get_hard_regnum (regstack, recog_operand[i]);

	if (regnum < 0)
	  abort ();

	replace_reg (recog_operand_loc[i], regnum);
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
    if (STACK_REG_P (recog_operand[i]))
      {
	/* An input reg is implicitly popped if it is tied to an
	   output, or if there is a CLOBBER for it.  */
	int j;

	for (j = 0; j < n_clobbers; j++)
	  if (operands_match_p (clobber_reg[j], recog_operand[i]))
	    break;

	if (j < n_clobbers || recog_op_alt[i][alt].matches >= 0)
	  {
	    /* recog_operand[i] might not be at the top of stack.  But that's
	       OK, because all we need to do is pop the right number of regs
	       off of the top of the reg-stack.  record_asm_stack_regs
	       guaranteed that all implicitly popped regs were grouped
	       at the top of the reg-stack.  */

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
	if (STACK_REG_P (recog_operand[j]) && REGNO (recog_operand[j]) == i)
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
    if (STACK_REG_P (recog_operand[i]))
      {
	int j;

	for (j = 0; j < n_notes; j++)
	  if (REGNO (recog_operand[i]) == REGNO (note_reg[j])
	      && note_kind[j] == REG_UNUSED)
	    {
	      insn = emit_pop_insn (insn, regstack, recog_operand[i],
				    emit_insn_after);
	      break;
	    }
      }

  for (i = n_outputs; i < n_outputs + n_inputs; i++)
    if (STACK_REG_P (recog_operand[i]))
      {
	int j;

	for (j = 0; j < n_notes; j++)
	  if (REGNO (recog_operand[i]) == REGNO (note_reg[j])
	      && note_kind[j] == REG_DEAD
	      && TEST_HARD_REG_BIT (regstack->reg_set,
				    REGNO (recog_operand[i])))
	    {
	      insn = emit_pop_insn (insn, regstack, recog_operand[i],
				    emit_insn_after);
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
  register rtx *note_link, note;
  register int i;

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
	      {
		subst_stack_regs_pat (insn, regstack,
				      XVECEXP (PATTERN (insn), 0, i));

		/* subst_stack_regs_pat may have deleted a no-op insn.  */
		if (GET_CODE (insn) == NOTE)
		  break;
	      }
	  }
      else
	subst_stack_regs_pat (insn, regstack, PATTERN (insn));
    }

  /* subst_stack_regs_pat may have deleted a no-op insn.  If so, any
     REG_UNUSED will already have been dealt with, so just return.  */

  if (GET_CODE (insn) == NOTE)
    return;

  /* If there is a REG_UNUSED note on a stack register on this insn,
     the indicated reg must be popped.  The REG_UNUSED note is removed,
     since the form of the newly emitted pop insn references the reg,
     making it no longer `unset'.  */

  note_link = &REG_NOTES(insn);
  for (note = *note_link; note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_UNUSED && STACK_REG_P (XEXP (note, 0)))
      {
	*note_link = XEXP (note, 1);
	insn = emit_pop_insn (insn, regstack, XEXP (note, 0), emit_insn_after);
      }
    else
      note_link = &XEXP (note, 1);
}

/* Change the organization of the stack so that it fits a new basic
   block.  Some registers might have to be popped, but there can never be
   a register live in the new block that is not now live.

   Insert any needed insns before or after INSN.  WHEN is emit_insn_before
   or emit_insn_after. OLD is the original stack layout, and NEW is
   the desired form.  OLD is updated to reflect the code emitted, ie, it
   will be the same as NEW upon return.

   This function will not preserve block_end[].  But that information
   is no longer needed once this has executed.  */

static void
change_stack (insn, old, new, when)
     rtx insn;
     stack old;
     stack new;
     rtx (*when)();
{
  int reg;

  /* We will be inserting new insns "backwards", by calling emit_insn_before.
     If we are to insert after INSN, find the next insn, and insert before
     it.  */

  if (when == emit_insn_after)
    insn = NEXT_INSN (insn);

  /* Pop any registers that are not needed in the new block.  */

  for (reg = old->top; reg >= 0; reg--)
    if (! TEST_HARD_REG_BIT (new->reg_set, old->reg[reg]))
      emit_pop_insn (insn, old, FP_MODE_REG (old->reg[reg], DFmode),
		     emit_insn_before);

  if (new->top == -2)
    {
      /* If the new block has never been processed, then it can inherit
	 the old stack order.  */

      new->top = old->top;
      bcopy (old->reg, new->reg, sizeof (new->reg));
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
}

/* Check PAT, which points to RTL in INSN, for a LABEL_REF.  If it is
   found, ensure that a jump from INSN to the code_label to which the
   label_ref points ends up with the same stack as that at the
   code_label.  Do this by inserting insns just before the code_label to
   pop and rotate the stack until it is in the correct order.  REGSTACK
   is the order of the register stack in INSN.

   Any code that is emitted here must not be later processed as part
   of any block, as it will already contain hard register numbers.  */

static void
goto_block_pat (insn, regstack, pat)
     rtx insn;
     stack regstack;
     rtx pat;
{
  rtx label;
  rtx new_jump, new_label, new_barrier;
  rtx *ref;
  stack label_stack;
  struct stack_def temp_stack;
  int reg;

  switch (GET_CODE (pat))
   {
     case RETURN:
	straighten_stack (PREV_INSN (insn), regstack);
	return;
     default:
     {
      int i, j;
      char *fmt = GET_RTX_FORMAT (GET_CODE (pat));

      for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    goto_block_pat (insn, regstack, XEXP (pat, i));
	  if (fmt[i] == 'E')
	    for (j = 0; j < XVECLEN (pat, i); j++)
	      goto_block_pat (insn, regstack, XVECEXP (pat, i, j));
	}
      return;
     }
     case LABEL_REF:;
   }

  label = XEXP (pat, 0);
  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  /* First, see if in fact anything needs to be done to the stack at all.  */
  if (INSN_UID (label) <= 0)
    return;

  label_stack = &block_stack_in[BLOCK_NUM (label)];

  if (label_stack->top == -2)
    {
      /* If the target block hasn't had a stack order selected, then
	 we need merely ensure that no pops are needed.  */

      for (reg = regstack->top; reg >= 0; reg--)
	if (! TEST_HARD_REG_BIT (label_stack->reg_set, regstack->reg[reg]))
	  break;

      if (reg == -1)
	{
	  /* change_stack will not emit any code in this case.  */

	  change_stack (label, regstack, label_stack, emit_insn_after);
	  return;
	}
    }
  else if (label_stack->top == regstack->top)
    {
      for (reg = label_stack->top; reg >= 0; reg--)
	if (label_stack->reg[reg] != regstack->reg[reg])
	  break;

      if (reg == -1)
	return;
    }

  /* At least one insn will need to be inserted before label.  Insert
     a jump around the code we are about to emit.  Emit a label for the new
     code, and point the original insn at this new label. We can't use
     redirect_jump here, because we're using fld[4] of the code labels as
     LABEL_REF chains, no NUSES counters.  */

  new_jump = emit_jump_insn_before (gen_jump (label), label);
  record_label_references (new_jump, PATTERN (new_jump));
  JUMP_LABEL (new_jump) = label;

  new_barrier = emit_barrier_after (new_jump);

  new_label = gen_label_rtx ();
  emit_label_after (new_label, new_barrier);
  LABEL_REFS (new_label) = new_label;

  /* The old label_ref will no longer point to the code_label if now uses,
     so strip the label_ref from the code_label's chain of references.  */

  for (ref = &LABEL_REFS (label); *ref != label; ref = &LABEL_NEXTREF (*ref))
    if (*ref == pat)
      break;

  if (*ref == label)
    abort ();

  *ref = LABEL_NEXTREF (*ref);

  XEXP (pat, 0) = new_label;
  record_label_references (insn, PATTERN (insn));

  if (JUMP_LABEL (insn) == label)
    JUMP_LABEL (insn) = new_label;

  /* Now emit the needed code.  */

  temp_stack = *regstack;

  change_stack (new_label, &temp_stack, label_stack, emit_insn_after);
}

/* Traverse all basic blocks in a function, converting the register
   references in each insn from the "flat" register file that gcc uses, to
   the stack-like registers the 387 uses.  */

static void
convert_regs ()
{
  register int block, reg;
  register rtx insn, next;
  struct stack_def regstack;

  for (block = 0; block < blocks; block++)
    {
      if (block_stack_in[block].top == -2)
	{
	  /* This block has not been previously encountered.  Choose a
	     default mapping for any stack regs live on entry */

	  block_stack_in[block].top = -1;

	  for (reg = LAST_STACK_REG; reg >= FIRST_STACK_REG; reg--)
	    if (TEST_HARD_REG_BIT (block_stack_in[block].reg_set, reg))
	      block_stack_in[block].reg[++block_stack_in[block].top] = reg;
	}

      /* Process all insns in this block.  Keep track of `next' here,
	 so that we don't process any insns emitted while making
	 substitutions in INSN.  */

      next = block_begin[block];
      regstack = block_stack_in[block];
      do
	{
	  insn = next;
	  next = NEXT_INSN (insn);

	  /* Don't bother processing unless there is a stack reg
	     mentioned or if it's a CALL_INSN (register passing of
	     floating point values).  */

	  if (stack_regs_mentioned (insn) || GET_CODE (insn) == CALL_INSN)
	    subst_stack_regs (insn, &regstack);

	} while (insn != block_end[block]);
      
      /* For all further actions, INSN needs to be the last insn in
         this basic block.  If subst_stack_regs inserted additional
         instructions after INSN, it is no longer the last one at
         this point.  */
      next = PREV_INSN (next);

      /* If subst_stack_regs inserted something after a JUMP_INSN, that
         is almost certainly a bug.  */
      if (GET_CODE (insn) == JUMP_INSN && insn != next)
	abort ();
      insn = next;

      /* Something failed if the stack life doesn't match.  */

      GO_IF_HARD_REG_EQUAL (regstack.reg_set, block_out_reg_set[block], win);

      abort ();

    win:

      /* Adjust the stack of this block on exit to match the stack of
	 the target block, or copy stack information into stack of
	 jump target if the target block's stack order hasn't been set
	 yet.  */

      if (GET_CODE (insn) == JUMP_INSN)
	goto_block_pat (insn, &regstack, PATTERN (insn));

      /* Likewise handle the case where we fall into the next block.  */

      if ((block < blocks - 1) && block_drops_in[block+1])
	change_stack (insn, &regstack, &block_stack_in[block+1],
		      emit_insn_after);
    }

  /* If the last basic block is the end of a loop, and that loop has
     regs live at its start, then the last basic block will have regs live
     at its end that need to be popped before the function returns.  */

   {
     int value_reg_low, value_reg_high;
     value_reg_low = value_reg_high = -1;
      {
        rtx retvalue;
        if ((retvalue = stack_result (current_function_decl)))
	 {
	   value_reg_low = REGNO (retvalue);
	   value_reg_high = value_reg_low +
	    HARD_REGNO_NREGS (value_reg_low, GET_MODE (retvalue)) - 1;
	 }

      }
     for (reg = regstack.top; reg >= 0; reg--)
        if (regstack.reg[reg] < value_reg_low
	    || regstack.reg[reg] > value_reg_high)
           insn = emit_pop_insn (insn, &regstack,
			    FP_MODE_REG (regstack.reg[reg], DFmode),
			    emit_insn_after);
   }
  straighten_stack (insn, &regstack);
}

/* Check expression PAT, which is in INSN, for label references.  if
   one is found, print the block number of destination to FILE.  */

static void
print_blocks (file, insn, pat)
     FILE *file;
     rtx insn, pat;
{
  register RTX_CODE code = GET_CODE (pat);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (pat, 0);

      if (GET_CODE (label) != CODE_LABEL)
	abort ();

      fprintf (file, " %d", BLOCK_NUM (label));

      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	print_blocks (file, insn, XEXP (pat, i));
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (pat, i); j++)
	    print_blocks (file, insn, XVECEXP (pat, i, j));
	}
    }
}

/* Write information about stack registers and stack blocks into FILE.
   This is part of making a debugging dump.  */

static void
dump_stack_info (file)
     FILE *file;
{
  register int block;

  fprintf (file, "\n%d stack blocks.\n", blocks);
  for (block = 0; block < blocks; block++)
    {
      register rtx head, jump, end;
      register int regno;

      fprintf (file, "\nStack block %d: first insn %d, last %d.\n",
	       block, INSN_UID (block_begin[block]),
	       INSN_UID (block_end[block]));

      head = block_begin[block];

      fprintf (file, "Reached from blocks: ");
      if (GET_CODE (head) == CODE_LABEL)
	for (jump = LABEL_REFS (head);
	     jump != head;
	     jump = LABEL_NEXTREF (jump))
	  {
	    register int from_block = BLOCK_NUM (CONTAINING_INSN (jump));
	    fprintf (file, " %d", from_block);
	  }
      if (block_drops_in[block])
	fprintf (file, " previous");

      fprintf (file, "\nlive stack registers on block entry: ");
      for (regno = FIRST_STACK_REG; regno <= LAST_STACK_REG; regno++)
	{
	  if (TEST_HARD_REG_BIT (block_stack_in[block].reg_set, regno))
	    fprintf (file, "%d ", regno);
	}

      fprintf (file, "\nlive stack registers on block exit: ");
      for (regno = FIRST_STACK_REG; regno <= LAST_STACK_REG; regno++)
	{
	  if (TEST_HARD_REG_BIT (block_out_reg_set[block], regno))
	    fprintf (file, "%d ", regno);
	}

      end = block_end[block];

      fprintf (file, "\nJumps to blocks: ");
      if (GET_CODE (end) == JUMP_INSN)
	print_blocks (file, end, PATTERN (end));

      if (block + 1 < blocks && block_drops_in[block+1])
	fprintf (file, " next");
      else if (block + 1 == blocks
	       || (GET_CODE (end) == JUMP_INSN
		   && GET_CODE (PATTERN (end)) == RETURN))
	fprintf (file, " return");

      fprintf (file, "\n");
    }
}
#endif /* STACK_REGS */
