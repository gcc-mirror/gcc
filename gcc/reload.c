/* Search an insn for pseudo regs that must be in hard regs and are not.
   Copyright (C) 1987, 88, 89, 92, 93, 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file contains subroutines used only from the file reload1.c.
   It knows how to scan one insn for operands and values
   that need to be copied into registers to make valid code.
   It also finds other operands and values which are valid
   but for which equivalent values in registers exist and
   ought to be used instead.

   Before processing the first insn of the function, call `init_reload'.

   To scan an insn, call `find_reloads'.  This does two things:
   1. sets up tables describing which values must be reloaded
   for this insn, and what kind of hard regs they must be reloaded into;
   2. optionally record the locations where those values appear in
   the data, so they can be replaced properly later.
   This is done only if the second arg to `find_reloads' is nonzero.

   The third arg to `find_reloads' specifies the number of levels
   of indirect addressing supported by the machine.  If it is zero,
   indirect addressing is not valid.  If it is one, (MEM (REG n))
   is valid even if (REG n) did not get a hard register; if it is two,
   (MEM (MEM (REG n))) is also valid even if (REG n) did not get a
   hard register, and similarly for higher values.

   Then you must choose the hard regs to reload those pseudo regs into,
   and generate appropriate load insns before this insn and perhaps
   also store insns after this insn.  Set up the array `reload_reg_rtx'
   to contain the REG rtx's for the registers you used.  In some
   cases `find_reloads' will return a nonzero value in `reload_reg_rtx'
   for certain reloads.  Then that tells you which register to use,
   so you do not need to allocate one.  But you still do need to add extra
   instructions to copy the value into and out of that register.

   Finally you must call `subst_reloads' to substitute the reload reg rtx's
   into the locations already recorded.

NOTE SIDE EFFECTS:

   find_reloads can alter the operands of the instruction it is called on.

   1. Two operands of any sort may be interchanged, if they are in a
   commutative instruction.
   This happens only if find_reloads thinks the instruction will compile
   better that way.

   2. Pseudo-registers that are equivalent to constants are replaced
   with those constants if they are not in hard registers.

1 happens every time find_reloads is called.
2 happens only when REPLACE is 1, which is only when
actually doing the reloads, not when just counting them.


Using a reload register for several reloads in one insn:

When an insn has reloads, it is considered as having three parts:
the input reloads, the insn itself after reloading, and the output reloads.
Reloads of values used in memory addresses are often needed for only one part.

When this is so, reload_when_needed records which part needs the reload.
Two reloads for different parts of the insn can share the same reload
register.

When a reload is used for addresses in multiple parts, or when it is
an ordinary operand, it is classified as RELOAD_OTHER, and cannot share
a register with any other reload.  */

#define REG_OK_STRICT

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "recog.h"
#include "reload.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"

#ifndef REGISTER_MOVE_COST
#define REGISTER_MOVE_COST(x, y) 2
#endif

/* The variables set up by `find_reloads' are:

   n_reloads		  number of distinct reloads needed; max reload # + 1
       tables indexed by reload number
   reload_in		  rtx for value to reload from
   reload_out		  rtx for where to store reload-reg afterward if nec
			   (often the same as reload_in)
   reload_reg_class	  enum reg_class, saying what regs to reload into
   reload_inmode	  enum machine_mode; mode this operand should have
			   when reloaded, on input.
   reload_outmode	  enum machine_mode; mode this operand should have
			   when reloaded, on output.
   reload_optional	  char, nonzero for an optional reload.
			   Optional reloads are ignored unless the
			   value is already sitting in a register.
   reload_inc		  int, positive amount to increment or decrement by if
			   reload_in is a PRE_DEC, PRE_INC, POST_DEC, POST_INC.
			   Ignored otherwise (don't assume it is zero).
   reload_in_reg	  rtx.  A reg for which reload_in is the equivalent.
			   If reload_in is a symbol_ref which came from
			   reg_equiv_constant, then this is the pseudo
			   which has that symbol_ref as equivalent.
   reload_reg_rtx	  rtx.  This is the register to reload into.
			   If it is zero when `find_reloads' returns,
			   you must find a suitable register in the class
			   specified by reload_reg_class, and store here
			   an rtx for that register with mode from
			   reload_inmode or reload_outmode.
   reload_nocombine	  char, nonzero if this reload shouldn't be
			   combined with another reload.
   reload_opnum		  int, operand number being reloaded.  This is
			   used to group related reloads and need not always
			   be equal to the actual operand number in the insn,
			   though it current will be; for in-out operands, it
			   is one of the two operand numbers.
   reload_when_needed    enum, classifies reload as needed either for
			   addressing an input reload, addressing an output,
			   for addressing a non-reloaded mem ref,
			   or for unspecified purposes (i.e., more than one
			   of the above).
   reload_secondary_p	  int, 1 if this is a secondary register for one
			   or more reloads.
   reload_secondary_in_reload
   reload_secondary_out_reload
   			  int, gives the reload number of a secondary
			   reload, when needed; otherwise -1
   reload_secondary_in_icode
   reload_secondary_out_icode
			  enum insn_code, if a secondary reload is required,
			   gives the INSN_CODE that uses the secondary
			   reload as a scratch register, or CODE_FOR_nothing
			   if the secondary reload register is to be an
			   intermediate register.  */
int n_reloads;

rtx reload_in[MAX_RELOADS];
rtx reload_out[MAX_RELOADS];
enum reg_class reload_reg_class[MAX_RELOADS];
enum machine_mode reload_inmode[MAX_RELOADS];
enum machine_mode reload_outmode[MAX_RELOADS];
rtx reload_reg_rtx[MAX_RELOADS];
char reload_optional[MAX_RELOADS];
int reload_inc[MAX_RELOADS];
rtx reload_in_reg[MAX_RELOADS];
char reload_nocombine[MAX_RELOADS];
int reload_opnum[MAX_RELOADS];
enum reload_type reload_when_needed[MAX_RELOADS];
int reload_secondary_p[MAX_RELOADS];
int reload_secondary_in_reload[MAX_RELOADS];
int reload_secondary_out_reload[MAX_RELOADS];
enum insn_code reload_secondary_in_icode[MAX_RELOADS];
enum insn_code reload_secondary_out_icode[MAX_RELOADS];

/* All the "earlyclobber" operands of the current insn
   are recorded here.  */
int n_earlyclobbers;
rtx reload_earlyclobbers[MAX_RECOG_OPERANDS];

int reload_n_operands;

/* Replacing reloads.

   If `replace_reloads' is nonzero, then as each reload is recorded
   an entry is made for it in the table `replacements'.
   Then later `subst_reloads' can look through that table and
   perform all the replacements needed.  */

/* Nonzero means record the places to replace.  */
static int replace_reloads;

/* Each replacement is recorded with a structure like this.  */
struct replacement
{
  rtx *where;			/* Location to store in */
  rtx *subreg_loc;		/* Location of SUBREG if WHERE is inside
				   a SUBREG; 0 otherwise.  */
  int what;			/* which reload this is for */
  enum machine_mode mode;	/* mode it must have */
};

static struct replacement replacements[MAX_RECOG_OPERANDS * ((MAX_REGS_PER_ADDRESS * 2) + 1)];

/* Number of replacements currently recorded.  */
static int n_replacements;

/* Used to track what is modified by an operand.  */
struct decomposition
{
  int reg_flag;		/* Nonzero if referencing a register. */
  int safe;		/* Nonzero if this can't conflict with anything. */
  rtx base;		/* Base adddress for MEM. */
  HOST_WIDE_INT start;	/* Starting offset or register number. */
  HOST_WIDE_INT end;	/* Endinf offset or register number.  */
};

/* MEM-rtx's created for pseudo-regs in stack slots not directly addressable;
   (see reg_equiv_address).  */
static rtx memlocs[MAX_RECOG_OPERANDS * ((MAX_REGS_PER_ADDRESS * 2) + 1)];
static int n_memlocs;

#ifdef SECONDARY_MEMORY_NEEDED

/* Save MEMs needed to copy from one class of registers to another.  One MEM
   is used per mode, but normally only one or two modes are ever used.  

   We keep two versions, before and after register elimination.  The one 
   after register elimination is record separately for each operand.  This
   is done in case the address is not valid to be sure that we separately
   reload each.  */

static rtx secondary_memlocs[NUM_MACHINE_MODES];
static rtx secondary_memlocs_elim[NUM_MACHINE_MODES][MAX_RECOG_OPERANDS];
#endif

/* The instruction we are doing reloads for;
   so we can test whether a register dies in it.  */
static rtx this_insn;

/* Nonzero if this instruction is a user-specified asm with operands.  */
static int this_insn_is_asm;

/* If hard_regs_live_known is nonzero,
   we can tell which hard regs are currently live,
   at least enough to succeed in choosing dummy reloads.  */
static int hard_regs_live_known;

/* Indexed by hard reg number,
   element is nonegative if hard reg has been spilled.
   This vector is passed to `find_reloads' as an argument
   and is not changed here.  */
static short *static_reload_reg_p;

/* Set to 1 in subst_reg_equivs if it changes anything.  */
static int subst_reg_equivs_changed;

/* On return from push_reload, holds the reload-number for the OUT
   operand, which can be different for that from the input operand.  */
static int output_reloadnum;

  /* Compare two RTX's.  */
#define MATCHES(x, y) \
 (x == y || (x != 0 && (GET_CODE (x) == REG				\
			? GET_CODE (y) == REG && REGNO (x) == REGNO (y)	\
			: rtx_equal_p (x, y) && ! side_effects_p (x))))

  /* Indicates if two reloads purposes are for similar enough things that we
     can merge their reloads.  */
#define MERGABLE_RELOADS(when1, when2, op1, op2) \
  ((when1) == RELOAD_OTHER || (when2) == RELOAD_OTHER	\
   || ((when1) == (when2) && (op1) == (op2))		\
   || ((when1) == RELOAD_FOR_INPUT && (when2) == RELOAD_FOR_INPUT) \
   || ((when1) == RELOAD_FOR_OPERAND_ADDRESS		\
       && (when2) == RELOAD_FOR_OPERAND_ADDRESS)	\
   || ((when1) == RELOAD_FOR_OTHER_ADDRESS		\
       && (when2) == RELOAD_FOR_OTHER_ADDRESS))

  /* Nonzero if these two reload purposes produce RELOAD_OTHER when merged.  */
#define MERGE_TO_OTHER(when1, when2, op1, op2) \
  ((when1) != (when2)					\
   || ! ((op1) == (op2)					\
	 || (when1) == RELOAD_FOR_INPUT			\
	 || (when1) == RELOAD_FOR_OPERAND_ADDRESS	\
	 || (when1) == RELOAD_FOR_OTHER_ADDRESS))

static int push_secondary_reload PROTO((int, rtx, int, int, enum reg_class,
					enum machine_mode, enum reload_type,
					enum insn_code *));
static int push_reload		PROTO((rtx, rtx, rtx *, rtx *, enum reg_class,
				       enum machine_mode, enum machine_mode,
				       int, int, int, enum reload_type));
static void push_replacement	PROTO((rtx *, int, enum machine_mode));
static void combine_reloads	PROTO((void));
static rtx find_dummy_reload	PROTO((rtx, rtx, rtx *, rtx *,
				       enum machine_mode, enum machine_mode,
				       enum reg_class, int));
static int earlyclobber_operand_p PROTO((rtx));
static int hard_reg_set_here_p	PROTO((int, int, rtx));
static struct decomposition decompose PROTO((rtx));
static int immune_p		PROTO((rtx, rtx, struct decomposition));
static int alternative_allows_memconst PROTO((char *, int));
static rtx find_reloads_toplev	PROTO((rtx, int, enum reload_type, int, int));
static rtx make_memloc		PROTO((rtx, int));
static int find_reloads_address	PROTO((enum machine_mode, rtx *, rtx, rtx *,
				       int, enum reload_type, int));
static rtx subst_reg_equivs	PROTO((rtx));
static rtx subst_indexed_address PROTO((rtx));
static int find_reloads_address_1 PROTO((rtx, int, rtx *, int,
					 enum reload_type,int));
static void find_reloads_address_part PROTO((rtx, rtx *, enum reg_class,
					     enum machine_mode, int,
					     enum reload_type, int));
static int find_inc_amount	PROTO((rtx, rtx));

#ifdef HAVE_SECONDARY_RELOADS

/* Determine if any secondary reloads are needed for loading (if IN_P is
   non-zero) or storing (if IN_P is zero) X to or from a reload register of
   register class RELOAD_CLASS in mode RELOAD_MODE.  If secondary reloads
   are needed, push them.

   Return the reload number of the secondary reload we made, or -1 if
   we didn't need one.  *PICODE is set to the insn_code to use if we do
   need a secondary reload.  */

static int
push_secondary_reload (in_p, x, opnum, optional, reload_class, reload_mode,
		       type, picode)
     int in_p;
     rtx x;
     int opnum;
     int optional;
     enum reg_class reload_class;
     enum machine_mode reload_mode;
     enum reload_type type;
     enum insn_code *picode;
{
  enum reg_class class = NO_REGS;
  enum machine_mode mode = reload_mode;
  enum insn_code icode = CODE_FOR_nothing;
  enum reg_class t_class = NO_REGS;
  enum machine_mode t_mode = VOIDmode;
  enum insn_code t_icode = CODE_FOR_nothing;
  enum reload_type secondary_type;
  int i;
  int s_reload, t_reload = -1;

  if (type == RELOAD_FOR_INPUT_ADDRESS || type == RELOAD_FOR_OUTPUT_ADDRESS)
    secondary_type = type;
  else
    secondary_type = in_p ? RELOAD_FOR_INPUT_ADDRESS : RELOAD_FOR_OUTPUT_ADDRESS;

  *picode = CODE_FOR_nothing;

  /* If X is a pseudo-register that has an equivalent MEM (actually, if it
     is still a pseudo-register by now, it *must* have an equivalent MEM
     but we don't want to assume that), use that equivalent when seeing if
     a secondary reload is needed since whether or not a reload is needed
     might be sensitive to the form of the MEM.  */

  if (GET_CODE (x) == REG && REGNO (x) >= FIRST_PSEUDO_REGISTER
      && reg_equiv_mem[REGNO (x)] != 0)
    x = reg_equiv_mem[REGNO (x)];

#ifdef SECONDARY_INPUT_RELOAD_CLASS
  if (in_p)
    class = SECONDARY_INPUT_RELOAD_CLASS (reload_class, reload_mode, x);
#endif

#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
  if (! in_p)
    class = SECONDARY_OUTPUT_RELOAD_CLASS (reload_class, reload_mode, x);
#endif

  /* If we don't need any secondary registers, done.  */
  if (class == NO_REGS)
    return -1;

  /* Get a possible insn to use.  If the predicate doesn't accept X, don't
     use the insn.  */

  icode = (in_p ? reload_in_optab[(int) reload_mode]
	   : reload_out_optab[(int) reload_mode]);

  if (icode != CODE_FOR_nothing
      && insn_operand_predicate[(int) icode][in_p]
      && (! (insn_operand_predicate[(int) icode][in_p]) (x, reload_mode)))
    icode = CODE_FOR_nothing;

  /* If we will be using an insn, see if it can directly handle the reload
     register we will be using.  If it can, the secondary reload is for a
     scratch register.  If it can't, we will use the secondary reload for
     an intermediate register and require a tertiary reload for the scratch
     register.  */

  if (icode != CODE_FOR_nothing)
    {
      /* If IN_P is non-zero, the reload register will be the output in 
	 operand 0.  If IN_P is zero, the reload register will be the input
	 in operand 1.  Outputs should have an initial "=", which we must
	 skip.  */

      char insn_letter = insn_operand_constraint[(int) icode][!in_p][in_p];
      enum reg_class insn_class
	= (insn_letter == 'r' ? GENERAL_REGS
	   : REG_CLASS_FROM_LETTER (insn_letter));

      if (insn_class == NO_REGS
	  || (in_p && insn_operand_constraint[(int) icode][!in_p][0] != '=')
	  /* The scratch register's constraint must start with "=&".  */
	  || insn_operand_constraint[(int) icode][2][0] != '='
	  || insn_operand_constraint[(int) icode][2][1] != '&')
	abort ();

      if (reg_class_subset_p (reload_class, insn_class))
	mode = insn_operand_mode[(int) icode][2];
      else
	{
	  char t_letter = insn_operand_constraint[(int) icode][2][2];
	  class = insn_class;
	  t_mode = insn_operand_mode[(int) icode][2];
	  t_class = (t_letter == 'r' ? GENERAL_REGS
		     : REG_CLASS_FROM_LETTER (t_letter));
	  t_icode = icode;
	  icode = CODE_FOR_nothing;
	}
    }

  /* This case isn't valid, so fail.  Reload is allowed to use the same
     register for RELOAD_FOR_INPUT_ADDRESS and RELOAD_FOR_INPUT reloads, but
     in the case of a secondary register, we actually need two different
     registers for correct code.  We fail here to prevent the possibility of
     silently generating incorrect code later.

     The convention is that secondary input reloads are valid only if the
     secondary_class is different from class.  If you have such a case, you
     can not use secondary reloads, you must work around the problem some
     other way.

     Allow this when MODE is not reload_mode and assume that the generated
     code handles this case (it does on the Alpha, which is the only place
     this currently happens).  */

  if (in_p && class == reload_class && mode == reload_mode)
    abort ();

  /* If we need a tertiary reload, see if we have one we can reuse or else
     make a new one.  */

  if (t_class != NO_REGS)
    {
      for (t_reload = 0; t_reload < n_reloads; t_reload++)
	if (reload_secondary_p[t_reload]
	    && (reg_class_subset_p (t_class, reload_reg_class[t_reload])
		|| reg_class_subset_p (reload_reg_class[t_reload], t_class))
	    && ((in_p && reload_inmode[t_reload] == t_mode)
		|| (! in_p && reload_outmode[t_reload] == t_mode))
	    && ((in_p && (reload_secondary_in_icode[t_reload]
			  == CODE_FOR_nothing))
		|| (! in_p &&(reload_secondary_out_icode[t_reload]
			      == CODE_FOR_nothing)))
	    && (reg_class_size[(int) t_class] == 1
#ifdef SMALL_REGISTER_CLASSES
		|| 1
#endif
		)
	    && MERGABLE_RELOADS (secondary_type,
				 reload_when_needed[t_reload],
				 opnum, reload_opnum[t_reload]))
	  {
	    if (in_p)
	      reload_inmode[t_reload] = t_mode;
	    if (! in_p)
	      reload_outmode[t_reload] = t_mode;

	    if (reg_class_subset_p (t_class, reload_reg_class[t_reload]))
	      reload_reg_class[t_reload] = t_class;

	    reload_opnum[t_reload] = MIN (reload_opnum[t_reload], opnum);
	    reload_optional[t_reload] &= optional;
	    reload_secondary_p[t_reload] = 1;
	    if (MERGE_TO_OTHER (secondary_type, reload_when_needed[t_reload],
				opnum, reload_opnum[t_reload]))
	      reload_when_needed[t_reload] = RELOAD_OTHER;
	  }

      if (t_reload == n_reloads)
	{
	  /* We need to make a new tertiary reload for this register class.  */
	  reload_in[t_reload] = reload_out[t_reload] = 0;
	  reload_reg_class[t_reload] = t_class;
	  reload_inmode[t_reload] = in_p ? t_mode : VOIDmode;
	  reload_outmode[t_reload] = ! in_p ? t_mode : VOIDmode;
	  reload_reg_rtx[t_reload] = 0;
	  reload_optional[t_reload] = optional;
	  reload_inc[t_reload] = 0;
	  /* Maybe we could combine these, but it seems too tricky.  */
	  reload_nocombine[t_reload] = 1;
	  reload_in_reg[t_reload] = 0;
	  reload_opnum[t_reload] = opnum;
	  reload_when_needed[t_reload] = secondary_type;
	  reload_secondary_in_reload[t_reload] = -1;
	  reload_secondary_out_reload[t_reload] = -1;
	  reload_secondary_in_icode[t_reload] = CODE_FOR_nothing;
	  reload_secondary_out_icode[t_reload] = CODE_FOR_nothing;
	  reload_secondary_p[t_reload] = 1;

	  n_reloads++;
	}
    }

  /* See if we can reuse an existing secondary reload.  */
  for (s_reload = 0; s_reload < n_reloads; s_reload++)
    if (reload_secondary_p[s_reload]
	&& (reg_class_subset_p (class, reload_reg_class[s_reload])
	    || reg_class_subset_p (reload_reg_class[s_reload], class))
	&& ((in_p && reload_inmode[s_reload] == mode)
	    || (! in_p && reload_outmode[s_reload] == mode))
	&& ((in_p && reload_secondary_in_reload[s_reload] == t_reload)
	    || (! in_p && reload_secondary_out_reload[s_reload] == t_reload))
	&& ((in_p && reload_secondary_in_icode[s_reload] == t_icode)
	    || (! in_p && reload_secondary_out_icode[s_reload] == t_icode))
	&& (reg_class_size[(int) class] == 1
#ifdef SMALL_REGISTER_CLASSES
	    || 1
#endif
	    )
	&& MERGABLE_RELOADS (secondary_type, reload_when_needed[s_reload],
			     opnum, reload_opnum[s_reload]))
      {
	if (in_p)
	  reload_inmode[s_reload] = mode;
	if (! in_p)
	  reload_outmode[s_reload] = mode;

	if (reg_class_subset_p (class, reload_reg_class[s_reload]))
	  reload_reg_class[s_reload] = class;

	reload_opnum[s_reload] = MIN (reload_opnum[s_reload], opnum);
	reload_optional[s_reload] &= optional;
	reload_secondary_p[s_reload] = 1;
	if (MERGE_TO_OTHER (secondary_type, reload_when_needed[s_reload],
			    opnum, reload_opnum[s_reload]))
	  reload_when_needed[s_reload] = RELOAD_OTHER;
      }

  if (s_reload == n_reloads)
    {
      /* We need to make a new secondary reload for this register class.  */
      reload_in[s_reload] = reload_out[s_reload] = 0;
      reload_reg_class[s_reload] = class;

      reload_inmode[s_reload] = in_p ? mode : VOIDmode;
      reload_outmode[s_reload] = ! in_p ? mode : VOIDmode;
      reload_reg_rtx[s_reload] = 0;
      reload_optional[s_reload] = optional;
      reload_inc[s_reload] = 0;
      /* Maybe we could combine these, but it seems too tricky.  */
      reload_nocombine[s_reload] = 1;
      reload_in_reg[s_reload] = 0;
      reload_opnum[s_reload] = opnum;
      reload_when_needed[s_reload] = secondary_type;
      reload_secondary_in_reload[s_reload] = in_p ? t_reload : -1;
      reload_secondary_out_reload[s_reload] = ! in_p ? t_reload : -1;
      reload_secondary_in_icode[s_reload] = in_p ? t_icode : CODE_FOR_nothing; 
      reload_secondary_out_icode[s_reload]
	= ! in_p ? t_icode : CODE_FOR_nothing;
      reload_secondary_p[s_reload] = 1;

      n_reloads++;

#ifdef SECONDARY_MEMORY_NEEDED
      /* If we need a memory location to copy between the two reload regs,
	 set it up now.  */

      if (in_p && icode == CODE_FOR_nothing
	  && SECONDARY_MEMORY_NEEDED (class, reload_class, reload_mode))
	get_secondary_mem (x, reload_mode, opnum, type);

      if (! in_p && icode == CODE_FOR_nothing
	  && SECONDARY_MEMORY_NEEDED (reload_class, class, reload_mode))
	get_secondary_mem (x, reload_mode, opnum, type);
#endif
    }

  *picode = icode;
  return s_reload;
}
#endif /* HAVE_SECONDARY_RELOADS */

#ifdef SECONDARY_MEMORY_NEEDED

/* Return a memory location that will be used to copy X in mode MODE.  
   If we haven't already made a location for this mode in this insn,
   call find_reloads_address on the location being returned.  */

rtx
get_secondary_mem (x, mode, opnum, type)
     rtx x;
     enum machine_mode mode;
     int opnum;
     enum reload_type type;
{
  rtx loc;
  int mem_valid;

  /* By default, if MODE is narrower than a word, widen it to a word.
     This is required because most machines that require these memory
     locations do not support short load and stores from all registers
     (e.g., FP registers).  */

#ifdef SECONDARY_MEMORY_NEEDED_MODE
  mode = SECONDARY_MEMORY_NEEDED_MODE (mode);
#else
  if (GET_MODE_BITSIZE (mode) < BITS_PER_WORD)
    mode = mode_for_size (BITS_PER_WORD, GET_MODE_CLASS (mode), 0);
#endif

  /* If we already have made a MEM for this operand in MODE, return it.  */
  if (secondary_memlocs_elim[(int) mode][opnum] != 0)
    return secondary_memlocs_elim[(int) mode][opnum];

  /* If this is the first time we've tried to get a MEM for this mode, 
     allocate a new one.  `something_changed' in reload will get set
     by noticing that the frame size has changed.  */

  if (secondary_memlocs[(int) mode] == 0)
    {
#ifdef SECONDARY_MEMORY_NEEDED_RTX
      secondary_memlocs[(int) mode] = SECONDARY_MEMORY_NEEDED_RTX (mode);
#else
      secondary_memlocs[(int) mode]
	= assign_stack_local (mode, GET_MODE_SIZE (mode), 0);
#endif
    }

  /* Get a version of the address doing any eliminations needed.  If that
     didn't give us a new MEM, make a new one if it isn't valid.  */

  loc = eliminate_regs (secondary_memlocs[(int) mode], VOIDmode, NULL_RTX);
  mem_valid = strict_memory_address_p (mode, XEXP (loc, 0));

  if (! mem_valid && loc == secondary_memlocs[(int) mode])
    loc = copy_rtx (loc);

  /* The only time the call below will do anything is if the stack
     offset is too large.  In that case IND_LEVELS doesn't matter, so we
     can just pass a zero.  Adjust the type to be the address of the
     corresponding object.  If the address was valid, save the eliminated
     address.  If it wasn't valid, we need to make a reload each time, so
     don't save it.  */

  if (! mem_valid)
    {
      type =  (type == RELOAD_FOR_INPUT ? RELOAD_FOR_INPUT_ADDRESS
	       : type == RELOAD_FOR_OUTPUT ? RELOAD_FOR_OUTPUT_ADDRESS
	       : RELOAD_OTHER);

      find_reloads_address (mode, NULL_PTR, XEXP (loc, 0), &XEXP (loc, 0),
			    opnum, type, 0);
    }

  secondary_memlocs_elim[(int) mode][opnum] = loc;
  return loc;
}

/* Clear any secondary memory locations we've made.  */

void
clear_secondary_mem ()
{
  bzero ((char *) secondary_memlocs, sizeof secondary_memlocs);
}
#endif /* SECONDARY_MEMORY_NEEDED */

/* Record one reload that needs to be performed.
   IN is an rtx saying where the data are to be found before this instruction.
   OUT says where they must be stored after the instruction.
   (IN is zero for data not read, and OUT is zero for data not written.)
   INLOC and OUTLOC point to the places in the instructions where
   IN and OUT were found.
   If IN and OUT are both non-zero, it means the same register must be used
   to reload both IN and OUT.

   CLASS is a register class required for the reloaded data.
   INMODE is the machine mode that the instruction requires
   for the reg that replaces IN and OUTMODE is likewise for OUT.

   If IN is zero, then OUT's location and mode should be passed as
   INLOC and INMODE.

   STRICT_LOW is the 1 if there is a containing STRICT_LOW_PART rtx.

   OPTIONAL nonzero means this reload does not need to be performed:
   it can be discarded if that is more convenient.

   OPNUM and TYPE say what the purpose of this reload is.

   The return value is the reload-number for this reload.

   If both IN and OUT are nonzero, in some rare cases we might
   want to make two separate reloads.  (Actually we never do this now.)
   Therefore, the reload-number for OUT is stored in
   output_reloadnum when we return; the return value applies to IN.
   Usually (presently always), when IN and OUT are nonzero,
   the two reload-numbers are equal, but the caller should be careful to
   distinguish them.  */

static int
push_reload (in, out, inloc, outloc, class,
	     inmode, outmode, strict_low, optional, opnum, type)
     register rtx in, out;
     rtx *inloc, *outloc;
     enum reg_class class;
     enum machine_mode inmode, outmode;
     int strict_low;
     int optional;
     int opnum;
     enum reload_type type;
{
  register int i;
  int dont_share = 0;
  rtx *in_subreg_loc = 0, *out_subreg_loc = 0;
  int secondary_in_reload = -1, secondary_out_reload = -1;
  enum insn_code secondary_in_icode, secondary_out_icode;

  /* INMODE and/or OUTMODE could be VOIDmode if no mode
     has been specified for the operand.  In that case,
     use the operand's mode as the mode to reload.  */
  if (inmode == VOIDmode && in != 0)
    inmode = GET_MODE (in);
  if (outmode == VOIDmode && out != 0)
    outmode = GET_MODE (out);

  /* If IN is a pseudo register everywhere-equivalent to a constant, and 
     it is not in a hard register, reload straight from the constant,
     since we want to get rid of such pseudo registers.
     Often this is done earlier, but not always in find_reloads_address.  */
  if (in != 0 && GET_CODE (in) == REG)
    {
      register int regno = REGNO (in);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	in = reg_equiv_constant[regno];
    }

  /* Likewise for OUT.  Of course, OUT will never be equivalent to
     an actual constant, but it might be equivalent to a memory location
     (in the case of a parameter).  */
  if (out != 0 && GET_CODE (out) == REG)
    {
      register int regno = REGNO (out);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	out = reg_equiv_constant[regno];
    }

  /* If we have a read-write operand with an address side-effect,
     change either IN or OUT so the side-effect happens only once.  */
  if (in != 0 && out != 0 && GET_CODE (in) == MEM && rtx_equal_p (in, out))
    {
      if (GET_CODE (XEXP (in, 0)) == POST_INC
	  || GET_CODE (XEXP (in, 0)) == POST_DEC)
	in = gen_rtx (MEM, GET_MODE (in), XEXP (XEXP (in, 0), 0));
      if (GET_CODE (XEXP (in, 0)) == PRE_INC
	  || GET_CODE (XEXP (in, 0)) == PRE_DEC)
	out = gen_rtx (MEM, GET_MODE (out), XEXP (XEXP (out, 0), 0));
    }

  /* If we are reloading a (SUBREG constant ...), really reload just the
     inside expression in its own mode.  Similarly for (SUBREG (PLUS ...)).
     If we have (SUBREG:M1 (MEM:M2 ...) ...) (or an inner REG that is still
     a pseudo and hence will become a MEM) with M1 wider than M2 and the
     register is a pseudo, also reload the inside expression.
     For machines that extend byte loads, do this for any SUBREG of a pseudo
     where both M1 and M2 are a word or smaller, M1 is wider than M2, and
     M2 is an integral mode that gets extended when loaded.
     Similar issue for (SUBREG:M1 (REG:M2 ...) ...) for a hard register R where
     either M1 is not valid for R or M2 is wider than a word but we only
     need one word to store an M2-sized quantity in R.
     (However, if OUT is nonzero, we need to reload the reg *and*
     the subreg, so do nothing here, and let following statement handle it.)

     Note that the case of (SUBREG (CONST_INT...)...) is handled elsewhere;
     we can't handle it here because CONST_INT does not indicate a mode.

     Similarly, we must reload the inside expression if we have a
     STRICT_LOW_PART (presumably, in == out in the cas).

     Also reload the inner expression if it does not require a secondary
     reload but the SUBREG does.

     Finally, reload the inner expression if it is a register that is in
     the class whose registers cannot be referenced in a different size
     and M1 is not the same size as M2.  */

  if (in != 0 && GET_CODE (in) == SUBREG
      && (CONSTANT_P (SUBREG_REG (in))
	  || GET_CODE (SUBREG_REG (in)) == PLUS
	  || strict_low
	  || (((GET_CODE (SUBREG_REG (in)) == REG
		&& REGNO (SUBREG_REG (in)) >= FIRST_PSEUDO_REGISTER)
	       || GET_CODE (SUBREG_REG (in)) == MEM)
	      && ((GET_MODE_SIZE (inmode)
		   > GET_MODE_SIZE (GET_MODE (SUBREG_REG (in))))
#ifdef LOAD_EXTEND_OP
		  || (GET_MODE_SIZE (inmode) <= UNITS_PER_WORD
		      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
			  <= UNITS_PER_WORD)
		      && (GET_MODE_SIZE (inmode)
			  > GET_MODE_SIZE (GET_MODE (SUBREG_REG (in))))
		      && INTEGRAL_MODE_P (GET_MODE (SUBREG_REG (in)))
		      && LOAD_EXTEND_OP (GET_MODE (SUBREG_REG (in))) != NIL)
#endif
		  ))
	  || (GET_CODE (SUBREG_REG (in)) == REG
	      && REGNO (SUBREG_REG (in)) < FIRST_PSEUDO_REGISTER
	      /* The case where out is nonzero
		 is handled differently in the following statement.  */
	      && (out == 0 || SUBREG_WORD (in) == 0)
	      && ((GET_MODE_SIZE (inmode) <= UNITS_PER_WORD
		   && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
		       > UNITS_PER_WORD)
		   && ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
			/ UNITS_PER_WORD)
		       != HARD_REGNO_NREGS (REGNO (SUBREG_REG (in)),
					    GET_MODE (SUBREG_REG (in)))))
		  || ! HARD_REGNO_MODE_OK ((REGNO (SUBREG_REG (in))
					    + SUBREG_WORD (in)),
					   inmode)))
#ifdef SECONDARY_INPUT_RELOAD_CLASS
	  || (SECONDARY_INPUT_RELOAD_CLASS (class, inmode, in) != NO_REGS
	      && (SECONDARY_INPUT_RELOAD_CLASS (class,
						GET_MODE (SUBREG_REG (in)),
						SUBREG_REG (in))
		  == NO_REGS))
#endif
#ifdef CLASS_CANNOT_CHANGE_SIZE
	  || (GET_CODE (SUBREG_REG (in)) == REG
	      && REGNO (SUBREG_REG (in)) < FIRST_PSEUDO_REGISTER
	      && (TEST_HARD_REG_BIT
		  (reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE],
		   REGNO (SUBREG_REG (in))))
	      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
		  != GET_MODE_SIZE (inmode)))
#endif
	  ))
    {
      in_subreg_loc = inloc;
      inloc = &SUBREG_REG (in);
      in = *inloc;
#ifndef LOAD_EXTEND_OP
      if (GET_CODE (in) == MEM)
	/* This is supposed to happen only for paradoxical subregs made by
	   combine.c.  (SUBREG (MEM)) isn't supposed to occur other ways.  */
	if (GET_MODE_SIZE (GET_MODE (in)) > GET_MODE_SIZE (inmode))
	  abort ();
#endif
      inmode = GET_MODE (in);
    }

  /* Similar issue for (SUBREG:M1 (REG:M2 ...) ...) for a hard register R where
     either M1 is not valid for R or M2 is wider than a word but we only
     need one word to store an M2-sized quantity in R.

     However, we must reload the inner reg *as well as* the subreg in
     that case.  */

  if (in != 0 && GET_CODE (in) == SUBREG
      && GET_CODE (SUBREG_REG (in)) == REG
      && REGNO (SUBREG_REG (in)) < FIRST_PSEUDO_REGISTER
      && (! HARD_REGNO_MODE_OK (REGNO (SUBREG_REG (in)), inmode)
	  || (GET_MODE_SIZE (inmode) <= UNITS_PER_WORD
	      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
		  > UNITS_PER_WORD)
	      && ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
		   / UNITS_PER_WORD)
		  != HARD_REGNO_NREGS (REGNO (SUBREG_REG (in)),
				       GET_MODE (SUBREG_REG (in)))))))
    {
      push_reload (SUBREG_REG (in), NULL_RTX, &SUBREG_REG (in), NULL_PTR,
		   GENERAL_REGS, VOIDmode, VOIDmode, 0, 0, opnum, type);
    }


  /* Similarly for paradoxical and problematical SUBREGs on the output.
     Note that there is no reason we need worry about the previous value
     of SUBREG_REG (out); even if wider than out,
     storing in a subreg is entitled to clobber it all
     (except in the case of STRICT_LOW_PART,
     and in that case the constraint should label it input-output.)  */
  if (out != 0 && GET_CODE (out) == SUBREG
      && (CONSTANT_P (SUBREG_REG (out))
	  || strict_low
	  || (((GET_CODE (SUBREG_REG (out)) == REG
		&& REGNO (SUBREG_REG (out)) >= FIRST_PSEUDO_REGISTER)
	       || GET_CODE (SUBREG_REG (out)) == MEM)
	      && ((GET_MODE_SIZE (outmode)
		   > GET_MODE_SIZE (GET_MODE (SUBREG_REG (out))))))
	  || (GET_CODE (SUBREG_REG (out)) == REG
	      && REGNO (SUBREG_REG (out)) < FIRST_PSEUDO_REGISTER
	      && ((GET_MODE_SIZE (outmode) <= UNITS_PER_WORD
		   && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (out)))
		       > UNITS_PER_WORD)
		   && ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (out)))
			/ UNITS_PER_WORD)
		       != HARD_REGNO_NREGS (REGNO (SUBREG_REG (out)),
					    GET_MODE (SUBREG_REG (out)))))
		  || ! HARD_REGNO_MODE_OK ((REGNO (SUBREG_REG (out))
					    + SUBREG_WORD (out)),
					   outmode)))
#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
	  || (SECONDARY_OUTPUT_RELOAD_CLASS (class, outmode, out) != NO_REGS
	      && (SECONDARY_OUTPUT_RELOAD_CLASS (class,
						 GET_MODE (SUBREG_REG (out)),
						 SUBREG_REG (out))
		  == NO_REGS))
#endif
#ifdef CLASS_CANNOT_CHANGE_SIZE
	  || (GET_CODE (SUBREG_REG (out)) == REG
	      && REGNO (SUBREG_REG (out)) < FIRST_PSEUDO_REGISTER
	      && (TEST_HARD_REG_BIT
		  (reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE],
		   REGNO (SUBREG_REG (out))))
	      && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (out)))
		  != GET_MODE_SIZE (outmode)))
#endif
	  ))
    {
      out_subreg_loc = outloc;
      outloc = &SUBREG_REG (out);
      out = *outloc; 
#ifndef LOAD_EXTEND_OP
     if (GET_CODE (out) == MEM
	  && GET_MODE_SIZE (GET_MODE (out)) > GET_MODE_SIZE (outmode))
	abort ();
#endif
      outmode = GET_MODE (out);
    }

  /* If IN appears in OUT, we can't share any input-only reload for IN.  */
  if (in != 0 && out != 0 && GET_CODE (out) == MEM
      && (GET_CODE (in) == REG || GET_CODE (in) == MEM)
      && reg_overlap_mentioned_for_reload_p (in, XEXP (out, 0)))
    dont_share = 1;

  /* If IN is a SUBREG of a hard register, make a new REG.  This
     simplifies some of the cases below.  */

  if (in != 0 && GET_CODE (in) == SUBREG && GET_CODE (SUBREG_REG (in)) == REG
      && REGNO (SUBREG_REG (in)) < FIRST_PSEUDO_REGISTER)
    in = gen_rtx (REG, GET_MODE (in),
		  REGNO (SUBREG_REG (in)) + SUBREG_WORD (in));

  /* Similarly for OUT.  */
  if (out != 0 && GET_CODE (out) == SUBREG
      && GET_CODE (SUBREG_REG (out)) == REG
      && REGNO (SUBREG_REG (out)) < FIRST_PSEUDO_REGISTER)
    out = gen_rtx (REG, GET_MODE (out),
		  REGNO (SUBREG_REG (out)) + SUBREG_WORD (out));

  /* Narrow down the class of register wanted if that is
     desirable on this machine for efficiency.  */
  if (in != 0)
    class = PREFERRED_RELOAD_CLASS (in, class);

  /* Output reloads may need analogous treatment, different in detail.  */
#ifdef PREFERRED_OUTPUT_RELOAD_CLASS
  if (out != 0)
    class = PREFERRED_OUTPUT_RELOAD_CLASS (out, class);
#endif

  /* Make sure we use a class that can handle the actual pseudo
     inside any subreg.  For example, on the 386, QImode regs
     can appear within SImode subregs.  Although GENERAL_REGS
     can handle SImode, QImode needs a smaller class.  */
#ifdef LIMIT_RELOAD_CLASS
  if (in_subreg_loc)
    class = LIMIT_RELOAD_CLASS (inmode, class);
  else if (in != 0 && GET_CODE (in) == SUBREG)
    class = LIMIT_RELOAD_CLASS (GET_MODE (SUBREG_REG (in)), class);

  if (out_subreg_loc)
    class = LIMIT_RELOAD_CLASS (outmode, class);
  if (out != 0 && GET_CODE (out) == SUBREG)
    class = LIMIT_RELOAD_CLASS (GET_MODE (SUBREG_REG (out)), class);
#endif

  /* Verify that this class is at least possible for the mode that
     is specified.  */
  if (this_insn_is_asm)
    {
      enum machine_mode mode;
      if (GET_MODE_SIZE (inmode) > GET_MODE_SIZE (outmode))
	mode = inmode;
      else
	mode = outmode;
      if (mode == VOIDmode)
	{
	  error_for_asm (this_insn, "cannot reload integer constant operand in `asm'");
	  mode = word_mode;
	  if (in != 0)
	    inmode = word_mode;
	  if (out != 0)
	    outmode = word_mode;
	}
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (HARD_REGNO_MODE_OK (i, mode)
	    && TEST_HARD_REG_BIT (reg_class_contents[(int) class], i))
	  {
	    int nregs = HARD_REGNO_NREGS (i, mode);

	    int j;
	    for (j = 1; j < nregs; j++)
	      if (! TEST_HARD_REG_BIT (reg_class_contents[(int) class], i + j))
		break;
	    if (j == nregs)
	      break;
	  }
      if (i == FIRST_PSEUDO_REGISTER)
	{
	  error_for_asm (this_insn, "impossible register constraint in `asm'");
	  class = ALL_REGS;
	}
    }

  if (class == NO_REGS)
    abort ();

  /* We can use an existing reload if the class is right
     and at least one of IN and OUT is a match
     and the other is at worst neutral.
     (A zero compared against anything is neutral.) 

     If SMALL_REGISTER_CLASSES, don't use existing reloads unless they are
     for the same thing since that can cause us to need more reload registers
     than we otherwise would.  */

  for (i = 0; i < n_reloads; i++)
    if ((reg_class_subset_p (class, reload_reg_class[i])
	 || reg_class_subset_p (reload_reg_class[i], class))
	/* If the existing reload has a register, it must fit our class.  */
	&& (reload_reg_rtx[i] == 0
	    || TEST_HARD_REG_BIT (reg_class_contents[(int) class],
				  true_regnum (reload_reg_rtx[i])))
	&& ((in != 0 && MATCHES (reload_in[i], in) && ! dont_share
	     && (out == 0 || reload_out[i] == 0 || MATCHES (reload_out[i], out)))
	    ||
	    (out != 0 && MATCHES (reload_out[i], out)
	     && (in == 0 || reload_in[i] == 0 || MATCHES (reload_in[i], in))))
	&& (reg_class_size[(int) class] == 1
#ifdef SMALL_REGISTER_CLASSES
	    || 1
#endif
	    )
	&& MERGABLE_RELOADS (type, reload_when_needed[i],
			     opnum, reload_opnum[i]))
      break;

  /* Reloading a plain reg for input can match a reload to postincrement
     that reg, since the postincrement's value is the right value.
     Likewise, it can match a preincrement reload, since we regard
     the preincrementation as happening before any ref in this insn
     to that register.  */
  if (i == n_reloads)
    for (i = 0; i < n_reloads; i++)
      if ((reg_class_subset_p (class, reload_reg_class[i])
	   || reg_class_subset_p (reload_reg_class[i], class))
	  /* If the existing reload has a register, it must fit our class.  */
	  && (reload_reg_rtx[i] == 0
	      || TEST_HARD_REG_BIT (reg_class_contents[(int) class],
				    true_regnum (reload_reg_rtx[i])))
	  && out == 0 && reload_out[i] == 0 && reload_in[i] != 0
	  && ((GET_CODE (in) == REG
	       && (GET_CODE (reload_in[i]) == POST_INC
		   || GET_CODE (reload_in[i]) == POST_DEC
		   || GET_CODE (reload_in[i]) == PRE_INC
		   || GET_CODE (reload_in[i]) == PRE_DEC)
	       && MATCHES (XEXP (reload_in[i], 0), in))
	      ||
	      (GET_CODE (reload_in[i]) == REG
	       && (GET_CODE (in) == POST_INC
		   || GET_CODE (in) == POST_DEC
		   || GET_CODE (in) == PRE_INC
		   || GET_CODE (in) == PRE_DEC)
	       && MATCHES (XEXP (in, 0), reload_in[i])))
	  && (reg_class_size[(int) class] == 1
#ifdef SMALL_REGISTER_CLASSES
	      || 1
#endif
	      )
	  && MERGABLE_RELOADS (type, reload_when_needed[i],
			       opnum, reload_opnum[i]))
	{
	  /* Make sure reload_in ultimately has the increment,
	     not the plain register.  */
	  if (GET_CODE (in) == REG)
	    in = reload_in[i];
	  break;
	}

  if (i == n_reloads)
    {
      /* See if we need a secondary reload register to move between CLASS
	 and IN or CLASS and OUT.  Get the icode and push any required reloads
	 needed for each of them if so.  */

#ifdef SECONDARY_INPUT_RELOAD_CLASS
      if (in != 0)
	secondary_in_reload
	  = push_secondary_reload (1, in, opnum, optional, class, inmode, type,
				   &secondary_in_icode);
#endif

#ifdef SECONDARY_OUTPUT_RELOAD_CLASS
      if (out != 0 && GET_CODE (out) != SCRATCH)
	secondary_out_reload
	  = push_secondary_reload (0, out, opnum, optional, class, outmode,
				   type, &secondary_out_icode);
#endif

      /* We found no existing reload suitable for re-use.
	 So add an additional reload.  */

      i = n_reloads;
      reload_in[i] = in;
      reload_out[i] = out;
      reload_reg_class[i] = class;
      reload_inmode[i] = inmode;
      reload_outmode[i] = outmode;
      reload_reg_rtx[i] = 0;
      reload_optional[i] = optional;
      reload_inc[i] = 0;
      reload_nocombine[i] = 0;
      reload_in_reg[i] = inloc ? *inloc : 0;
      reload_opnum[i] = opnum;
      reload_when_needed[i] = type;
      reload_secondary_in_reload[i] = secondary_in_reload;
      reload_secondary_out_reload[i] = secondary_out_reload;
      reload_secondary_in_icode[i] = secondary_in_icode;
      reload_secondary_out_icode[i] = secondary_out_icode;
      reload_secondary_p[i] = 0;

      n_reloads++;

#ifdef SECONDARY_MEMORY_NEEDED
      /* If a memory location is needed for the copy, make one.  */
      if (in != 0 && GET_CODE (in) == REG
	  && REGNO (in) < FIRST_PSEUDO_REGISTER
	  && SECONDARY_MEMORY_NEEDED (REGNO_REG_CLASS (REGNO (in)),
				     class, inmode))
	get_secondary_mem (in, inmode, opnum, type);

      if (out != 0 && GET_CODE (out) == REG
	  && REGNO (out) < FIRST_PSEUDO_REGISTER
	  && SECONDARY_MEMORY_NEEDED (class, REGNO_REG_CLASS (REGNO (out)),
				      outmode))
	get_secondary_mem (out, outmode, opnum, type);
#endif
    }
  else
    {
      /* We are reusing an existing reload,
	 but we may have additional information for it.
	 For example, we may now have both IN and OUT
	 while the old one may have just one of them.  */

      if (inmode != VOIDmode)
	reload_inmode[i] = inmode;
      if (outmode != VOIDmode)
	reload_outmode[i] = outmode;
      if (in != 0)
	reload_in[i] = in;
      if (out != 0)
	reload_out[i] = out;
      if (reg_class_subset_p (class, reload_reg_class[i]))
	reload_reg_class[i] = class;
      reload_optional[i] &= optional;
      if (MERGE_TO_OTHER (type, reload_when_needed[i],
			  opnum, reload_opnum[i]))
	reload_when_needed[i] = RELOAD_OTHER;
      reload_opnum[i] = MIN (reload_opnum[i], opnum);
    }

  /* If the ostensible rtx being reload differs from the rtx found
     in the location to substitute, this reload is not safe to combine
     because we cannot reliably tell whether it appears in the insn.  */

  if (in != 0 && in != *inloc)
    reload_nocombine[i] = 1;

#if 0
  /* This was replaced by changes in find_reloads_address_1 and the new
     function inc_for_reload, which go with a new meaning of reload_inc.  */

  /* If this is an IN/OUT reload in an insn that sets the CC,
     it must be for an autoincrement.  It doesn't work to store
     the incremented value after the insn because that would clobber the CC.
     So we must do the increment of the value reloaded from,
     increment it, store it back, then decrement again.  */
  if (out != 0 && sets_cc0_p (PATTERN (this_insn)))
    {
      out = 0;
      reload_out[i] = 0;
      reload_inc[i] = find_inc_amount (PATTERN (this_insn), in);
      /* If we did not find a nonzero amount-to-increment-by,
	 that contradicts the belief that IN is being incremented
	 in an address in this insn.  */
      if (reload_inc[i] == 0)
	abort ();
    }
#endif

  /* If we will replace IN and OUT with the reload-reg,
     record where they are located so that substitution need
     not do a tree walk.  */

  if (replace_reloads)
    {
      if (inloc != 0)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->subreg_loc = in_subreg_loc;
	  r->where = inloc;
	  r->mode = inmode;
	}
      if (outloc != 0 && outloc != inloc)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->where = outloc;
	  r->subreg_loc = out_subreg_loc;
	  r->mode = outmode;
	}
    }

  /* If this reload is just being introduced and it has both
     an incoming quantity and an outgoing quantity that are
     supposed to be made to match, see if either one of the two
     can serve as the place to reload into.

     If one of them is acceptable, set reload_reg_rtx[i]
     to that one.  */

  if (in != 0 && out != 0 && in != out && reload_reg_rtx[i] == 0)
    {
      reload_reg_rtx[i] = find_dummy_reload (in, out, inloc, outloc,
					     inmode, outmode,
					     reload_reg_class[i], i);

      /* If the outgoing register already contains the same value
	 as the incoming one, we can dispense with loading it.
	 The easiest way to tell the caller that is to give a phony
	 value for the incoming operand (same as outgoing one).  */
      if (reload_reg_rtx[i] == out
	  && (GET_CODE (in) == REG || CONSTANT_P (in))
	  && 0 != find_equiv_reg (in, this_insn, 0, REGNO (out),
				  static_reload_reg_p, i, inmode))
	reload_in[i] = out;
    }

  /* If this is an input reload and the operand contains a register that
     dies in this insn and is used nowhere else, see if it is the right class
     to be used for this reload.  Use it if so.  (This occurs most commonly
     in the case of paradoxical SUBREGs and in-out reloads).  We cannot do
     this if it is also an output reload that mentions the register unless
     the output is a SUBREG that clobbers an entire register.

     Note that the operand might be one of the spill regs, if it is a
     pseudo reg and we are in a block where spilling has not taken place.
     But if there is no spilling in this block, that is OK.
     An explicitly used hard reg cannot be a spill reg.  */

  if (reload_reg_rtx[i] == 0 && in != 0)
    {
      rtx note;
      int regno;

      for (note = REG_NOTES (this_insn); note; note = XEXP (note, 1))
	if (REG_NOTE_KIND (note) == REG_DEAD
	    && GET_CODE (XEXP (note, 0)) == REG
	    && (regno = REGNO (XEXP (note, 0))) < FIRST_PSEUDO_REGISTER
	    && reg_mentioned_p (XEXP (note, 0), in)
	    && ! refers_to_regno_for_reload_p (regno,
					       (regno
						+ HARD_REGNO_NREGS (regno,
								    inmode)),
					       PATTERN (this_insn), inloc)
	    /* If this is also an output reload, IN cannot be used as
	       the reload register if it is set in this insn unless IN
	       is also OUT.  */
	    && (out == 0 || in == out
		|| ! hard_reg_set_here_p (regno,
					  (regno
					   + HARD_REGNO_NREGS (regno,
							       inmode)),
					  PATTERN (this_insn)))
	    /* ??? Why is this code so different from the previous?
	       Is there any simple coherent way to describe the two together?
	       What's going on here.  */
	    && (in != out
		|| (GET_CODE (in) == SUBREG
		    && (((GET_MODE_SIZE (GET_MODE (in)) + (UNITS_PER_WORD - 1))
			 / UNITS_PER_WORD)
			== ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (in)))
			     + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD))))
	    /* Make sure the operand fits in the reg that dies.  */
	    && GET_MODE_SIZE (inmode) <= GET_MODE_SIZE (GET_MODE (XEXP (note, 0)))
	    && HARD_REGNO_MODE_OK (regno, inmode)
	    && GET_MODE_SIZE (outmode) <= GET_MODE_SIZE (GET_MODE (XEXP (note, 0)))
	    && HARD_REGNO_MODE_OK (regno, outmode)
	    && TEST_HARD_REG_BIT (reg_class_contents[(int) class], regno)
	    && !fixed_regs[regno])
	  {
	    reload_reg_rtx[i] = gen_rtx (REG, inmode, regno);
	    break;
	  }
    }

  if (out)
    output_reloadnum = i;

  return i;
}

/* Record an additional place we must replace a value
   for which we have already recorded a reload.
   RELOADNUM is the value returned by push_reload
   when the reload was recorded.
   This is used in insn patterns that use match_dup.  */

static void
push_replacement (loc, reloadnum, mode)
     rtx *loc;
     int reloadnum;
     enum machine_mode mode;
{
  if (replace_reloads)
    {
      register struct replacement *r = &replacements[n_replacements++];
      r->what = reloadnum;
      r->where = loc;
      r->subreg_loc = 0;
      r->mode = mode;
    }
}

/* Transfer all replacements that used to be in reload FROM to be in
   reload TO.  */

void
transfer_replacements (to, from)
     int to, from;
{
  int i;

  for (i = 0; i < n_replacements; i++)
    if (replacements[i].what == from)
      replacements[i].what = to;
}

/* If there is only one output reload, and it is not for an earlyclobber
   operand, try to combine it with a (logically unrelated) input reload
   to reduce the number of reload registers needed.

   This is safe if the input reload does not appear in
   the value being output-reloaded, because this implies
   it is not needed any more once the original insn completes.

   If that doesn't work, see we can use any of the registers that
   die in this insn as a reload register.  We can if it is of the right
   class and does not appear in the value being output-reloaded.  */

static void
combine_reloads ()
{
  int i;
  int output_reload = -1;
  rtx note;

  /* Find the output reload; return unless there is exactly one
     and that one is mandatory.  */

  for (i = 0; i < n_reloads; i++)
    if (reload_out[i] != 0)
      {
	if (output_reload >= 0)
	  return;
	output_reload = i;
      }

  if (output_reload < 0 || reload_optional[output_reload])
    return;

  /* An input-output reload isn't combinable.  */

  if (reload_in[output_reload] != 0)
    return;

  /* If this reload is for an earlyclobber operand, we can't do anything.  */
  if (earlyclobber_operand_p (reload_out[output_reload]))
    return;

  /* Check each input reload; can we combine it?  */

  for (i = 0; i < n_reloads; i++)
    if (reload_in[i] && ! reload_optional[i] && ! reload_nocombine[i]
	/* Life span of this reload must not extend past main insn.  */
	&& reload_when_needed[i] != RELOAD_FOR_OUTPUT_ADDRESS
	&& reload_when_needed[i] != RELOAD_OTHER
	&& (CLASS_MAX_NREGS (reload_reg_class[i], reload_inmode[i])
	    == CLASS_MAX_NREGS (reload_reg_class[output_reload],
				reload_outmode[output_reload]))
	&& reload_inc[i] == 0
	&& reload_reg_rtx[i] == 0
#ifdef SECONDARY_MEMORY_NEEDED
	/* Don't combine two reloads with different secondary
	   memory locations.  */
	&& (secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[i]] == 0
	    || secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[output_reload]] == 0
	    || rtx_equal_p (secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[i]],
			    secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[output_reload]]))
#endif
#ifdef SMALL_REGISTER_CLASSES
	&& reload_reg_class[i] == reload_reg_class[output_reload]
#else
	&& (reg_class_subset_p (reload_reg_class[i],
				reload_reg_class[output_reload])
	    || reg_class_subset_p (reload_reg_class[output_reload],
				   reload_reg_class[i]))
#endif
	&& (MATCHES (reload_in[i], reload_out[output_reload])
	    /* Args reversed because the first arg seems to be
	       the one that we imagine being modified
	       while the second is the one that might be affected.  */
	    || (! reg_overlap_mentioned_for_reload_p (reload_out[output_reload],
						      reload_in[i])
		/* However, if the input is a register that appears inside
		   the output, then we also can't share.
		   Imagine (set (mem (reg 69)) (plus (reg 69) ...)).
		   If the same reload reg is used for both reg 69 and the
		   result to be stored in memory, then that result
		   will clobber the address of the memory ref.  */
		&& ! (GET_CODE (reload_in[i]) == REG
		      && reg_overlap_mentioned_for_reload_p (reload_in[i],
							     reload_out[output_reload]))))
	&& (reg_class_size[(int) reload_reg_class[i]]
#ifdef SMALL_REGISTER_CLASSES
	     || 1
#endif
	    )
	/* We will allow making things slightly worse by combining an
	   input and an output, but no worse than that.  */
	&& (reload_when_needed[i] == RELOAD_FOR_INPUT
	    || reload_when_needed[i] == RELOAD_FOR_OUTPUT))
      {
	int j;

	/* We have found a reload to combine with!  */
	reload_out[i] = reload_out[output_reload];
	reload_outmode[i] = reload_outmode[output_reload];
	/* Mark the old output reload as inoperative.  */
	reload_out[output_reload] = 0;
	/* The combined reload is needed for the entire insn.  */
	reload_when_needed[i] = RELOAD_OTHER;
	/* If the output reload had a secondary reload, copy it. */
	if (reload_secondary_out_reload[output_reload] != -1)
	  {
	    reload_secondary_out_reload[i]
	      = reload_secondary_out_reload[output_reload];
	    reload_secondary_out_icode[i]
	      = reload_secondary_out_icode[output_reload];
	  }

#ifdef SECONDARY_MEMORY_NEEDED
	/* Copy any secondary MEM.  */
	if (secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[output_reload]] != 0)
	  secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[i]]
	    = secondary_memlocs_elim[(int) reload_outmode[output_reload]][reload_opnum[output_reload]];
#endif
	/* If required, minimize the register class. */
	if (reg_class_subset_p (reload_reg_class[output_reload],
				reload_reg_class[i]))
	  reload_reg_class[i] = reload_reg_class[output_reload];

	/* Transfer all replacements from the old reload to the combined.  */
	for (j = 0; j < n_replacements; j++)
	  if (replacements[j].what == output_reload)
	    replacements[j].what = i;

	return;
      }

  /* If this insn has only one operand that is modified or written (assumed
     to be the first),  it must be the one corresponding to this reload.  It
     is safe to use anything that dies in this insn for that output provided
     that it does not occur in the output (we already know it isn't an
     earlyclobber.  If this is an asm insn, give up.  */

  if (INSN_CODE (this_insn) == -1)
    return;

  for (i = 1; i < insn_n_operands[INSN_CODE (this_insn)]; i++)
    if (insn_operand_constraint[INSN_CODE (this_insn)][i][0] == '='
	|| insn_operand_constraint[INSN_CODE (this_insn)][i][0] == '+')
      return;

  /* See if some hard register that dies in this insn and is not used in
     the output is the right class.  Only works if the register we pick
     up can fully hold our output reload.  */
  for (note = REG_NOTES (this_insn); note; note = XEXP (note, 1))
    if (REG_NOTE_KIND (note) == REG_DEAD
	&& GET_CODE (XEXP (note, 0)) == REG
	&& ! reg_overlap_mentioned_for_reload_p (XEXP (note, 0),
						 reload_out[output_reload])
	&& REGNO (XEXP (note, 0)) < FIRST_PSEUDO_REGISTER
	&& HARD_REGNO_MODE_OK (REGNO (XEXP (note, 0)), reload_outmode[output_reload])
	&& TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[output_reload]],
			      REGNO (XEXP (note, 0)))
	&& (HARD_REGNO_NREGS (REGNO (XEXP (note, 0)), reload_outmode[output_reload])
	    <= HARD_REGNO_NREGS (REGNO (XEXP (note, 0)), GET_MODE (XEXP (note, 0))))
	&& ! fixed_regs[REGNO (XEXP (note, 0))])
      {
	reload_reg_rtx[output_reload] = gen_rtx (REG,
						 reload_outmode[output_reload],
						 REGNO (XEXP (note, 0)));
	return;
      }
}

/* Try to find a reload register for an in-out reload (expressions IN and OUT).
   See if one of IN and OUT is a register that may be used;
   this is desirable since a spill-register won't be needed.
   If so, return the register rtx that proves acceptable.

   INLOC and OUTLOC are locations where IN and OUT appear in the insn.
   CLASS is the register class required for the reload.

   If FOR_REAL is >= 0, it is the number of the reload,
   and in some cases when it can be discovered that OUT doesn't need
   to be computed, clear out reload_out[FOR_REAL].

   If FOR_REAL is -1, this should not be done, because this call
   is just to see if a register can be found, not to find and install it.  */

static rtx
find_dummy_reload (real_in, real_out, inloc, outloc,
		   inmode, outmode, class, for_real)
     rtx real_in, real_out;
     rtx *inloc, *outloc;
     enum machine_mode inmode, outmode;
     enum reg_class class;
     int for_real;
{
  rtx in = real_in;
  rtx out = real_out;
  int in_offset = 0;
  int out_offset = 0;
  rtx value = 0;

  /* If operands exceed a word, we can't use either of them
     unless they have the same size.  */
  if (GET_MODE_SIZE (outmode) != GET_MODE_SIZE (inmode)
      && (GET_MODE_SIZE (outmode) > UNITS_PER_WORD
	  || GET_MODE_SIZE (inmode) > UNITS_PER_WORD))
    return 0;

  /* Find the inside of any subregs.  */
  while (GET_CODE (out) == SUBREG)
    {
      out_offset = SUBREG_WORD (out);
      out = SUBREG_REG (out);
    }
  while (GET_CODE (in) == SUBREG)
    {
      in_offset = SUBREG_WORD (in);
      in = SUBREG_REG (in);
    }

  /* Narrow down the reg class, the same way push_reload will;
     otherwise we might find a dummy now, but push_reload won't.  */
  class = PREFERRED_RELOAD_CLASS (in, class);

  /* See if OUT will do.  */
  if (GET_CODE (out) == REG
      && REGNO (out) < FIRST_PSEUDO_REGISTER)
    {
      register int regno = REGNO (out) + out_offset;
      int nwords = HARD_REGNO_NREGS (regno, outmode);
      rtx saved_rtx;

      /* When we consider whether the insn uses OUT,
	 ignore references within IN.  They don't prevent us
	 from copying IN into OUT, because those refs would
	 move into the insn that reloads IN.

	 However, we only ignore IN in its role as this reload.
	 If the insn uses IN elsewhere and it contains OUT,
	 that counts.  We can't be sure it's the "same" operand
	 so it might not go through this reload.  */
      saved_rtx = *inloc;
      *inloc = const0_rtx;

      if (regno < FIRST_PSEUDO_REGISTER
	  /* A fixed reg that can overlap other regs better not be used
	     for reloading in any way.  */
#ifdef OVERLAPPING_REGNO_P
	  && ! (fixed_regs[regno] && OVERLAPPING_REGNO_P (regno))
#endif
	  && ! refers_to_regno_for_reload_p (regno, regno + nwords,
					     PATTERN (this_insn), outloc))
	{
	  int i;
	  for (i = 0; i < nwords; i++)
	    if (! TEST_HARD_REG_BIT (reg_class_contents[(int) class],
				     regno + i))
	      break;

	  if (i == nwords)
	    {
	      if (GET_CODE (real_out) == REG)
		value = real_out;
	      else
		value = gen_rtx (REG, outmode, regno);
	    }
	}

      *inloc = saved_rtx;
    }

  /* Consider using IN if OUT was not acceptable
     or if OUT dies in this insn (like the quotient in a divmod insn).
     We can't use IN unless it is dies in this insn,
     which means we must know accurately which hard regs are live.
     Also, the result can't go in IN if IN is used within OUT.  */
  if (hard_regs_live_known
      && GET_CODE (in) == REG
      && REGNO (in) < FIRST_PSEUDO_REGISTER
      && (value == 0
	  || find_reg_note (this_insn, REG_UNUSED, real_out))
      && find_reg_note (this_insn, REG_DEAD, real_in)
      && !fixed_regs[REGNO (in)]
      && HARD_REGNO_MODE_OK (REGNO (in),
			     /* The only case where out and real_out might
				have different modes is where real_out
				is a subreg, and in that case, out
				has a real mode.  */
			     (GET_MODE (out) != VOIDmode
			      ? GET_MODE (out) : outmode)))
    {
      register int regno = REGNO (in) + in_offset;
      int nwords = HARD_REGNO_NREGS (regno, inmode);

      if (! refers_to_regno_for_reload_p (regno, regno + nwords, out, NULL_PTR)
	  && ! hard_reg_set_here_p (regno, regno + nwords,
				    PATTERN (this_insn)))
	{
	  int i;
	  for (i = 0; i < nwords; i++)
	    if (! TEST_HARD_REG_BIT (reg_class_contents[(int) class],
				     regno + i))
	      break;

	  if (i == nwords)
	    {
	      /* If we were going to use OUT as the reload reg
		 and changed our mind, it means OUT is a dummy that
		 dies here.  So don't bother copying value to it.  */
	      if (for_real >= 0 && value == real_out)
		reload_out[for_real] = 0;
	      if (GET_CODE (real_in) == REG)
		value = real_in;
	      else
		value = gen_rtx (REG, inmode, regno);
	    }
	}
    }

  return value;
}

/* This page contains subroutines used mainly for determining
   whether the IN or an OUT of a reload can serve as the
   reload register.  */

/* Return 1 if X is an operand of an insn that is being earlyclobbered.  */

static int
earlyclobber_operand_p (x)
     rtx x;
{
  int i;

  for (i = 0; i < n_earlyclobbers; i++)
    if (reload_earlyclobbers[i] == x)
      return 1;

  return 0;
}

/* Return 1 if expression X alters a hard reg in the range
   from BEG_REGNO (inclusive) to END_REGNO (exclusive),
   either explicitly or in the guise of a pseudo-reg allocated to REGNO.
   X should be the body of an instruction.  */

static int
hard_reg_set_here_p (beg_regno, end_regno, x)
     register int beg_regno, end_regno;
     rtx x;
{
  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      register rtx op0 = SET_DEST (x);
      while (GET_CODE (op0) == SUBREG)
	op0 = SUBREG_REG (op0);
      if (GET_CODE (op0) == REG)
	{
	  register int r = REGNO (op0);
	  /* See if this reg overlaps range under consideration.  */
	  if (r < end_regno
	      && r + HARD_REGNO_NREGS (r, GET_MODE (op0)) > beg_regno)
	    return 1;
	}
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i = XVECLEN (x, 0) - 1;
      for (; i >= 0; i--)
	if (hard_reg_set_here_p (beg_regno, end_regno, XVECEXP (x, 0, i)))
	  return 1;
    }

  return 0;
}

/* Return 1 if ADDR is a valid memory address for mode MODE,
   and check that each pseudo reg has the proper kind of
   hard reg.  */

int
strict_memory_address_p (mode, addr)
     enum machine_mode mode;
     register rtx addr;
{
  GO_IF_LEGITIMATE_ADDRESS (mode, addr, win);
  return 0;

 win:
  return 1;
}

/* Like rtx_equal_p except that it allows a REG and a SUBREG to match
   if they are the same hard reg, and has special hacks for
   autoincrement and autodecrement.
   This is specifically intended for find_reloads to use
   in determining whether two operands match.
   X is the operand whose number is the lower of the two.

   The value is 2 if Y contains a pre-increment that matches
   a non-incrementing address in X.  */

/* ??? To be completely correct, we should arrange to pass
   for X the output operand and for Y the input operand.
   For now, we assume that the output operand has the lower number
   because that is natural in (SET output (... input ...)).  */

int
operands_match_p (x, y)
     register rtx x, y;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;
  int success_2;
      
  if (x == y)
    return 1;
  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      register int j;

      if (code == SUBREG)
	{
	  i = REGNO (SUBREG_REG (x));
	  if (i >= FIRST_PSEUDO_REGISTER)
	    goto slow;
	  i += SUBREG_WORD (x);
	}
      else
	i = REGNO (x);

      if (GET_CODE (y) == SUBREG)
	{
	  j = REGNO (SUBREG_REG (y));
	  if (j >= FIRST_PSEUDO_REGISTER)
	    goto slow;
	  j += SUBREG_WORD (y);
	}
      else
	j = REGNO (y);

      /* On a WORDS_BIG_ENDIAN machine, point to the last register of a
	 multiple hard register group, so that for example (reg:DI 0) and
	 (reg:SI 1) will be considered the same register.  */
      if (WORDS_BIG_ENDIAN && GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD
	  && i < FIRST_PSEUDO_REGISTER)
	i += (GET_MODE_SIZE (GET_MODE (x)) / UNITS_PER_WORD) - 1;
      if (WORDS_BIG_ENDIAN && GET_MODE_SIZE (GET_MODE (y)) > UNITS_PER_WORD
	  && j < FIRST_PSEUDO_REGISTER)
	j += (GET_MODE_SIZE (GET_MODE (y)) / UNITS_PER_WORD) - 1;

      return i == j;
    }
  /* If two operands must match, because they are really a single
     operand of an assembler insn, then two postincrements are invalid
     because the assembler insn would increment only once.
     On the other hand, an postincrement matches ordinary indexing
     if the postincrement is the output operand.  */
  if (code == POST_DEC || code == POST_INC)
    return operands_match_p (XEXP (x, 0), y);
  /* Two preincrements are invalid
     because the assembler insn would increment only once.
     On the other hand, an preincrement matches ordinary indexing
     if the preincrement is the input operand.
     In this case, return 2, since some callers need to do special
     things when this happens.  */
  if (GET_CODE (y) == PRE_DEC || GET_CODE (y) == PRE_INC)
    return operands_match_p (x, XEXP (y, 0)) ? 2 : 0;

 slow:

  /* Now we have disposed of all the cases 
     in which different rtx codes can match.  */
  if (code != GET_CODE (y))
    return 0;
  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  success_2 = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int val;
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'e':
	  val = operands_match_p (XEXP (x, i), XEXP (y, i));
	  if (val == 0)
	    return 0;
	  /* If any subexpression returns 2,
	     we should return 2 if we are successful.  */
	  if (val == 2)
	    success_2 = 1;
	  break;

	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1 + success_2;
}

/* Return the number of times character C occurs in string S.  */

int
n_occurrences (c, s)
     int c;
     char *s;
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}

/* Describe the range of registers or memory referenced by X.
   If X is a register, set REG_FLAG and put the first register 
   number into START and the last plus one into END.
   If X is a memory reference, put a base address into BASE 
   and a range of integer offsets into START and END.
   If X is pushing on the stack, we can assume it causes no trouble, 
   so we set the SAFE field.  */

static struct decomposition
decompose (x)
     rtx x;
{
  struct decomposition val;
  int all_const = 0;

  val.reg_flag = 0;
  val.safe = 0;
  if (GET_CODE (x) == MEM)
    {
      rtx base, offset = 0;
      rtx addr = XEXP (x, 0);

      if (GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == PRE_INC
	  || GET_CODE (addr) == POST_DEC || GET_CODE (addr) == POST_INC)
	{
	  val.base = XEXP (addr, 0);
	  val.start = - GET_MODE_SIZE (GET_MODE (x));
	  val.end = GET_MODE_SIZE (GET_MODE (x));
	  val.safe = REGNO (val.base) == STACK_POINTER_REGNUM;
	  return val;
	}

      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  all_const = 1;
	}
      if (GET_CODE (addr) == PLUS)
	{
	  if (CONSTANT_P (XEXP (addr, 0)))
	    {
	      base = XEXP (addr, 1);
	      offset = XEXP (addr, 0);
	    }
	  else if (CONSTANT_P (XEXP (addr, 1)))
	    {
	      base = XEXP (addr, 0);
	      offset = XEXP (addr, 1);
	    }
	}

      if (offset == 0)
	{
	  base = addr;
	  offset = const0_rtx;
	} 
      if (GET_CODE (offset) == CONST)
	offset = XEXP (offset, 0);
      if (GET_CODE (offset) == PLUS)
	{
	  if (GET_CODE (XEXP (offset, 0)) == CONST_INT)
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, XEXP (offset, 1));
	      offset = XEXP (offset, 0);
	    }
	  else if (GET_CODE (XEXP (offset, 1)) == CONST_INT)
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, XEXP (offset, 0));
	      offset = XEXP (offset, 1);
	    }
	  else
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, offset);
	      offset = const0_rtx;
	    }
	}
      else if (GET_CODE (offset) != CONST_INT)
	{
	  base = gen_rtx (PLUS, GET_MODE (base), base, offset);
	  offset = const0_rtx;
	}

      if (all_const && GET_CODE (base) == PLUS)
	base = gen_rtx (CONST, GET_MODE (base), base);

      if (GET_CODE (offset) != CONST_INT)
	abort ();

      val.start = INTVAL (offset);
      val.end = val.start + GET_MODE_SIZE (GET_MODE (x));
      val.base = base;
      return val;
    }
  else if (GET_CODE (x) == REG)
    {
      val.reg_flag = 1;
      val.start = true_regnum (x); 
      if (val.start < 0)
	{
	  /* A pseudo with no hard reg.  */
	  val.start = REGNO (x);
	  val.end = val.start + 1;
	}
      else
	/* A hard reg.  */
	val.end = val.start + HARD_REGNO_NREGS (val.start, GET_MODE (x));
    }
  else if (GET_CODE (x) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (x)) != REG)
	/* This could be more precise, but it's good enough.  */
	return decompose (SUBREG_REG (x));
      val.reg_flag = 1;
      val.start = true_regnum (x); 
      if (val.start < 0)
	return decompose (SUBREG_REG (x));
      else
	/* A hard reg.  */
	val.end = val.start + HARD_REGNO_NREGS (val.start, GET_MODE (x));
    }
  else if (CONSTANT_P (x)
	   /* This hasn't been assigned yet, so it can't conflict yet.  */
	   || GET_CODE (x) == SCRATCH)
    val.safe = 1;
  else
    abort ();
  return val;
}

/* Return 1 if altering Y will not modify the value of X.
   Y is also described by YDATA, which should be decompose (Y).  */

static int
immune_p (x, y, ydata)
     rtx x, y;
     struct decomposition ydata;
{
  struct decomposition xdata;

  if (ydata.reg_flag)
    return !refers_to_regno_for_reload_p (ydata.start, ydata.end, x, NULL_PTR);
  if (ydata.safe)
    return 1;

  if (GET_CODE (y) != MEM)
    abort ();
  /* If Y is memory and X is not, Y can't affect X.  */
  if (GET_CODE (x) != MEM)
    return 1;

  xdata =  decompose (x);

  if (! rtx_equal_p (xdata.base, ydata.base))
    {
      /* If bases are distinct symbolic constants, there is no overlap.  */
      if (CONSTANT_P (xdata.base) && CONSTANT_P (ydata.base))
	return 1;
      /* Constants and stack slots never overlap.  */
      if (CONSTANT_P (xdata.base)
	  && (ydata.base == frame_pointer_rtx
	      || ydata.base == hard_frame_pointer_rtx
	      || ydata.base == stack_pointer_rtx))
	return 1;
      if (CONSTANT_P (ydata.base)
	  && (xdata.base == frame_pointer_rtx
	      || xdata.base == hard_frame_pointer_rtx
	      || xdata.base == stack_pointer_rtx))
	return 1;
      /* If either base is variable, we don't know anything.  */
      return 0;
    }


  return (xdata.start >= ydata.end || ydata.start >= xdata.end);
}

/* Similar, but calls decompose.  */

int
safe_from_earlyclobber (op, clobber)
     rtx op, clobber;
{
  struct decomposition early_data;

  early_data = decompose (clobber);
  return immune_p (op, clobber, early_data);
}

/* Main entry point of this file: search the body of INSN
   for values that need reloading and record them with push_reload.
   REPLACE nonzero means record also where the values occur
   so that subst_reloads can be used.

   IND_LEVELS says how many levels of indirection are supported by this
   machine; a value of zero means that a memory reference is not a valid
   memory address.

   LIVE_KNOWN says we have valid information about which hard
   regs are live at each point in the program; this is true when
   we are called from global_alloc but false when stupid register
   allocation has been done.

   RELOAD_REG_P if nonzero is a vector indexed by hard reg number
   which is nonnegative if the reg has been commandeered for reloading into.
   It is copied into STATIC_RELOAD_REG_P and referenced from there
   by various subroutines.  */

void
find_reloads (insn, replace, ind_levels, live_known, reload_reg_p)
     rtx insn;
     int replace, ind_levels;
     int live_known;
     short *reload_reg_p;
{
#ifdef REGISTER_CONSTRAINTS

  register int insn_code_number;
  register int i, j;
  int noperands;
  /* These are the constraints for the insn.  We don't change them.  */
  char *constraints1[MAX_RECOG_OPERANDS];
  /* These start out as the constraints for the insn
     and they are chewed up as we consider alternatives.  */
  char *constraints[MAX_RECOG_OPERANDS];
  /* These are the preferred classes for an operand, or NO_REGS if it isn't
     a register.  */
  enum reg_class preferred_class[MAX_RECOG_OPERANDS];
  char pref_or_nothing[MAX_RECOG_OPERANDS];
  /* Nonzero for a MEM operand whose entire address needs a reload.  */
  int address_reloaded[MAX_RECOG_OPERANDS];
  /* Value of enum reload_type to use for operand.  */
  enum reload_type operand_type[MAX_RECOG_OPERANDS];
  /* Value of enum reload_type to use within address of operand.  */
  enum reload_type address_type[MAX_RECOG_OPERANDS];
  /* Save the usage of each operand.  */
  enum reload_usage { RELOAD_READ, RELOAD_READ_WRITE, RELOAD_WRITE } modified[MAX_RECOG_OPERANDS];
  int no_input_reloads = 0, no_output_reloads = 0;
  int n_alternatives;
  int this_alternative[MAX_RECOG_OPERANDS];
  char this_alternative_win[MAX_RECOG_OPERANDS];
  char this_alternative_offmemok[MAX_RECOG_OPERANDS];
  char this_alternative_earlyclobber[MAX_RECOG_OPERANDS];
  int this_alternative_matches[MAX_RECOG_OPERANDS];
  int swapped;
  int goal_alternative[MAX_RECOG_OPERANDS];
  int this_alternative_number;
  int goal_alternative_number;
  int operand_reloadnum[MAX_RECOG_OPERANDS];
  int goal_alternative_matches[MAX_RECOG_OPERANDS];
  int goal_alternative_matched[MAX_RECOG_OPERANDS];
  char goal_alternative_win[MAX_RECOG_OPERANDS];
  char goal_alternative_offmemok[MAX_RECOG_OPERANDS];
  char goal_alternative_earlyclobber[MAX_RECOG_OPERANDS];
  int goal_alternative_swapped;
  int best;
  int commutative;
  char operands_match[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  rtx substed_operand[MAX_RECOG_OPERANDS];
  rtx body = PATTERN (insn);
  rtx set = single_set (insn);
  int goal_earlyclobber, this_earlyclobber;
  enum machine_mode operand_mode[MAX_RECOG_OPERANDS];

  this_insn = insn;
  this_insn_is_asm = 0;		/* Tentative.  */
  n_reloads = 0;
  n_replacements = 0;
  n_memlocs = 0;
  n_earlyclobbers = 0;
  replace_reloads = replace;
  hard_regs_live_known = live_known;
  static_reload_reg_p = reload_reg_p;

  /* JUMP_INSNs and CALL_INSNs are not allowed to have any output reloads;
     neither are insns that SET cc0.  Insns that use CC0 are not allowed
     to have any input reloads.  */
  if (GET_CODE (insn) == JUMP_INSN || GET_CODE (insn) == CALL_INSN)
    no_output_reloads = 1;

#ifdef HAVE_cc0
  if (reg_referenced_p (cc0_rtx, PATTERN (insn)))
    no_input_reloads = 1;
  if (reg_set_p (cc0_rtx, PATTERN (insn)))
    no_output_reloads = 1;
#endif
     
#ifdef SECONDARY_MEMORY_NEEDED
  /* The eliminated forms of any secondary memory locations are per-insn, so
     clear them out here.  */

  bzero ((char *) secondary_memlocs_elim, sizeof secondary_memlocs_elim);
#endif

  /* Find what kind of insn this is.  NOPERANDS gets number of operands.
     Make OPERANDS point to a vector of operand values.
     Make OPERAND_LOCS point to a vector of pointers to
     where the operands were found.
     Fill CONSTRAINTS and CONSTRAINTS1 with pointers to the
     constraint-strings for this insn.
     Return if the insn needs no reload processing.  */

  switch (GET_CODE (body))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case SET:
      /* Dispose quickly of (set (reg..) (reg..)) if both have hard regs and it
	 is cheap to move between them.  If it is not, there may not be an insn
	 to do the copy, so we may need a reload.  */
      if (GET_CODE (SET_DEST (body)) == REG
	  && REGNO (SET_DEST (body)) < FIRST_PSEUDO_REGISTER
	  && GET_CODE (SET_SRC (body)) == REG
	  && REGNO (SET_SRC (body)) < FIRST_PSEUDO_REGISTER
	  && REGISTER_MOVE_COST (REGNO_REG_CLASS (REGNO (SET_SRC (body))),
				 REGNO_REG_CLASS (REGNO (SET_DEST (body)))) == 2)
	return;
    case PARALLEL:
    case ASM_OPERANDS:
      reload_n_operands = noperands = asm_noperands (body);
      if (noperands >= 0)
	{
	  /* This insn is an `asm' with operands.  */

	  insn_code_number = -1;
	  this_insn_is_asm = 1;

	  /* expand_asm_operands makes sure there aren't too many operands.  */
	  if (noperands > MAX_RECOG_OPERANDS)
	    abort ();

	  /* Now get the operand values and constraints out of the insn.  */

	  decode_asm_operands (body, recog_operand, recog_operand_loc,
			       constraints, operand_mode);
	  if (noperands > 0)
	    {
	      bcopy ((char *) constraints, (char *) constraints1,
		     noperands * sizeof (char *));
	      n_alternatives = n_occurrences (',', constraints[0]) + 1;
	      for (i = 1; i < noperands; i++)
		if (n_alternatives != n_occurrences (',', constraints[i]) + 1)
		  {
		    error_for_asm (insn, "operand constraints differ in number of alternatives");
		    /* Avoid further trouble with this insn.  */
		    PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
		    n_reloads = 0;
		    return;
		  }
	    }
	  break;
	}

    default:
      /* Ordinary insn: recognize it, get the operands via insn_extract
	 and get the constraints.  */

      insn_code_number = recog_memoized (insn);
      if (insn_code_number < 0)
	fatal_insn_not_found (insn);

      reload_n_operands = noperands = insn_n_operands[insn_code_number];
      n_alternatives = insn_n_alternatives[insn_code_number];
      /* Just return "no reloads" if insn has no operands with constraints.  */
      if (n_alternatives == 0)
	return;
      insn_extract (insn);
      for (i = 0; i < noperands; i++)
	{
	  constraints[i] = constraints1[i]
	    = insn_operand_constraint[insn_code_number][i];
	  operand_mode[i] = insn_operand_mode[insn_code_number][i];
	}
    }

  if (noperands == 0)
    return;

  commutative = -1;

  /* If we will need to know, later, whether some pair of operands
     are the same, we must compare them now and save the result.
     Reloading the base and index registers will clobber them
     and afterward they will fail to match.  */

  for (i = 0; i < noperands; i++)
    {
      register char *p;
      register int c;

      substed_operand[i] = recog_operand[i];
      p = constraints[i];

      modified[i] = RELOAD_READ;

      /* Scan this operand's constraint to see if it is an output operand, 
	 an in-out operand, is commutative, or should match another.  */

      while (c = *p++)
	{
	  if (c == '=')
	    modified[i] = RELOAD_WRITE;
	  else if (c == '+')
	    modified[i] = RELOAD_READ_WRITE;
	  else if (c == '%')
	    {
	      /* The last operand should not be marked commutative.  */
	      if (i == noperands - 1)
		{
		  if (this_insn_is_asm)
		    warning_for_asm (this_insn,
				     "`%%' constraint used with last operand");
		  else
		    abort ();
		}
	      else
		commutative = i;
	    }
	  else if (c >= '0' && c <= '9')
	    {
	      c -= '0';
	      operands_match[c][i]
		= operands_match_p (recog_operand[c], recog_operand[i]);

	      /* An operand may not match itself.  */
	      if (c == i)
		{
		  if (this_insn_is_asm)
		    warning_for_asm (this_insn,
				     "operand %d has constraint %d", i, c);
		  else
		    abort ();
		}

	      /* If C can be commuted with C+1, and C might need to match I,
		 then C+1 might also need to match I.  */
	      if (commutative >= 0)
		{
		  if (c == commutative || c == commutative + 1)
		    {
		      int other = c + (c == commutative ? 1 : -1);
		      operands_match[other][i]
			= operands_match_p (recog_operand[other], recog_operand[i]);
		    }
		  if (i == commutative || i == commutative + 1)
		    {
		      int other = i + (i == commutative ? 1 : -1);
		      operands_match[c][other]
			= operands_match_p (recog_operand[c], recog_operand[other]);
		    }
		  /* Note that C is supposed to be less than I.
		     No need to consider altering both C and I because in
		     that case we would alter one into the other.  */
		}
	    }
	}
    }

  /* Examine each operand that is a memory reference or memory address
     and reload parts of the addresses into index registers.
     Also here any references to pseudo regs that didn't get hard regs
     but are equivalent to constants get replaced in the insn itself
     with those constants.  Nobody will ever see them again. 

     Finally, set up the preferred classes of each operand.  */

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);

      address_reloaded[i] = 0;
      operand_type[i] = (modified[i] == RELOAD_READ ? RELOAD_FOR_INPUT
			 : modified[i] == RELOAD_WRITE ? RELOAD_FOR_OUTPUT
			 : RELOAD_OTHER);
      address_type[i]
	= (modified[i] == RELOAD_READ ? RELOAD_FOR_INPUT_ADDRESS
	   : modified[i] == RELOAD_WRITE ? RELOAD_FOR_OUTPUT_ADDRESS
	   : RELOAD_OTHER);

      if (*constraints[i] == 0)
	/* Ignore things like match_operator operands.  */
	;
      else if (constraints[i][0] == 'p')
	{
	  find_reloads_address (VOIDmode, NULL_PTR,
				recog_operand[i], recog_operand_loc[i],
				i, operand_type[i], ind_levels);
	  substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	}
      else if (code == MEM)
	{
	  if (find_reloads_address (GET_MODE (recog_operand[i]),
				    recog_operand_loc[i],
				    XEXP (recog_operand[i], 0),
				    &XEXP (recog_operand[i], 0),
				    i, address_type[i], ind_levels))
	    address_reloaded[i] = 1;
	  substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	}
      else if (code == SUBREG)
	substed_operand[i] = recog_operand[i] = *recog_operand_loc[i]
	  = find_reloads_toplev (recog_operand[i], i, address_type[i],
				 ind_levels,
				 set != 0
				 && &SET_DEST (set) == recog_operand_loc[i]);
      else if (code == PLUS)
	/* We can get a PLUS as an "operand" as a result of
	   register elimination.  See eliminate_regs and gen_reload.  */
	substed_operand[i] = recog_operand[i] = *recog_operand_loc[i]
	  = find_reloads_toplev (recog_operand[i], i, address_type[i],
				 ind_levels, 0);
      else if (code == REG)
	{
	  /* This is equivalent to calling find_reloads_toplev.
	     The code is duplicated for speed.
	     When we find a pseudo always equivalent to a constant,
	     we replace it by the constant.  We must be sure, however,
	     that we don't try to replace it in the insn in which it
	     is being set.   */
	  register int regno = REGNO (recog_operand[i]);
	  if (reg_equiv_constant[regno] != 0
	      && (set == 0 || &SET_DEST (set) != recog_operand_loc[i]))
	    substed_operand[i] = recog_operand[i]
	      = reg_equiv_constant[regno];
#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
	  if (reg_equiv_mem[regno] != 0)
	    substed_operand[i] = recog_operand[i]
	      = reg_equiv_mem[regno];
#endif
	  if (reg_equiv_address[regno] != 0)
	    {
	      /* If reg_equiv_address is not a constant address, copy it,
		 since it may be shared.  */
	      rtx address = reg_equiv_address[regno];

	      if (rtx_varies_p (address))
		address = copy_rtx (address);

	      /* If this is an output operand, we must output a CLOBBER
		 after INSN so find_equiv_reg knows REGNO is being written. 
		 Mark this insn specially, do we can put our output reloads
		 after it.  */

	      if (modified[i] != RELOAD_READ)
		PUT_MODE (emit_insn_after (gen_rtx (CLOBBER, VOIDmode,
						    recog_operand[i]),
					   insn),
			  DImode);

	      *recog_operand_loc[i] = recog_operand[i]
		= gen_rtx (MEM, GET_MODE (recog_operand[i]), address);
	      RTX_UNCHANGING_P (recog_operand[i])
		= RTX_UNCHANGING_P (regno_reg_rtx[regno]);
	      find_reloads_address (GET_MODE (recog_operand[i]),
				    recog_operand_loc[i],
				    XEXP (recog_operand[i], 0),
				    &XEXP (recog_operand[i], 0),
				    i, address_type[i], ind_levels);
	      substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	    }
	}
      /* If the operand is still a register (we didn't replace it with an
	 equivalent), get the preferred class to reload it into.  */
      code = GET_CODE (recog_operand[i]);
      preferred_class[i]
	= ((code == REG && REGNO (recog_operand[i]) >= FIRST_PSEUDO_REGISTER)
	   ? reg_preferred_class (REGNO (recog_operand[i])) : NO_REGS);
      pref_or_nothing[i]
	= (code == REG && REGNO (recog_operand[i]) >= FIRST_PSEUDO_REGISTER
	   && reg_alternate_class (REGNO (recog_operand[i])) == NO_REGS);
    }

  /* If this is simply a copy from operand 1 to operand 0, merge the
     preferred classes for the operands.  */
  if (set != 0 && noperands >= 2 && recog_operand[0] == SET_DEST (set)
      && recog_operand[1] == SET_SRC (set))
    {
      preferred_class[0] = preferred_class[1]
	= reg_class_subunion[(int) preferred_class[0]][(int) preferred_class[1]];
      pref_or_nothing[0] |= pref_or_nothing[1];
      pref_or_nothing[1] |= pref_or_nothing[0];
    }

  /* Now see what we need for pseudo-regs that didn't get hard regs
     or got the wrong kind of hard reg.  For this, we must consider
     all the operands together against the register constraints.  */

  best = MAX_RECOG_OPERANDS + 300;

  swapped = 0;
  goal_alternative_swapped = 0;
 try_swapped:

  /* The constraints are made of several alternatives.
     Each operand's constraint looks like foo,bar,... with commas
     separating the alternatives.  The first alternatives for all
     operands go together, the second alternatives go together, etc.

     First loop over alternatives.  */

  for (this_alternative_number = 0;
       this_alternative_number < n_alternatives;
       this_alternative_number++)
    {
      /* Loop over operands for one constraint alternative.  */
      /* LOSERS counts those that don't fit this alternative
	 and would require loading.  */
      int losers = 0;
      /* BAD is set to 1 if it some operand can't fit this alternative
	 even after reloading.  */
      int bad = 0;
      /* REJECT is a count of how undesirable this alternative says it is
	 if any reloading is required.  If the alternative matches exactly
	 then REJECT is ignored, but otherwise it gets this much
	 counted against it in addition to the reloading needed.  Each 
	 ? counts three times here since we want the disparaging caused by
	 a bad register class to only count 1/3 as much.  */
      int reject = 0;

      this_earlyclobber = 0;

      for (i = 0; i < noperands; i++)
	{
	  register char *p = constraints[i];
	  register int win = 0;
	  /* 0 => this operand can be reloaded somehow for this alternative */
	  int badop = 1;
	  /* 0 => this operand can be reloaded if the alternative allows regs.  */
	  int winreg = 0;
	  int c;
	  register rtx operand = recog_operand[i];
	  int offset = 0;
	  /* Nonzero means this is a MEM that must be reloaded into a reg
	     regardless of what the constraint says.  */
	  int force_reload = 0;
	  int offmemok = 0;
	  /* Nonzero if a constant forced into memory would be OK for this
	     operand.  */
	  int constmemok = 0;
	  int earlyclobber = 0;

	  /* If the operand is a SUBREG, extract
	     the REG or MEM (or maybe even a constant) within.
	     (Constants can occur as a result of reg_equiv_constant.)  */

	  while (GET_CODE (operand) == SUBREG)
	    {
	      offset += SUBREG_WORD (operand);
	      operand = SUBREG_REG (operand);
	      /* Force reload if this is a constant or PLUS or if there may may
		 be a problem accessing OPERAND in the outer mode.  */
	      if (CONSTANT_P (operand)
		  || GET_CODE (operand) == PLUS
		  /* We must force a reload of paradoxical SUBREGs
		     of a MEM because the alignment of the inner value
		     may not be enough to do the outer reference.  On
		     big-endian machines, it may also reference outside
		     the object.

		     On machines that extend byte operations and we have a
		     SUBREG where both the inner and outer modes are no wider
		     than a word and the inner mode is narrower, is integral,
		     and gets extended when loaded from memory, combine.c has
		     made assumptions about the behavior of the machine in such
		     register access.  If the data is, in fact, in memory we
		     must always load using the size assumed to be in the
		     register and let the insn do the different-sized 
		     accesses.  */
		  || ((GET_CODE (operand) == MEM
		       || (GET_CODE (operand)== REG
			   && REGNO (operand) >= FIRST_PSEUDO_REGISTER))
		      && (((GET_MODE_BITSIZE (GET_MODE (operand))
			    < BIGGEST_ALIGNMENT)
			   && (GET_MODE_SIZE (operand_mode[i])
			       > GET_MODE_SIZE (GET_MODE (operand))))
			  || (GET_CODE (operand) == MEM && BYTES_BIG_ENDIAN)
#ifdef LOAD_EXTEND_OP
			  || (GET_MODE_SIZE (operand_mode[i]) <= UNITS_PER_WORD
			      && (GET_MODE_SIZE (GET_MODE (operand))
				  <= UNITS_PER_WORD)
			      && (GET_MODE_SIZE (operand_mode[i])
				  > GET_MODE_SIZE (GET_MODE (operand)))
			      && INTEGRAL_MODE_P (GET_MODE (operand))
			      && LOAD_EXTEND_OP (GET_MODE (operand)) != NIL)
#endif
			  ))
		  /* Subreg of a hard reg which can't handle the subreg's mode
		     or which would handle that mode in the wrong number of
		     registers for subregging to work.  */
		  || (GET_CODE (operand) == REG
		      && REGNO (operand) < FIRST_PSEUDO_REGISTER
		      && ((GET_MODE_SIZE (operand_mode[i]) <= UNITS_PER_WORD
			   && (GET_MODE_SIZE (GET_MODE (operand))
			       > UNITS_PER_WORD)
			   && ((GET_MODE_SIZE (GET_MODE (operand))
				/ UNITS_PER_WORD)
			       != HARD_REGNO_NREGS (REGNO (operand),
						    GET_MODE (operand))))
			  || ! HARD_REGNO_MODE_OK (REGNO (operand) + offset,
						   operand_mode[i]))))
		force_reload = 1;
	    }

	  this_alternative[i] = (int) NO_REGS;
	  this_alternative_win[i] = 0;
	  this_alternative_offmemok[i] = 0;
	  this_alternative_earlyclobber[i] = 0;
	  this_alternative_matches[i] = -1;

	  /* An empty constraint or empty alternative
	     allows anything which matched the pattern.  */
	  if (*p == 0 || *p == ',')
	    win = 1, badop = 0;

	  /* Scan this alternative's specs for this operand;
	     set WIN if the operand fits any letter in this alternative.
	     Otherwise, clear BADOP if this operand could
	     fit some letter after reloads,
	     or set WINREG if this operand could fit after reloads
	     provided the constraint allows some registers.  */

	  while (*p && (c = *p++) != ',')
	    switch (c)
	      {
	      case '=':
	      case '+':
	      case '*':
		break;

	      case '%':
		/* The last operand should not be marked commutative.  */
		if (i != noperands - 1)
		  commutative = i;
		break;

	      case '?':
		reject += 3;
		break;

	      case '!':
		reject = 300;
		break;

	      case '#':
		/* Ignore rest of this alternative as far as
		   reloading is concerned.  */
		while (*p && *p != ',') p++;
		break;

	      case '0':
	      case '1':
	      case '2':
	      case '3':
	      case '4':
		c -= '0';
		this_alternative_matches[i] = c;
		/* We are supposed to match a previous operand.
		   If we do, we win if that one did.
		   If we do not, count both of the operands as losers.
		   (This is too conservative, since most of the time
		   only a single reload insn will be needed to make
		   the two operands win.  As a result, this alternative
		   may be rejected when it is actually desirable.)  */
		if ((swapped && (c != commutative || i != commutative + 1))
		    /* If we are matching as if two operands were swapped,
		       also pretend that operands_match had been computed
		       with swapped.
		       But if I is the second of those and C is the first,
		       don't exchange them, because operands_match is valid
		       only on one side of its diagonal.  */
		    ? (operands_match
		        [(c == commutative || c == commutative + 1)
			 ? 2*commutative + 1 - c : c]
		        [(i == commutative || i == commutative + 1)
			 ? 2*commutative + 1 - i : i])
		    : operands_match[c][i])
		  win = this_alternative_win[c];
		else
		  {
		    /* Operands don't match.  */
		    rtx value;
		    /* Retroactively mark the operand we had to match
		       as a loser, if it wasn't already.  */
		    if (this_alternative_win[c])
		      losers++;
		    this_alternative_win[c] = 0;
		    if (this_alternative[c] == (int) NO_REGS)
		      bad = 1;
		    /* But count the pair only once in the total badness of
		       this alternative, if the pair can be a dummy reload.  */
		    value
		      = find_dummy_reload (recog_operand[i], recog_operand[c],
					   recog_operand_loc[i], recog_operand_loc[c],
					   operand_mode[i], operand_mode[c],
					   this_alternative[c], -1);

		    if (value != 0)
		      losers--;
		  }
		/* This can be fixed with reloads if the operand
		   we are supposed to match can be fixed with reloads.  */
		badop = 0;
		this_alternative[i] = this_alternative[c];

		/* If we have to reload this operand and some previous
		   operand also had to match the same thing as this
		   operand, we don't know how to do that.  So reject this
		   alternative.  */
		if (! win || force_reload)
		  for (j = 0; j < i; j++)
		    if (this_alternative_matches[j]
			== this_alternative_matches[i])
		      badop = 1;

		break;

	      case 'p':
		/* All necessary reloads for an address_operand
		   were handled in find_reloads_address.  */
		this_alternative[i] = (int) BASE_REG_CLASS;
		win = 1;
		break;

	      case 'm':
		if (force_reload)
		  break;
		if (GET_CODE (operand) == MEM
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		if (CONSTANT_P (operand))
		  badop = 0;
		constmemok = 1;
		break;

	      case '<':
		if (GET_CODE (operand) == MEM
		    && ! address_reloaded[i]
		    && (GET_CODE (XEXP (operand, 0)) == PRE_DEC
			|| GET_CODE (XEXP (operand, 0)) == POST_DEC))
		  win = 1;
		break;

	      case '>':
		if (GET_CODE (operand) == MEM
		    && ! address_reloaded[i]
		    && (GET_CODE (XEXP (operand, 0)) == PRE_INC
			|| GET_CODE (XEXP (operand, 0)) == POST_INC))
		  win = 1;
		break;

		/* Memory operand whose address is not offsettable.  */
	      case 'V':
		if (force_reload)
		  break;
		if (GET_CODE (operand) == MEM
		    && ! (ind_levels ? offsettable_memref_p (operand)
			  : offsettable_nonstrict_memref_p (operand))
		    /* Certain mem addresses will become offsettable
		       after they themselves are reloaded.  This is important;
		       we don't want our own handling of unoffsettables
		       to override the handling of reg_equiv_address.  */
		    && !(GET_CODE (XEXP (operand, 0)) == REG
			 && (ind_levels == 0
			     || reg_equiv_address[REGNO (XEXP (operand, 0))] != 0)))
		  win = 1;
		break;

		/* Memory operand whose address is offsettable.  */
	      case 'o':
		if (force_reload)
		  break;
		if ((GET_CODE (operand) == MEM
		     /* If IND_LEVELS, find_reloads_address won't reload a
			pseudo that didn't get a hard reg, so we have to
			reject that case.  */
		     && (ind_levels ? offsettable_memref_p (operand)
			 : offsettable_nonstrict_memref_p (operand)))
		    /* Certain mem addresses will become offsettable
		       after they themselves are reloaded.  This is important;
		       we don't want our own handling of unoffsettables
		       to override the handling of reg_equiv_address.  */
		    || (GET_CODE (operand) == MEM
			&& GET_CODE (XEXP (operand, 0)) == REG
			&& (ind_levels == 0
			    || reg_equiv_address[REGNO (XEXP (operand, 0))] != 0))
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0
			/* If reg_equiv_address is nonzero, we will be
			   loading it into a register; hence it will be
			   offsettable, but we cannot say that reg_equiv_mem
			   is offsettable without checking.  */
			&& ((reg_equiv_mem[REGNO (operand)] != 0
			     && offsettable_memref_p (reg_equiv_mem[REGNO (operand)]))
			    || (reg_equiv_address[REGNO (operand)] != 0))))
		  win = 1;
		if (CONSTANT_P (operand) || GET_CODE (operand) == MEM)
		  badop = 0;
		constmemok = 1;
		offmemok = 1;
		break;

	      case '&':
		/* Output operand that is stored before the need for the
		   input operands (and their index registers) is over.  */
		earlyclobber = 1, this_earlyclobber = 1;
		break;

	      case 'E':
		/* Match any floating double constant, but only if
		   we can examine the bits of it reliably.  */
		if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
		     || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
		    && GET_MODE (operand) != VOIDmode && ! flag_pretend_float)
		  break;
		if (GET_CODE (operand) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'F':
		if (GET_CODE (operand) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'G':
	      case 'H':
		if (GET_CODE (operand) == CONST_DOUBLE
		    && CONST_DOUBLE_OK_FOR_LETTER_P (operand, c))
		  win = 1;
		break;

	      case 's':
		if (GET_CODE (operand) == CONST_INT
		    || (GET_CODE (operand) == CONST_DOUBLE
			&& GET_MODE (operand) == VOIDmode))
		  break;
	      case 'i':
		if (CONSTANT_P (operand)
#ifdef LEGITIMATE_PIC_OPERAND_P
		    && (! flag_pic || LEGITIMATE_PIC_OPERAND_P (operand))
#endif
		    )
		  win = 1;
		break;

	      case 'n':
		if (GET_CODE (operand) == CONST_INT
		    || (GET_CODE (operand) == CONST_DOUBLE
			&& GET_MODE (operand) == VOIDmode))
		  win = 1;
		break;

	      case 'I':
	      case 'J':
	      case 'K':
	      case 'L':
	      case 'M':
	      case 'N':
	      case 'O':
	      case 'P':
		if (GET_CODE (operand) == CONST_INT
		    && CONST_OK_FOR_LETTER_P (INTVAL (operand), c))
		  win = 1;
		break;

	      case 'X':
		win = 1;
		break;

	      case 'g':
		if (! force_reload
		    /* A PLUS is never a valid operand, but reload can make
		       it from a register when eliminating registers.  */
		    && GET_CODE (operand) != PLUS
		    /* A SCRATCH is not a valid operand.  */
		    && GET_CODE (operand) != SCRATCH
#ifdef LEGITIMATE_PIC_OPERAND_P
		    && (! CONSTANT_P (operand) 
			|| ! flag_pic 
			|| LEGITIMATE_PIC_OPERAND_P (operand))
#endif
		    && (GENERAL_REGS == ALL_REGS
			|| GET_CODE (operand) != REG
			|| (REGNO (operand) >= FIRST_PSEUDO_REGISTER
			    && reg_renumber[REGNO (operand)] < 0)))
		  win = 1;
		/* Drop through into 'r' case */

	      case 'r':
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) GENERAL_REGS];
		goto reg;

#ifdef EXTRA_CONSTRAINT
              case 'Q':
              case 'R':
              case 'S':
              case 'T':
              case 'U':
		if (EXTRA_CONSTRAINT (operand, c))
		  win = 1;
		break;
#endif
  
	      default:
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) REG_CLASS_FROM_LETTER (c)];
		
	      reg:
		if (GET_MODE (operand) == BLKmode)
		  break;
		winreg = 1;
		if (GET_CODE (operand) == REG
		    && reg_fits_class_p (operand, this_alternative[i],
					 offset, GET_MODE (recog_operand[i])))
		  win = 1;
		break;
	      }

	  constraints[i] = p;

	  /* If this operand could be handled with a reg,
	     and some reg is allowed, then this operand can be handled.  */
	  if (winreg && this_alternative[i] != (int) NO_REGS)
	    badop = 0;

	  /* Record which operands fit this alternative.  */
	  this_alternative_earlyclobber[i] = earlyclobber;
	  if (win && ! force_reload)
	    this_alternative_win[i] = 1;
	  else
	    {
	      int const_to_mem = 0;

	      this_alternative_offmemok[i] = offmemok;
	      losers++;
	      if (badop)
		bad = 1;
	      /* Alternative loses if it has no regs for a reg operand.  */
	      if (GET_CODE (operand) == REG
		  && this_alternative[i] == (int) NO_REGS
		  && this_alternative_matches[i] < 0)
		bad = 1;

	      /* Alternative loses if it requires a type of reload not
		 permitted for this insn.  We can always reload SCRATCH
		 and objects with a REG_UNUSED note.  */
	      if (GET_CODE (operand) != SCRATCH
		  && modified[i] != RELOAD_READ && no_output_reloads
		  && ! find_reg_note (insn, REG_UNUSED, operand))
		bad = 1;
	      else if (modified[i] != RELOAD_WRITE && no_input_reloads)
		bad = 1;

	      /* If this is a constant that is reloaded into the desired
		 class by copying it to memory first, count that as another
		 reload.  This is consistent with other code and is
		 required to avoid chosing another alternative when
		 the constant is moved into memory by this function on
		 an early reload pass.  Note that the test here is 
		 precisely the same as in the code below that calls
		 force_const_mem.  */
	      if (CONSTANT_P (operand)
		  /* force_const_mem does not accept HIGH.  */
		  && GET_CODE (operand) != HIGH
		  && (PREFERRED_RELOAD_CLASS (operand,
					      (enum reg_class) this_alternative[i])
		      == NO_REGS)
		  && operand_mode[i] != VOIDmode)
		{
		  const_to_mem = 1;
		  if (this_alternative[i] != (int) NO_REGS)
		    losers++;
		}

	      /* If we can't reload this value at all, reject this
		 alternative.  Note that we could also lose due to
		 LIMIT_RELOAD_RELOAD_CLASS, but we don't check that
		 here.  */

	      if (! CONSTANT_P (operand)
		  && (enum reg_class) this_alternative[i] != NO_REGS
		  && (PREFERRED_RELOAD_CLASS (operand,
					      (enum reg_class) this_alternative[i])
		      == NO_REGS))
		bad = 1;

	      /* We prefer to reload pseudos over reloading other things,
		 since such reloads may be able to be eliminated later.
		 If we are reloading a SCRATCH, we won't be generating any
		 insns, just using a register, so it is also preferred. 
		 So bump REJECT in other cases.  Don't do this in the
		 case where we are forcing a constant into memory and
		 it will then win since we don't want to have a different
		 alternative match then.  */
	      if (! (GET_CODE (operand) == REG
		     && REGNO (operand) >= FIRST_PSEUDO_REGISTER)
		  && GET_CODE (operand) != SCRATCH
		  && ! (const_to_mem && constmemok))
		reject++;
	    }

	  /* If this operand is a pseudo register that didn't get a hard 
	     reg and this alternative accepts some register, see if the
	     class that we want is a subset of the preferred class for this
	     register.  If not, but it intersects that class, use the
	     preferred class instead.  If it does not intersect the preferred
	     class, show that usage of this alternative should be discouraged;
	     it will be discouraged more still if the register is `preferred
	     or nothing'.  We do this because it increases the chance of
	     reusing our spill register in a later insn and avoiding a pair
	     of memory stores and loads.

	     Don't bother with this if this alternative will accept this
	     operand.

	     Don't do this for a multiword operand, since it is only a
	     small win and has the risk of requiring more spill registers,
	     which could cause a large loss.

	     Don't do this if the preferred class has only one register
	     because we might otherwise exhaust the class.  */


	  if (! win && this_alternative[i] != (int) NO_REGS
	      && GET_MODE_SIZE (operand_mode[i]) <= UNITS_PER_WORD
	      && reg_class_size[(int) preferred_class[i]] > 1)
	    {
	      if (! reg_class_subset_p (this_alternative[i],
					preferred_class[i]))
		{
		  /* Since we don't have a way of forming the intersection,
		     we just do something special if the preferred class
		     is a subset of the class we have; that's the most 
		     common case anyway.  */
		  if (reg_class_subset_p (preferred_class[i],
					  this_alternative[i]))
		    this_alternative[i] = (int) preferred_class[i];
		  else
		    reject += (1 + pref_or_nothing[i]);
		}
	    }
	}

      /* Now see if any output operands that are marked "earlyclobber"
	 in this alternative conflict with any input operands
	 or any memory addresses.  */

      for (i = 0; i < noperands; i++)
	if (this_alternative_earlyclobber[i]
	    && this_alternative_win[i])
	  {
	    struct decomposition early_data; 

	    early_data = decompose (recog_operand[i]);

	    if (modified[i] == RELOAD_READ)
	      {
		if (this_insn_is_asm)
		  warning_for_asm (this_insn,
				   "`&' constraint used with input operand");
		else
		  abort ();
		continue;
	      }
	    
	    if (this_alternative[i] == NO_REGS)
	      {
		this_alternative_earlyclobber[i] = 0;
		if (this_insn_is_asm)
		  error_for_asm (this_insn,
				 "`&' constraint used with no register class");
		else
		  abort ();
	      }

	    for (j = 0; j < noperands; j++)
	      /* Is this an input operand or a memory ref?  */
	      if ((GET_CODE (recog_operand[j]) == MEM
		   || modified[j] != RELOAD_WRITE)
		  && j != i
		  /* Ignore things like match_operator operands.  */
		  && *constraints1[j] != 0
		  /* Don't count an input operand that is constrained to match
		     the early clobber operand.  */
		  && ! (this_alternative_matches[j] == i
			&& rtx_equal_p (recog_operand[i], recog_operand[j]))
		  /* Is it altered by storing the earlyclobber operand?  */
		  && !immune_p (recog_operand[j], recog_operand[i], early_data))
		{
		  /* If the output is in a single-reg class,
		     it's costly to reload it, so reload the input instead.  */
		  if (reg_class_size[this_alternative[i]] == 1
		      && (GET_CODE (recog_operand[j]) == REG
			  || GET_CODE (recog_operand[j]) == SUBREG))
		    {
		      losers++;
		      this_alternative_win[j] = 0;
		    }
		  else
		    break;
		}
	    /* If an earlyclobber operand conflicts with something,
	       it must be reloaded, so request this and count the cost.  */
	    if (j != noperands)
	      {
		losers++;
		this_alternative_win[i] = 0;
		for (j = 0; j < noperands; j++)
		  if (this_alternative_matches[j] == i
		      && this_alternative_win[j])
		    {
		      this_alternative_win[j] = 0;
		      losers++;
		    }
	      }
	  }

      /* If one alternative accepts all the operands, no reload required,
	 choose that alternative; don't consider the remaining ones.  */
      if (losers == 0)
	{
	  /* Unswap these so that they are never swapped at `finish'.  */
	  if (commutative >= 0)
	    {
	      recog_operand[commutative] = substed_operand[commutative];
	      recog_operand[commutative + 1]
		= substed_operand[commutative + 1];
	    }
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative_win[i] = 1;
	      goal_alternative[i] = this_alternative[i];
	      goal_alternative_offmemok[i] = this_alternative_offmemok[i];
	      goal_alternative_matches[i] = this_alternative_matches[i];
	      goal_alternative_earlyclobber[i]
		= this_alternative_earlyclobber[i];
	    }
	  goal_alternative_number = this_alternative_number;
	  goal_alternative_swapped = swapped;
	  goal_earlyclobber = this_earlyclobber;
	  goto finish;
	}

      /* REJECT, set by the ! and ? constraint characters and when a register
	 would be reloaded into a non-preferred class, discourages the use of
	 this alternative for a reload goal.  REJECT is incremented by three
	 for each ? and one for each non-preferred class.  */
      losers = losers * 3 + reject;

      /* If this alternative can be made to work by reloading,
	 and it needs less reloading than the others checked so far,
	 record it as the chosen goal for reloading.  */
      if (! bad && best > losers)
	{
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative[i] = this_alternative[i];
	      goal_alternative_win[i] = this_alternative_win[i];
	      goal_alternative_offmemok[i] = this_alternative_offmemok[i];
	      goal_alternative_matches[i] = this_alternative_matches[i];
	      goal_alternative_earlyclobber[i]
		= this_alternative_earlyclobber[i];
	    }
	  goal_alternative_swapped = swapped;
	  best = losers;
	  goal_alternative_number = this_alternative_number;
	  goal_earlyclobber = this_earlyclobber;
	}
    }

  /* If insn is commutative (it's safe to exchange a certain pair of operands)
     then we need to try each alternative twice,
     the second time matching those two operands
     as if we had exchanged them.
     To do this, really exchange them in operands.

     If we have just tried the alternatives the second time,
     return operands to normal and drop through.  */

  if (commutative >= 0)
    {
      swapped = !swapped;
      if (swapped)
	{
	  register enum reg_class tclass;
	  register int t;

	  recog_operand[commutative] = substed_operand[commutative + 1];
	  recog_operand[commutative + 1] = substed_operand[commutative];

	  tclass = preferred_class[commutative];
	  preferred_class[commutative] = preferred_class[commutative + 1];
	  preferred_class[commutative + 1] = tclass;

	  t = pref_or_nothing[commutative];
	  pref_or_nothing[commutative] = pref_or_nothing[commutative + 1];
	  pref_or_nothing[commutative + 1] = t;

	  bcopy ((char *) constraints1, (char *) constraints,
		 noperands * sizeof (char *));
	  goto try_swapped;
	}
      else
	{
	  recog_operand[commutative] = substed_operand[commutative];
	  recog_operand[commutative + 1] = substed_operand[commutative + 1];
	}
    }

  /* The operands don't meet the constraints.
     goal_alternative describes the alternative
     that we could reach by reloading the fewest operands.
     Reload so as to fit it.  */

  if (best == MAX_RECOG_OPERANDS + 300)
    {
      /* No alternative works with reloads??  */
      if (insn_code_number >= 0)
	abort ();
      error_for_asm (insn, "inconsistent operand constraints in an `asm'");
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
      n_reloads = 0;
      return;
    }

  /* Jump to `finish' from above if all operands are valid already.
     In that case, goal_alternative_win is all 1.  */
 finish:

  /* Right now, for any pair of operands I and J that are required to match,
     with I < J,
     goal_alternative_matches[J] is I.
     Set up goal_alternative_matched as the inverse function:
     goal_alternative_matched[I] = J.  */

  for (i = 0; i < noperands; i++)
    goal_alternative_matched[i] = -1;

  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i]
	&& goal_alternative_matches[i] >= 0)
      goal_alternative_matched[goal_alternative_matches[i]] = i;

  /* If the best alternative is with operands 1 and 2 swapped,
     consider them swapped before reporting the reloads.  Update the
     operand numbers of any reloads already pushed.  */

  if (goal_alternative_swapped)
    {
      register rtx tem;

      tem = substed_operand[commutative];
      substed_operand[commutative] = substed_operand[commutative + 1];
      substed_operand[commutative + 1] = tem;
      tem = recog_operand[commutative];
      recog_operand[commutative] = recog_operand[commutative + 1];
      recog_operand[commutative + 1] = tem;

      for (i = 0; i < n_reloads; i++)
	{
	  if (reload_opnum[i] == commutative)
	    reload_opnum[i] = commutative + 1;
	  else if (reload_opnum[i] == commutative + 1)
	    reload_opnum[i] = commutative;
	}
    }

  /* Perform whatever substitutions on the operands we are supposed
     to make due to commutativity or replacement of registers
     with equivalent constants or memory slots.  */

  for (i = 0; i < noperands; i++)
    {
      *recog_operand_loc[i] = substed_operand[i];
      /* While we are looping on operands, initialize this.  */
      operand_reloadnum[i] = -1;

      /* If this is an earlyclobber operand, we need to widen the scope.
	 The reload must remain valid from the start of the insn being
	 reloaded until after the operand is stored into its destination.
	 We approximate this with RELOAD_OTHER even though we know that we
	 do not conflict with RELOAD_FOR_INPUT_ADDRESS reloads.

	 One special case that is worth checking is when we have an
	 output that is earlyclobber but isn't used past the insn (typically
	 a SCRATCH).  In this case, we only need have the reload live 
	 through the insn itself, but not for any of our input or output
	 reloads. 

	 In any case, anything needed to address this operand can remain
	 however they were previously categorized.  */

      if (goal_alternative_earlyclobber[i])
	operand_type[i]
	  = (find_reg_note (insn, REG_UNUSED, recog_operand[i])
	     ? RELOAD_FOR_INSN : RELOAD_OTHER);
    }

  /* Any constants that aren't allowed and can't be reloaded
     into registers are here changed into memory references.  */
  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i]
	&& CONSTANT_P (recog_operand[i])
	/* force_const_mem does not accept HIGH.  */
	&& GET_CODE (recog_operand[i]) != HIGH
	&& (PREFERRED_RELOAD_CLASS (recog_operand[i],
				    (enum reg_class) goal_alternative[i])
	    == NO_REGS)
	&& operand_mode[i] != VOIDmode)
      {
	*recog_operand_loc[i] = recog_operand[i]
	  = find_reloads_toplev (force_const_mem (operand_mode[i],
						  recog_operand[i]),
				 i, address_type[i], ind_levels, 0);
	if (alternative_allows_memconst (constraints1[i],
					 goal_alternative_number))
	  goal_alternative_win[i] = 1;
      }

  /* Record the values of the earlyclobber operands for the caller.  */
  if (goal_earlyclobber)
    for (i = 0; i < noperands; i++)
      if (goal_alternative_earlyclobber[i])
	reload_earlyclobbers[n_earlyclobbers++] = recog_operand[i];

  /* Now record reloads for all the operands that need them.  */
  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i])
      {
	/* Operands that match previous ones have already been handled.  */
	if (goal_alternative_matches[i] >= 0)
	  ;
	/* Handle an operand with a nonoffsettable address
	   appearing where an offsettable address will do
	   by reloading the address into a base register.

	   ??? We can also do this when the operand is a register and
	   reg_equiv_mem is not offsettable, but this is a bit tricky,
	   so we don't bother with it.  It may not be worth doing.  */
	else if (goal_alternative_matched[i] == -1
		 && goal_alternative_offmemok[i]
		 && GET_CODE (recog_operand[i]) == MEM)
	  {
	    operand_reloadnum[i]
	      = push_reload (XEXP (recog_operand[i], 0), NULL_RTX,
			     &XEXP (recog_operand[i], 0), NULL_PTR,
			     BASE_REG_CLASS, GET_MODE (XEXP (recog_operand[i], 0)),
			     VOIDmode, 0, 0, i, RELOAD_FOR_INPUT);
	    reload_inc[operand_reloadnum[i]]
	      = GET_MODE_SIZE (GET_MODE (recog_operand[i]));

	    /* If this operand is an output, we will have made any
	       reloads for its address as RELOAD_FOR_OUTPUT_ADDRESS, but
	       now we are treating part of the operand as an input, so
	       we must change these to RELOAD_FOR_INPUT_ADDRESS.  */

	    if (modified[i] == RELOAD_WRITE)
	      for (j = 0; j < n_reloads; j++)
		if (reload_opnum[j] == i
		    && reload_when_needed[j] == RELOAD_FOR_OUTPUT_ADDRESS)
		  reload_when_needed[j] = RELOAD_FOR_INPUT_ADDRESS;
	  }
	else if (goal_alternative_matched[i] == -1)
	  operand_reloadnum[i] =
	    push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			 modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			 (modified[i] != RELOAD_WRITE ?
			  recog_operand_loc[i] : 0),
			 modified[i] != RELOAD_READ ? recog_operand_loc[i] : 0,
			 (enum reg_class) goal_alternative[i],
			 (modified[i] == RELOAD_WRITE
			  ? VOIDmode : operand_mode[i]),
			 (modified[i] == RELOAD_READ
			  ? VOIDmode : operand_mode[i]),
			 (insn_code_number < 0 ? 0
			  : insn_operand_strict_low[insn_code_number][i]),
			 0, i, operand_type[i]);
	/* In a matching pair of operands, one must be input only
	   and the other must be output only.
	   Pass the input operand as IN and the other as OUT.  */
	else if (modified[i] == RELOAD_READ
		 && modified[goal_alternative_matched[i]] == RELOAD_WRITE)
	  {
	    operand_reloadnum[i]
	      = push_reload (recog_operand[i],
			     recog_operand[goal_alternative_matched[i]],
			     recog_operand_loc[i],
			     recog_operand_loc[goal_alternative_matched[i]],
			     (enum reg_class) goal_alternative[i],
			     operand_mode[i],
			     operand_mode[goal_alternative_matched[i]],
			     0, 0, i, RELOAD_OTHER);
	    operand_reloadnum[goal_alternative_matched[i]] = output_reloadnum;
	  }
	else if (modified[i] == RELOAD_WRITE
		 && modified[goal_alternative_matched[i]] == RELOAD_READ)
	  {
	    operand_reloadnum[goal_alternative_matched[i]]
	      = push_reload (recog_operand[goal_alternative_matched[i]],
			     recog_operand[i],
			     recog_operand_loc[goal_alternative_matched[i]],
			     recog_operand_loc[i],
			     (enum reg_class) goal_alternative[i],
			     operand_mode[goal_alternative_matched[i]],
			     operand_mode[i],
			     0, 0, i, RELOAD_OTHER);
	    operand_reloadnum[i] = output_reloadnum;
	  }
	else if (insn_code_number >= 0)
	  abort ();
	else
	  {
	    error_for_asm (insn, "inconsistent operand constraints in an `asm'");
	    /* Avoid further trouble with this insn.  */
	    PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
	    n_reloads = 0;
	    return;
	  }
      }
    else if (goal_alternative_matched[i] < 0
	     && goal_alternative_matches[i] < 0
	     && optimize)
      {
	/* For each non-matching operand that's a MEM or a pseudo-register 
	   that didn't get a hard register, make an optional reload.
	   This may get done even if the insn needs no reloads otherwise.  */

	rtx operand = recog_operand[i];

	while (GET_CODE (operand) == SUBREG)
	  operand = XEXP (operand, 0);
	if ((GET_CODE (operand) == MEM
	     || (GET_CODE (operand) == REG
		 && REGNO (operand) >= FIRST_PSEUDO_REGISTER))
	    && (enum reg_class) goal_alternative[i] != NO_REGS
	    && ! no_input_reloads
	    /* Optional output reloads don't do anything and we mustn't
	       make in-out reloads on insns that are not permitted output
	       reloads.  */
	    && (modified[i] == RELOAD_READ
		|| (modified[i] == RELOAD_READ_WRITE && ! no_output_reloads)))
	  operand_reloadnum[i]
	    = push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			   modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			   (modified[i] != RELOAD_WRITE
			    ? recog_operand_loc[i] : 0),
			   (modified[i] != RELOAD_READ
			    ? recog_operand_loc[i] : 0),
			   (enum reg_class) goal_alternative[i],
			   (modified[i] == RELOAD_WRITE
			    ? VOIDmode : operand_mode[i]),
			   (modified[i] == RELOAD_READ
			    ? VOIDmode : operand_mode[i]),
			   (insn_code_number < 0 ? 0
			    : insn_operand_strict_low[insn_code_number][i]),
			   1, i, operand_type[i]);
      }
    else if (goal_alternative_matches[i] >= 0
	     && goal_alternative_win[goal_alternative_matches[i]]
	     && modified[i] == RELOAD_READ
	     && modified[goal_alternative_matches[i]] == RELOAD_WRITE
	     && ! no_input_reloads && ! no_output_reloads
	     && optimize)
      {
	/* Similarly, make an optional reload for a pair of matching
	   objects that are in MEM or a pseudo that didn't get a hard reg.  */

	rtx operand = recog_operand[i];

	while (GET_CODE (operand) == SUBREG)
	  operand = XEXP (operand, 0);
	if ((GET_CODE (operand) == MEM
	     || (GET_CODE (operand) == REG
		 && REGNO (operand) >= FIRST_PSEUDO_REGISTER))
	    && ((enum reg_class) goal_alternative[goal_alternative_matches[i]]
		!= NO_REGS))
	  operand_reloadnum[i] = operand_reloadnum[goal_alternative_matches[i]]
	    = push_reload (recog_operand[goal_alternative_matches[i]],
			   recog_operand[i],
			   recog_operand_loc[goal_alternative_matches[i]],
			   recog_operand_loc[i],
			   (enum reg_class) goal_alternative[goal_alternative_matches[i]],
			   operand_mode[goal_alternative_matches[i]],
			   operand_mode[i],
			   0, 1, goal_alternative_matches[i], RELOAD_OTHER);
      }
  
  /* If this insn pattern contains any MATCH_DUP's, make sure that
     they will be substituted if the operands they match are substituted.
     Also do now any substitutions we already did on the operands.

     Don't do this if we aren't making replacements because we might be
     propagating things allocated by frame pointer elimination into places
     it doesn't expect.  */

  if (insn_code_number >= 0 && replace)
    for (i = insn_n_dups[insn_code_number] - 1; i >= 0; i--)
      {
	int opno = recog_dup_num[i];
	*recog_dup_loc[i] = *recog_operand_loc[opno];
	if (operand_reloadnum[opno] >= 0)
	  push_replacement (recog_dup_loc[i], operand_reloadnum[opno],
			    insn_operand_mode[insn_code_number][opno]);
      }

#if 0
  /* This loses because reloading of prior insns can invalidate the equivalence
     (or at least find_equiv_reg isn't smart enough to find it any more),
     causing this insn to need more reload regs than it needed before.
     It may be too late to make the reload regs available.
     Now this optimization is done safely in choose_reload_regs.  */

  /* For each reload of a reg into some other class of reg,
     search for an existing equivalent reg (same value now) in the right class.
     We can use it as long as we don't need to change its contents.  */
  for (i = 0; i < n_reloads; i++)
    if (reload_reg_rtx[i] == 0
	&& reload_in[i] != 0
	&& GET_CODE (reload_in[i]) == REG
	&& reload_out[i] == 0)
      {
	reload_reg_rtx[i]
	  = find_equiv_reg (reload_in[i], insn, reload_reg_class[i], -1,
			    static_reload_reg_p, 0, reload_inmode[i]);
	/* Prevent generation of insn to load the value
	   because the one we found already has the value.  */
	if (reload_reg_rtx[i])
	  reload_in[i] = reload_reg_rtx[i];
      }
#endif

  /* Perhaps an output reload can be combined with another
     to reduce needs by one.  */
  if (!goal_earlyclobber)
    combine_reloads ();

  /* If we have a pair of reloads for parts of an address, they are reloading
     the same object, the operands themselves were not reloaded, and they
     are for two operands that are supposed to match, merge the reloads and
     change the type of the surviving reload to RELOAD_FOR_OPERAND_ADDRESS. */

  for (i = 0; i < n_reloads; i++)
    {
      int k;

      for (j = i + 1; j < n_reloads; j++)
	if ((reload_when_needed[i] == RELOAD_FOR_INPUT_ADDRESS
	     || reload_when_needed[i] == RELOAD_FOR_OUTPUT_ADDRESS)
	    && (reload_when_needed[j] == RELOAD_FOR_INPUT_ADDRESS
		|| reload_when_needed[j] == RELOAD_FOR_OUTPUT_ADDRESS)
	    && rtx_equal_p (reload_in[i], reload_in[j])
	    && (operand_reloadnum[reload_opnum[i]] < 0
		|| reload_optional[operand_reloadnum[reload_opnum[i]]])
	    && (operand_reloadnum[reload_opnum[j]] < 0
		|| reload_optional[operand_reloadnum[reload_opnum[j]]])
	    && (goal_alternative_matches[reload_opnum[i]] == reload_opnum[j]
		|| (goal_alternative_matches[reload_opnum[j]]
		    == reload_opnum[i])))
	  {
	    for (k = 0; k < n_replacements; k++)
	      if (replacements[k].what == j)
		replacements[k].what = i;

	    reload_when_needed[i] = RELOAD_FOR_OPERAND_ADDRESS;
	    reload_in[j] = 0;
	  }
    }

  /* Scan all the reloads and update their type. 
     If a reload is for the address of an operand and we didn't reload
     that operand, change the type.  Similarly, change the operand number
     of a reload when two operands match.  If a reload is optional, treat it
     as though the operand isn't reloaded.

     ??? This latter case is somewhat odd because if we do the optional
     reload, it means the object is hanging around.  Thus we need only
     do the address reload if the optional reload was NOT done.

     Change secondary reloads to be the address type of their operand, not
     the normal type.

     If an operand's reload is now RELOAD_OTHER, change any
     RELOAD_FOR_INPUT_ADDRESS reloads of that operand to
     RELOAD_FOR_OTHER_ADDRESS.  */

  for (i = 0; i < n_reloads; i++)
    {
      if (reload_secondary_p[i]
	  && reload_when_needed[i] == operand_type[reload_opnum[i]])
	reload_when_needed[i] = address_type[reload_opnum[i]];

      if ((reload_when_needed[i] == RELOAD_FOR_INPUT_ADDRESS
	   || reload_when_needed[i] == RELOAD_FOR_OUTPUT_ADDRESS)
	  && (operand_reloadnum[reload_opnum[i]] < 0
	      || reload_optional[operand_reloadnum[reload_opnum[i]]]))
	{
	  /* If we have a secondary reload to go along with this reload,
	     change its type to RELOAD_FOR_OPADDR_ADDR. */

	  if (reload_when_needed[i] == RELOAD_FOR_INPUT_ADDRESS
	      && reload_secondary_in_reload[i] != -1)
	    {
	      int secondary_in_reload = reload_secondary_in_reload[i];

	      reload_when_needed[secondary_in_reload] = 
		RELOAD_FOR_OPADDR_ADDR;

	      /* If there's a tertiary reload we have to change it also. */
	      if (secondary_in_reload > 0
		  && reload_secondary_in_reload[secondary_in_reload] != -1)
		reload_when_needed[reload_secondary_in_reload[secondary_in_reload]] 
		  = RELOAD_FOR_OPADDR_ADDR;
	    }

	  if (reload_when_needed[i] == RELOAD_FOR_OUTPUT_ADDRESS
	      && reload_secondary_out_reload[i] != -1)
	    {
	      int secondary_out_reload = reload_secondary_out_reload[i];

	      reload_when_needed[secondary_out_reload] = 
		RELOAD_FOR_OPADDR_ADDR;

	      /* If there's a tertiary reload we have to change it also. */
	      if (secondary_out_reload
		  && reload_secondary_out_reload[secondary_out_reload] != -1)
		reload_when_needed[reload_secondary_out_reload[secondary_out_reload]] 
		  = RELOAD_FOR_OPADDR_ADDR;
	    }
	  reload_when_needed[i] = RELOAD_FOR_OPERAND_ADDRESS;
	}

      if (reload_when_needed[i] == RELOAD_FOR_INPUT_ADDRESS
	  && operand_reloadnum[reload_opnum[i]] >= 0
	  && (reload_when_needed[operand_reloadnum[reload_opnum[i]]] 
	      == RELOAD_OTHER))
	reload_when_needed[i] = RELOAD_FOR_OTHER_ADDRESS;

      if (goal_alternative_matches[reload_opnum[i]] >= 0)
	reload_opnum[i] = goal_alternative_matches[reload_opnum[i]];
    }

  /* See if we have any reloads that are now allowed to be merged
     because we've changed when the reload is needed to
     RELOAD_FOR_OPERAND_ADDRESS or RELOAD_FOR_OTHER_ADDRESS.  Only
     check for the most common cases.  */

  for (i = 0; i < n_reloads; i++)
    if (reload_in[i] != 0 && reload_out[i] == 0
	&& (reload_when_needed[i] == RELOAD_FOR_OPERAND_ADDRESS
	    || reload_when_needed[i] == RELOAD_FOR_OTHER_ADDRESS))
      for (j = 0; j < n_reloads; j++)
	if (i != j && reload_in[j] != 0 && reload_out[j] == 0
	    && reload_when_needed[j] == reload_when_needed[i]
	    && MATCHES (reload_in[i], reload_in[j])
	    && reload_reg_class[i] == reload_reg_class[j]
	    && !reload_nocombine[i] && !reload_nocombine[j]
	    && reload_reg_rtx[i] == reload_reg_rtx[j])
	  {
	    reload_opnum[i] = MIN (reload_opnum[i], reload_opnum[j]);
	    transfer_replacements (i, j);
	    reload_in[j] = 0;
	  }

#else /* no REGISTER_CONSTRAINTS */
  int noperands;
  int insn_code_number;
  int goal_earlyclobber = 0; /* Always 0, to make combine_reloads happen.  */
  register int i;
  rtx body = PATTERN (insn);

  n_reloads = 0;
  n_replacements = 0;
  n_earlyclobbers = 0;
  replace_reloads = replace;
  this_insn = insn;

  /* Find what kind of insn this is.  NOPERANDS gets number of operands.
     Store the operand values in RECOG_OPERAND and the locations
     of the words in the insn that point to them in RECOG_OPERAND_LOC.
     Return if the insn needs no reload processing.  */

  switch (GET_CODE (body))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case PARALLEL:
    case SET:
      noperands = asm_noperands (body);
      if (noperands >= 0)
	{
	  /* This insn is an `asm' with operands.
	     First, find out how many operands, and allocate space.  */

	  insn_code_number = -1;
	  /* ??? This is a bug! ???
	     Give up and delete this insn if it has too many operands.  */
	  if (noperands > MAX_RECOG_OPERANDS)
	    abort ();

	  /* Now get the operand values out of the insn.  */

	  decode_asm_operands (body, recog_operand, recog_operand_loc,
			       NULL_PTR, NULL_PTR);
	  break;
	}

    default:
      /* Ordinary insn: recognize it, allocate space for operands and
	 constraints, and get them out via insn_extract.  */

      insn_code_number = recog_memoized (insn);
      noperands = insn_n_operands[insn_code_number];
      insn_extract (insn);
    }

  if (noperands == 0)
    return;

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);
      int is_set_dest = GET_CODE (body) == SET && (i == 0);

      if (insn_code_number >= 0)
	if (insn_operand_address_p[insn_code_number][i])
	  find_reloads_address (VOIDmode, NULL_PTR,
				recog_operand[i], recog_operand_loc[i],
				i, RELOAD_FOR_INPUT, ind_levels);

      /* In these cases, we can't tell if the operand is an input
	 or an output, so be conservative.  In practice it won't be
	 problem.  */

      if (code == MEM)
	find_reloads_address (GET_MODE (recog_operand[i]),
			      recog_operand_loc[i],
			      XEXP (recog_operand[i], 0),
			      &XEXP (recog_operand[i], 0),
			      i, RELOAD_OTHER, ind_levels);
      if (code == SUBREG)
	recog_operand[i] = *recog_operand_loc[i]
	  = find_reloads_toplev (recog_operand[i], i, RELOAD_OTHER,
				 ind_levels, is_set_dest);
      if (code == REG)
	{
	  register int regno = REGNO (recog_operand[i]);
	  if (reg_equiv_constant[regno] != 0 && !is_set_dest)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_constant[regno];
#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
	  if (reg_equiv_mem[regno] != 0)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_mem[regno];
#endif
	}
    }

  /* Perhaps an output reload can be combined with another
     to reduce needs by one.  */
  if (!goal_earlyclobber)
    combine_reloads ();
#endif /* no REGISTER_CONSTRAINTS */
}

/* Return 1 if alternative number ALTNUM in constraint-string CONSTRAINT
   accepts a memory operand with constant address.  */

static int
alternative_allows_memconst (constraint, altnum)
     char *constraint;
     int altnum;
{
  register int c;
  /* Skip alternatives before the one requested.  */
  while (altnum > 0)
    {
      while (*constraint++ != ',');
      altnum--;
    }
  /* Scan the requested alternative for 'm' or 'o'.
     If one of them is present, this alternative accepts memory constants.  */
  while ((c = *constraint++) && c != ',' && c != '#')
    if (c == 'm' || c == 'o')
      return 1;
  return 0;
}

/* Scan X for memory references and scan the addresses for reloading.
   Also checks for references to "constant" regs that we want to eliminate
   and replaces them with the values they stand for.
   We may alter X destructively if it contains a reference to such.
   If X is just a constant reg, we return the equivalent value
   instead of X.

   IND_LEVELS says how many levels of indirect addressing this machine
   supports.

   OPNUM and TYPE identify the purpose of the reload.

   IS_SET_DEST is true if X is the destination of a SET, which is not
   appropriate to be replaced by a constant.  */

static rtx
find_reloads_toplev (x, opnum, type, ind_levels, is_set_dest)
     rtx x;
     int opnum;
     enum reload_type type;
     int ind_levels;
     int is_set_dest;
{
  register RTX_CODE code = GET_CODE (x);

  register char *fmt = GET_RTX_FORMAT (code);
  register int i;

  if (code == REG)
    {
      /* This code is duplicated for speed in find_reloads.  */
      register int regno = REGNO (x);
      if (reg_equiv_constant[regno] != 0 && !is_set_dest)
	x = reg_equiv_constant[regno];
#if 0
/*  This creates (subreg (mem...)) which would cause an unnecessary
    reload of the mem.  */
      else if (reg_equiv_mem[regno] != 0)
	x = reg_equiv_mem[regno];
#endif
      else if (reg_equiv_address[regno] != 0)
	{
	  /* If reg_equiv_address varies, it may be shared, so copy it.  */
	  rtx addr = reg_equiv_address[regno];

	  if (rtx_varies_p (addr))
	    addr = copy_rtx (addr);

	  x = gen_rtx (MEM, GET_MODE (x), addr);
	  RTX_UNCHANGING_P (x) = RTX_UNCHANGING_P (regno_reg_rtx[regno]);
	  find_reloads_address (GET_MODE (x), NULL_PTR,
				XEXP (x, 0),
				&XEXP (x, 0), opnum, type, ind_levels);
	}
      return x;
    }
  if (code == MEM)
    {
      rtx tem = x;
      find_reloads_address (GET_MODE (x), &tem, XEXP (x, 0), &XEXP (x, 0),
			    opnum, type, ind_levels);
      return tem;
    }

  if (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG)
    {
      /* Check for SUBREG containing a REG that's equivalent to a constant. 
	 If the constant has a known value, truncate it right now.
	 Similarly if we are extracting a single-word of a multi-word
	 constant.  If the constant is symbolic, allow it to be substituted
	 normally.  push_reload will strip the subreg later.  If the
	 constant is VOIDmode, abort because we will lose the mode of
	 the register (this should never happen because one of the cases
	 above should handle it).  */

      register int regno = REGNO (SUBREG_REG (x));
      rtx tem;

      if (subreg_lowpart_p (x)
	  && regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0
	  && (tem = gen_lowpart_common (GET_MODE (x),
					reg_equiv_constant[regno])) != 0)
	return tem;

      if (GET_MODE_BITSIZE (GET_MODE (x)) == BITS_PER_WORD
	  && regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0
	  && (tem = operand_subword (reg_equiv_constant[regno],
				     SUBREG_WORD (x), 0,
				     GET_MODE (SUBREG_REG (x)))) != 0)
	return tem;

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0
	  && GET_MODE (reg_equiv_constant[regno]) == VOIDmode)
	abort ();

      /* If the subreg contains a reg that will be converted to a mem,
	 convert the subreg to a narrower memref now.
	 Otherwise, we would get (subreg (mem ...) ...),
	 which would force reload of the mem.

	 We also need to do this if there is an equivalent MEM that is
	 not offsettable.  In that case, alter_subreg would produce an
	 invalid address on big-endian machines.

	 For machines that extend byte loads, we must not reload using
	 a wider mode if we have a paradoxical SUBREG.  find_reloads will
	 force a reload in that case.  So we should not do anything here.  */

      else if (regno >= FIRST_PSEUDO_REGISTER
#ifdef LOAD_EXTEND_OP
	       && (GET_MODE_SIZE (GET_MODE (x))
		   <= GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
#endif
	       && (reg_equiv_address[regno] != 0
		   || (reg_equiv_mem[regno] != 0
		       && (! strict_memory_address_p (GET_MODE (x), 
						      XEXP (reg_equiv_mem[regno], 0))
			   || ! offsettable_memref_p (reg_equiv_mem[regno])))))
	{
	  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
	  rtx addr = (reg_equiv_address[regno] ? reg_equiv_address[regno]
		      : XEXP (reg_equiv_mem[regno], 0));
#if BYTES_BIG_ENDIAN
	  int size;
	  size = GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)));
	  offset += MIN (size, UNITS_PER_WORD);
	  size = GET_MODE_SIZE (GET_MODE (x));
	  offset -= MIN (size, UNITS_PER_WORD);
#endif
	  addr = plus_constant (addr, offset);
	  x = gen_rtx (MEM, GET_MODE (x), addr);
	  RTX_UNCHANGING_P (x) = RTX_UNCHANGING_P (regno_reg_rtx[regno]);
	  find_reloads_address (GET_MODE (x), NULL_PTR,
				XEXP (x, 0),
				&XEXP (x, 0), opnum, type, ind_levels);
	}

    }

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = find_reloads_toplev (XEXP (x, i), opnum, type,
					   ind_levels, is_set_dest);
    }
  return x;
}

/* Return a mem ref for the memory equivalent of reg REGNO.
   This mem ref is not shared with anything.  */

static rtx
make_memloc (ad, regno)
     rtx ad;
     int regno;
{
  register int i;
  rtx tem = reg_equiv_address[regno];

#if 0 /* We cannot safely reuse a memloc made here;
	 if the pseudo appears twice, and its mem needs a reload,
	 it gets two separate reloads assigned, but it only
	 gets substituted with the second of them;
	 then it can get used before that reload reg gets loaded up.  */
  for (i = 0; i < n_memlocs; i++)
    if (rtx_equal_p (tem, XEXP (memlocs[i], 0)))
      return memlocs[i];
#endif

  /* If TEM might contain a pseudo, we must copy it to avoid
     modifying it when we do the substitution for the reload.  */
  if (rtx_varies_p (tem))
    tem = copy_rtx (tem);

  tem = gen_rtx (MEM, GET_MODE (ad), tem);
  RTX_UNCHANGING_P (tem) = RTX_UNCHANGING_P (regno_reg_rtx[regno]);
  memlocs[n_memlocs++] = tem;
  return tem;
}

/* Record all reloads needed for handling memory address AD
   which appears in *LOC in a memory reference to mode MODE
   which itself is found in location  *MEMREFLOC.
   Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.

   OPNUM and TYPE specify the purpose of this reload.

   IND_LEVELS says how many levels of indirect addressing this machine
   supports.

   Value is nonzero if this address is reloaded or replaced as a whole.
   This is interesting to the caller if the address is an autoincrement.

   Note that there is no verification that the address will be valid after
   this routine does its work.  Instead, we rely on the fact that the address
   was valid when reload started.  So we need only undo things that reload
   could have broken.  These are wrong register types, pseudos not allocated
   to a hard register, and frame pointer elimination.  */

static int
find_reloads_address (mode, memrefloc, ad, loc, opnum, type, ind_levels)
     enum machine_mode mode;
     rtx *memrefloc;
     rtx ad;
     rtx *loc;
     int opnum;
     enum reload_type type;
     int ind_levels;
{
  register int regno;
  rtx tem;

  /* If the address is a register, see if it is a legitimate address and
     reload if not.  We first handle the cases where we need not reload
     or where we must reload in a non-standard way.  */

  if (GET_CODE (ad) == REG)
    {
      regno = REGNO (ad);

      if (reg_equiv_constant[regno] != 0
	  && strict_memory_address_p (mode, reg_equiv_constant[regno]))
	{
	  *loc = ad = reg_equiv_constant[regno];
	  return 1;
	}

      else if (reg_equiv_address[regno] != 0)
	{
	  tem = make_memloc (ad, regno);
	  find_reloads_address (GET_MODE (tem), NULL_PTR, XEXP (tem, 0),
				&XEXP (tem, 0), opnum, type, ind_levels);
	  push_reload (tem, NULL_RTX, loc, NULL_PTR, BASE_REG_CLASS,
		       GET_MODE (ad), VOIDmode, 0, 0,
		       opnum, type);
	  return 1;
	}

      /* We can avoid a reload if the register's equivalent memory expression
	 is valid as an indirect memory address.
	 But not all addresses are valid in a mem used as an indirect address:
	 only reg or reg+constant.  */

      else if (reg_equiv_mem[regno] != 0 && ind_levels > 0
	       && strict_memory_address_p (mode, reg_equiv_mem[regno])
	       && (GET_CODE (XEXP (reg_equiv_mem[regno], 0)) == REG
		   || (GET_CODE (XEXP (reg_equiv_mem[regno], 0)) == PLUS
		       && GET_CODE (XEXP (XEXP (reg_equiv_mem[regno], 0), 0)) == REG
		       && CONSTANT_P (XEXP (XEXP (reg_equiv_mem[regno], 0), 0)))))
	return 0;

      /* The only remaining case where we can avoid a reload is if this is a
	 hard register that is valid as a base register and which is not the
	 subject of a CLOBBER in this insn.  */

      else if (regno < FIRST_PSEUDO_REGISTER && REGNO_OK_FOR_BASE_P (regno)
	       && ! regno_clobbered_p (regno, this_insn))
	return 0;

      /* If we do not have one of the cases above, we must do the reload.  */
      push_reload (ad, NULL_RTX, loc, NULL_PTR, BASE_REG_CLASS,
		   GET_MODE (ad), VOIDmode, 0, 0, opnum, type);
      return 1;
    }

  if (strict_memory_address_p (mode, ad))
    {
      /* The address appears valid, so reloads are not needed.
	 But the address may contain an eliminable register.
	 This can happen because a machine with indirect addressing
	 may consider a pseudo register by itself a valid address even when
	 it has failed to get a hard reg.
	 So do a tree-walk to find and eliminate all such regs.  */

      /* But first quickly dispose of a common case.  */
      if (GET_CODE (ad) == PLUS
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT
	  && GET_CODE (XEXP (ad, 0)) == REG
	  && reg_equiv_constant[REGNO (XEXP (ad, 0))] == 0)
	return 0;

      subst_reg_equivs_changed = 0;
      *loc = subst_reg_equivs (ad);

      if (! subst_reg_equivs_changed)
	return 0;

      /* Check result for validity after substitution.  */
      if (strict_memory_address_p (mode, ad))
	return 0;
    }

  /* The address is not valid.  We have to figure out why.  One possibility
     is that it is itself a MEM.  This can happen when the frame pointer is
     being eliminated, a pseudo is not allocated to a hard register, and the
     offset between the frame and stack pointers is not its initial value.
     In that case the pseudo will have been replaced by a MEM referring to
     the stack pointer.  */
  if (GET_CODE (ad) == MEM)
    {
      /* First ensure that the address in this MEM is valid.  Then, unless
	 indirect addresses are valid, reload the MEM into a register.  */
      tem = ad;
      find_reloads_address (GET_MODE (ad), &tem, XEXP (ad, 0), &XEXP (ad, 0),
			    opnum, type, ind_levels == 0 ? 0 : ind_levels - 1);

      /* If tem was changed, then we must create a new memory reference to
	 hold it and store it back into memrefloc.  */
      if (tem != ad && memrefloc)
	{
	  *memrefloc = copy_rtx (*memrefloc);
	  copy_replacements (tem, XEXP (*memrefloc, 0));
	  loc = &XEXP (*memrefloc, 0);
	}

      /* Check similar cases as for indirect addresses as above except
	 that we can allow pseudos and a MEM since they should have been
	 taken care of above.  */

      if (ind_levels == 0
	  || (GET_CODE (XEXP (tem, 0)) == SYMBOL_REF && ! indirect_symref_ok)
	  || GET_CODE (XEXP (tem, 0)) == MEM
	  || ! (GET_CODE (XEXP (tem, 0)) == REG
		|| (GET_CODE (XEXP (tem, 0)) == PLUS
		    && GET_CODE (XEXP (XEXP (tem, 0), 0)) == REG
		    && GET_CODE (XEXP (XEXP (tem, 0), 1)) == CONST_INT)))
	{
	  /* Must use TEM here, not AD, since it is the one that will
	     have any subexpressions reloaded, if needed.  */
	  push_reload (tem, NULL_RTX, loc, NULL_PTR,
		       BASE_REG_CLASS, GET_MODE (tem), VOIDmode, 0,
		       0, opnum, type);
	  return 1;
	}
      else
	return 0;
    }

  /* If we have address of a stack slot but it's not valid
     (displacement is too large), compute the sum in a register.  */
  else if (GET_CODE (ad) == PLUS
	   && (XEXP (ad, 0) == frame_pointer_rtx
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	       || XEXP (ad, 0) == hard_frame_pointer_rtx
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       || XEXP (ad, 0) == arg_pointer_rtx
#endif
	       || XEXP (ad, 0) == stack_pointer_rtx)
	   && GET_CODE (XEXP (ad, 1)) == CONST_INT)
    {
      /* Unshare the MEM rtx so we can safely alter it.  */
      if (memrefloc)
	{
	  *memrefloc = copy_rtx (*memrefloc);
	  loc = &XEXP (*memrefloc, 0);
	}
      if (double_reg_address_ok)
	{
	  /* Unshare the sum as well.  */
	  *loc = ad = copy_rtx (ad);
	  /* Reload the displacement into an index reg.
	     We assume the frame pointer or arg pointer is a base reg.  */
	  find_reloads_address_part (XEXP (ad, 1), &XEXP (ad, 1),
				     INDEX_REG_CLASS, GET_MODE (ad), opnum,
				     type, ind_levels);
	}
      else
	{
	  /* If the sum of two regs is not necessarily valid,
	     reload the sum into a base reg.
	     That will at least work.  */
	  find_reloads_address_part (ad, loc, BASE_REG_CLASS, Pmode,
				     opnum, type, ind_levels);
	}
      return 1;
    }

  /* If we have an indexed stack slot, there are three possible reasons why
     it might be invalid: The index might need to be reloaded, the address
     might have been made by frame pointer elimination and hence have a
     constant out of range, or both reasons might apply.  

     We can easily check for an index needing reload, but even if that is the
     case, we might also have an invalid constant.  To avoid making the
     conservative assumption and requiring two reloads, we see if this address
     is valid when not interpreted strictly.  If it is, the only problem is
     that the index needs a reload and find_reloads_address_1 will take care
     of it.

     There is still a case when we might generate an extra reload,
     however.  In certain cases eliminate_regs will return a MEM for a REG
     (see the code there for details).  In those cases, memory_address_p
     applied to our address will return 0 so we will think that our offset
     must be too large.  But it might indeed be valid and the only problem
     is that a MEM is present where a REG should be.  This case should be
     very rare and there doesn't seem to be any way to avoid it.

     If we decide to do something here, it must be that
     `double_reg_address_ok' is true and that this address rtl was made by
     eliminate_regs.  We generate a reload of the fp/sp/ap + constant and
     rework the sum so that the reload register will be added to the index.
     This is safe because we know the address isn't shared.

     We check for fp/ap/sp as both the first and second operand of the
     innermost PLUS.  */

  else if (GET_CODE (ad) == PLUS && GET_CODE (XEXP (ad, 1)) == CONST_INT
	   && GET_CODE (XEXP (ad, 0)) == PLUS
	   && (XEXP (XEXP (ad, 0), 0) == frame_pointer_rtx
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
	       || XEXP (XEXP (ad, 0), 0) == hard_frame_pointer_rtx
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       || XEXP (XEXP (ad, 0), 0) == arg_pointer_rtx
#endif
	       || XEXP (XEXP (ad, 0), 0) == stack_pointer_rtx)
	   && ! memory_address_p (mode, ad))
    {
      *loc = ad = gen_rtx (PLUS, GET_MODE (ad),
			   plus_constant (XEXP (XEXP (ad, 0), 0),
					  INTVAL (XEXP (ad, 1))),
			   XEXP (XEXP (ad, 0), 1));
      find_reloads_address_part (XEXP (ad, 0), &XEXP (ad, 0), BASE_REG_CLASS,
				 GET_MODE (ad), opnum, type, ind_levels);
      find_reloads_address_1 (XEXP (ad, 1), 1, &XEXP (ad, 1), opnum, type, 0);

      return 1;
    }
			   
  else if (GET_CODE (ad) == PLUS && GET_CODE (XEXP (ad, 1)) == CONST_INT
	   && GET_CODE (XEXP (ad, 0)) == PLUS
	   && (XEXP (XEXP (ad, 0), 1) == frame_pointer_rtx
#if HARD_FRAME_POINTER_REGNUM != FRAME_POINTER_REGNUM
	       || XEXP (XEXP (ad, 0), 1) == hard_frame_pointer_rtx
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       || XEXP (XEXP (ad, 0), 1) == arg_pointer_rtx
#endif
	       || XEXP (XEXP (ad, 0), 1) == stack_pointer_rtx)
	   && ! memory_address_p (mode, ad))
    {
      *loc = ad = gen_rtx (PLUS, GET_MODE (ad),
			   XEXP (XEXP (ad, 0), 0),
			   plus_constant (XEXP (XEXP (ad, 0), 1),
					  INTVAL (XEXP (ad, 1))));
      find_reloads_address_part (XEXP (ad, 1), &XEXP (ad, 1), BASE_REG_CLASS,
				 GET_MODE (ad), opnum, type, ind_levels);
      find_reloads_address_1 (XEXP (ad, 0), 1, &XEXP (ad, 0), opnum, type, 0);

      return 1;
    }
			   
  /* See if address becomes valid when an eliminable register
     in a sum is replaced.  */

  tem = ad;
  if (GET_CODE (ad) == PLUS)
    tem = subst_indexed_address (ad);
  if (tem != ad && strict_memory_address_p (mode, tem))
    {
      /* Ok, we win that way.  Replace any additional eliminable
	 registers.  */

      subst_reg_equivs_changed = 0;
      tem = subst_reg_equivs (tem);

      /* Make sure that didn't make the address invalid again.  */

      if (! subst_reg_equivs_changed || strict_memory_address_p (mode, tem))
	{
	  *loc = tem;
	  return 0;
	}
    }

  /* If constants aren't valid addresses, reload the constant address
     into a register.  */
  if (CONSTANT_P (ad) && ! strict_memory_address_p (mode, ad))
    {
      /* If AD is in address in the constant pool, the MEM rtx may be shared.
	 Unshare it so we can safely alter it.  */
      if (memrefloc && GET_CODE (ad) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (ad))
	{
	  *memrefloc = copy_rtx (*memrefloc);
	  loc = &XEXP (*memrefloc, 0);
	}

      find_reloads_address_part (ad, loc, BASE_REG_CLASS, Pmode, opnum, type,
				 ind_levels);
      return 1;
    }

  return find_reloads_address_1 (ad, 0, loc, opnum, type, ind_levels);
}

/* Find all pseudo regs appearing in AD
   that are eliminable in favor of equivalent values
   and do not have hard regs; replace them by their equivalents.  */

static rtx
subst_reg_equivs (ad)
     rtx ad;
{
  register RTX_CODE code = GET_CODE (ad);
  register int i;
  register char *fmt;

  switch (code)
    {
    case HIGH:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
      return ad;

    case REG:
      {
	register int regno = REGNO (ad);

	if (reg_equiv_constant[regno] != 0)
	  {
	    subst_reg_equivs_changed = 1;
	    return reg_equiv_constant[regno];
	  }
      }
      return ad;

    case PLUS:
      /* Quickly dispose of a common case.  */
      if (XEXP (ad, 0) == frame_pointer_rtx
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT)
	return ad;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      XEXP (ad, i) = subst_reg_equivs (XEXP (ad, i));
  return ad;
}

/* Compute the sum of X and Y, making canonicalizations assumed in an
   address, namely: sum constant integers, surround the sum of two
   constants with a CONST, put the constant as the second operand, and
   group the constant on the outermost sum.

   This routine assumes both inputs are already in canonical form.  */

rtx
form_sum (x, y)
     rtx x, y;
{
  rtx tem;
  enum machine_mode mode = GET_MODE (x);

  if (mode == VOIDmode)
    mode = GET_MODE (y);

  if (mode == VOIDmode)
    mode = Pmode;

  if (GET_CODE (x) == CONST_INT)
    return plus_constant (y, INTVAL (x));
  else if (GET_CODE (y) == CONST_INT)
    return plus_constant (x, INTVAL (y));
  else if (CONSTANT_P (x))
    tem = x, x = y, y = tem;

  if (GET_CODE (x) == PLUS && CONSTANT_P (XEXP (x, 1)))
    return form_sum (XEXP (x, 0), form_sum (XEXP (x, 1), y));

  /* Note that if the operands of Y are specified in the opposite
     order in the recursive calls below, infinite recursion will occur.  */
  if (GET_CODE (y) == PLUS && CONSTANT_P (XEXP (y, 1)))
    return form_sum (form_sum (x, XEXP (y, 0)), XEXP (y, 1));

  /* If both constant, encapsulate sum.  Otherwise, just form sum.  A
     constant will have been placed second.  */
  if (CONSTANT_P (x) && CONSTANT_P (y))
    {
      if (GET_CODE (x) == CONST)
	x = XEXP (x, 0);
      if (GET_CODE (y) == CONST)
	y = XEXP (y, 0);

      return gen_rtx (CONST, VOIDmode, gen_rtx (PLUS, mode, x, y));
    }

  return gen_rtx (PLUS, mode, x, y);
}

/* If ADDR is a sum containing a pseudo register that should be
   replaced with a constant (from reg_equiv_constant),
   return the result of doing so, and also apply the associative
   law so that the result is more likely to be a valid address.
   (But it is not guaranteed to be one.)

   Note that at most one register is replaced, even if more are
   replaceable.  Also, we try to put the result into a canonical form
   so it is more likely to be a valid address.

   In all other cases, return ADDR.  */

static rtx
subst_indexed_address (addr)
     rtx addr;
{
  rtx op0 = 0, op1 = 0, op2 = 0;
  rtx tem;
  int regno;

  if (GET_CODE (addr) == PLUS)
    {
      /* Try to find a register to replace.  */
      op0 = XEXP (addr, 0), op1 = XEXP (addr, 1), op2 = 0;
      if (GET_CODE (op0) == REG
	  && (regno = REGNO (op0)) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	op0 = reg_equiv_constant[regno];
      else if (GET_CODE (op1) == REG
	  && (regno = REGNO (op1)) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	op1 = reg_equiv_constant[regno];
      else if (GET_CODE (op0) == PLUS
	       && (tem = subst_indexed_address (op0)) != op0)
	op0 = tem;
      else if (GET_CODE (op1) == PLUS
	       && (tem = subst_indexed_address (op1)) != op1)
	op1 = tem;
      else
	return addr;

      /* Pick out up to three things to add.  */
      if (GET_CODE (op1) == PLUS)
	op2 = XEXP (op1, 1), op1 = XEXP (op1, 0);
      else if (GET_CODE (op0) == PLUS)
	op2 = op1, op1 = XEXP (op0, 1), op0 = XEXP (op0, 0);

      /* Compute the sum.  */
      if (op2 != 0)
	op1 = form_sum (op1, op2);
      if (op1 != 0)
	op0 = form_sum (op0, op1);

      return op0;
    }
  return addr;
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a would-be memory address, X.
   (This function is not called if the address we find is strictly valid.)
   CONTEXT = 1 means we are considering regs as index regs,
   = 0 means we are considering them as base regs.

   OPNUM and TYPE specify the purpose of any reloads made.

   IND_LEVELS says how many levels of indirect addressing are
   supported at this point in the address.

   We return nonzero if X, as a whole, is reloaded or replaced.  */

/* Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.
   Also, this is not fully machine-customizable; it works for machines
   such as vaxes and 68000's and 32000's, but other possible machines
   could have addressing modes that this does not handle right.  */

static int
find_reloads_address_1 (x, context, loc, opnum, type, ind_levels)
     rtx x;
     int context;
     rtx *loc;
     int opnum;
     enum reload_type type;
     int ind_levels;
{
  register RTX_CODE code = GET_CODE (x);

  switch (code)
    {
    case PLUS:
      {
	register rtx orig_op0 = XEXP (x, 0);
	register rtx orig_op1 = XEXP (x, 1);
	register RTX_CODE code0 = GET_CODE (orig_op0);
	register RTX_CODE code1 = GET_CODE (orig_op1);
	register rtx op0 = orig_op0;
	register rtx op1 = orig_op1;

	if (GET_CODE (op0) == SUBREG)
	  {
	    op0 = SUBREG_REG (op0);
	    code0 = GET_CODE (op0);
	  }

	if (GET_CODE (op1) == SUBREG)
	  {
	    op1 = SUBREG_REG (op1);
	    code1 = GET_CODE (op1);
	  }

	if (code0 == MULT || code0 == SIGN_EXTEND || code1 == MEM)
	  {
	    find_reloads_address_1 (orig_op0, 1, &XEXP (x, 0), opnum, type,
				    ind_levels);
	    find_reloads_address_1 (orig_op1, 0, &XEXP (x, 1), opnum, type,
				    ind_levels);
	  }

	else if (code1 == MULT || code1 == SIGN_EXTEND || code0 == MEM)
	  {
	    find_reloads_address_1 (orig_op0, 0, &XEXP (x, 0), opnum, type,
				    ind_levels);
	    find_reloads_address_1 (orig_op1, 1, &XEXP (x, 1), opnum, type,
				    ind_levels);
	  }

	else if (code0 == CONST_INT || code0 == CONST
		 || code0 == SYMBOL_REF || code0 == LABEL_REF)
	  find_reloads_address_1 (orig_op1, 0, &XEXP (x, 1), opnum, type,
				  ind_levels);

	else if (code1 == CONST_INT || code1 == CONST
		 || code1 == SYMBOL_REF || code1 == LABEL_REF)
	  find_reloads_address_1 (orig_op0, 0, &XEXP (x, 0), opnum, type,
				  ind_levels);

	else if (code0 == REG && code1 == REG)
	  {
	    if (REG_OK_FOR_INDEX_P (op0)
		&& REG_OK_FOR_BASE_P (op1))
	      return 0;
	    else if (REG_OK_FOR_INDEX_P (op1)
		     && REG_OK_FOR_BASE_P (op0))
	      return 0;
	    else if (REG_OK_FOR_BASE_P (op1))
	      find_reloads_address_1 (orig_op0, 1, &XEXP (x, 0), opnum, type, 
				      ind_levels);
	    else if (REG_OK_FOR_BASE_P (op0))
	      find_reloads_address_1 (orig_op1, 1, &XEXP (x, 1), opnum, type,
				      ind_levels);
	    else if (REG_OK_FOR_INDEX_P (op1))
	      find_reloads_address_1 (orig_op0, 0, &XEXP (x, 0), opnum, type,
				      ind_levels);
	    else if (REG_OK_FOR_INDEX_P (op0))
	    find_reloads_address_1 (orig_op1, 0, &XEXP (x, 1), opnum, type,
				    ind_levels);
	    else
	      {
		find_reloads_address_1 (orig_op0, 1, &XEXP (x, 0), opnum, type,
					ind_levels);
		find_reloads_address_1 (orig_op1, 0, &XEXP (x, 1), opnum, type,
					ind_levels);
	      }
	  }

	else if (code0 == REG)
	  {
	    find_reloads_address_1 (orig_op0, 1, &XEXP (x, 0), opnum, type,
				    ind_levels);
	    find_reloads_address_1 (orig_op1, 0, &XEXP (x, 1), opnum, type,
				    ind_levels);
	  }

	else if (code1 == REG)
	  {
	    find_reloads_address_1 (orig_op1, 1, &XEXP (x, 1), opnum, type,
				    ind_levels);
	    find_reloads_address_1 (orig_op0, 0, &XEXP (x, 0), opnum, type,
				    ind_levels);
	  }
      }

      return 0;

    case POST_INC:
    case POST_DEC:
    case PRE_INC:
    case PRE_DEC:
      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register int regno = REGNO (XEXP (x, 0));
	  int value = 0;
	  rtx x_orig = x;

	  /* A register that is incremented cannot be constant!  */
	  if (regno >= FIRST_PSEUDO_REGISTER
	      && reg_equiv_constant[regno] != 0)
	    abort ();

	  /* Handle a register that is equivalent to a memory location
	     which cannot be addressed directly.  */
	  if (reg_equiv_address[regno] != 0)
	    {
	      rtx tem = make_memloc (XEXP (x, 0), regno);
	      /* First reload the memory location's address.  */
	      find_reloads_address (GET_MODE (tem), 0, XEXP (tem, 0),
				    &XEXP (tem, 0), opnum, type, ind_levels);
	      /* Put this inside a new increment-expression.  */
	      x = gen_rtx (GET_CODE (x), GET_MODE (x), tem);
	      /* Proceed to reload that, as if it contained a register.  */
	    }

	  /* If we have a hard register that is ok as an index,
	     don't make a reload.  If an autoincrement of a nice register
	     isn't "valid", it must be that no autoincrement is "valid".
	     If that is true and something made an autoincrement anyway,
	     this must be a special context where one is allowed.
	     (For example, a "push" instruction.)
	     We can't improve this address, so leave it alone.  */

	  /* Otherwise, reload the autoincrement into a suitable hard reg
	     and record how much to increment by.  */

	  if (reg_renumber[regno] >= 0)
	    regno = reg_renumber[regno];
	  if ((regno >= FIRST_PSEUDO_REGISTER
	       || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		    : REGNO_OK_FOR_BASE_P (regno))))
	    {
	      register rtx link;

	      int reloadnum
		= push_reload (x, NULL_RTX, loc, NULL_PTR,
			       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			       GET_MODE (x), GET_MODE (x), VOIDmode, 0,
			       opnum, type);
	      reload_inc[reloadnum]
		= find_inc_amount (PATTERN (this_insn), XEXP (x_orig, 0));

	      value = 1;

#ifdef AUTO_INC_DEC
	      /* Update the REG_INC notes.  */

	      for (link = REG_NOTES (this_insn);
		   link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_INC
		    && REGNO (XEXP (link, 0)) == REGNO (XEXP (x_orig, 0)))
		  push_replacement (&XEXP (link, 0), reloadnum, VOIDmode);
#endif
	    }
	  return value;
	}

      else if (GET_CODE (XEXP (x, 0)) == MEM)
	{
	  /* This is probably the result of a substitution, by eliminate_regs,
	     of an equivalent address for a pseudo that was not allocated to a
	     hard register.  Verify that the specified address is valid and
	     reload it into a register.  */
	  rtx tem = XEXP (x, 0);
	  register rtx link;
	  int reloadnum;

	  /* Since we know we are going to reload this item, don't decrement
	     for the indirection level.

	     Note that this is actually conservative:  it would be slightly
	     more efficient to use the value of SPILL_INDIRECT_LEVELS from
	     reload1.c here.  */
	  find_reloads_address (GET_MODE (x), &XEXP (x, 0),
				XEXP (XEXP (x, 0), 0), &XEXP (XEXP (x, 0), 0),
				opnum, type, ind_levels);

	  reloadnum = push_reload (x, NULL_RTX, loc, NULL_PTR,
				   context ? INDEX_REG_CLASS : BASE_REG_CLASS,
				   GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	  reload_inc[reloadnum]
	    = find_inc_amount (PATTERN (this_insn), XEXP (x, 0));

	  link = FIND_REG_INC_NOTE (this_insn, tem);
	  if (link != 0)
	    push_replacement (&XEXP (link, 0), reloadnum, VOIDmode);

	  return 1;
	}
      return 0;

    case MEM:
      /* This is probably the result of a substitution, by eliminate_regs, of
	 an equivalent address for a pseudo that was not allocated to a hard
	 register.  Verify that the specified address is valid and reload it
	 into a register.

	 Since we know we are going to reload this item, don't decrement for
	 the indirection level.

	 Note that this is actually conservative:  it would be slightly more
	 efficient to use the value of SPILL_INDIRECT_LEVELS from
	 reload1.c here.  */

      find_reloads_address (GET_MODE (x), loc, XEXP (x, 0), &XEXP (x, 0),
			    opnum, type, ind_levels);
      push_reload (*loc, NULL_RTX, loc, NULL_PTR,
		   context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		   GET_MODE (x), VOIDmode, 0, 0, opnum, type);
      return 1;

    case REG:
      {
	register int regno = REGNO (x);

	if (reg_equiv_constant[regno] != 0)
	  {
	    find_reloads_address_part (reg_equiv_constant[regno], loc, 
				       (context ? INDEX_REG_CLASS
					: BASE_REG_CLASS),
				       GET_MODE (x), opnum, type, ind_levels);
	    return 1;
	  }

#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
	if (reg_equiv_mem[regno] != 0)
	  {
	    push_reload (reg_equiv_mem[regno], NULL_RTX, loc, NULL_PTR,
			 context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			 GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	    return 1;
	  }
#endif

	if (reg_equiv_address[regno] != 0)
	  {
	    x = make_memloc (x, regno);
	    find_reloads_address (GET_MODE (x), 0, XEXP (x, 0), &XEXP (x, 0),
				  opnum, type, ind_levels);
	  }

	if (reg_renumber[regno] >= 0)
	  regno = reg_renumber[regno];

	if ((regno >= FIRST_PSEUDO_REGISTER
	     || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		  : REGNO_OK_FOR_BASE_P (regno))))
	  {
	    push_reload (x, NULL_RTX, loc, NULL_PTR,
			 context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			 GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	    return 1;
	  }

	/* If a register appearing in an address is the subject of a CLOBBER
	   in this insn, reload it into some other register to be safe.
	   The CLOBBER is supposed to make the register unavailable
	   from before this insn to after it.  */
	if (regno_clobbered_p (regno, this_insn))
	  {
	    push_reload (x, NULL_RTX, loc, NULL_PTR,
			 context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			 GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	    return 1;
	  }
      }
      return 0;

    case SUBREG:
      /* If this is a SUBREG of a hard register and the resulting register is
	 of the wrong class, reload the whole SUBREG.  This avoids needless
	 copies if SUBREG_REG is multi-word.  */
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER)
	{
	  int regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);

	  if (! (context ? REGNO_OK_FOR_INDEX_P (regno)
		 : REGNO_OK_FOR_BASE_P (regno)))
	    {
	      push_reload (x, NULL_RTX, loc, NULL_PTR,
			   context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			   GET_MODE (x), VOIDmode, 0, 0, opnum, type);
	      return 1;
	    }
	}
      break;
    }

  {
    register char *fmt = GET_RTX_FORMAT (code);
    register int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  find_reloads_address_1 (XEXP (x, i), context, &XEXP (x, i),
				  opnum, type, ind_levels);
      }
  }

  return 0;
}

/* X, which is found at *LOC, is a part of an address that needs to be
   reloaded into a register of class CLASS.  If X is a constant, or if
   X is a PLUS that contains a constant, check that the constant is a
   legitimate operand and that we are supposed to be able to load
   it into the register.

   If not, force the constant into memory and reload the MEM instead.

   MODE is the mode to use, in case X is an integer constant.

   OPNUM and TYPE describe the purpose of any reloads made.

   IND_LEVELS says how many levels of indirect addressing this machine
   supports.  */

static void
find_reloads_address_part (x, loc, class, mode, opnum, type, ind_levels)
     rtx x;
     rtx *loc;
     enum reg_class class;
     enum machine_mode mode;
     int opnum;
     enum reload_type type;
     int ind_levels;
{
  if (CONSTANT_P (x)
      && (! LEGITIMATE_CONSTANT_P (x)
	  || PREFERRED_RELOAD_CLASS (x, class) == NO_REGS))
    {
      rtx tem = x = force_const_mem (mode, x);
      find_reloads_address (mode, &tem, XEXP (tem, 0), &XEXP (tem, 0),
			    opnum, type, ind_levels);
    }

  else if (GET_CODE (x) == PLUS
	   && CONSTANT_P (XEXP (x, 1))
	   && (! LEGITIMATE_CONSTANT_P (XEXP (x, 1))
	       || PREFERRED_RELOAD_CLASS (XEXP (x, 1), class) == NO_REGS))
    {
      rtx tem = force_const_mem (GET_MODE (x), XEXP (x, 1));

      x = gen_rtx (PLUS, GET_MODE (x), XEXP (x, 0), tem);
      find_reloads_address (mode, &tem, XEXP (tem, 0), &XEXP (tem, 0),
			    opnum, type, ind_levels);
    }

  push_reload (x, NULL_RTX, loc, NULL_PTR, class,
	       mode, VOIDmode, 0, 0, opnum, type);
}

/* Substitute into the current INSN the registers into which we have reloaded
   the things that need reloading.  The array `replacements'
   says contains the locations of all pointers that must be changed
   and says what to replace them with.

   Return the rtx that X translates into; usually X, but modified.  */

void
subst_reloads ()
{
  register int i;

  for (i = 0; i < n_replacements; i++)
    {
      register struct replacement *r = &replacements[i];
      register rtx reloadreg = reload_reg_rtx[r->what];
      if (reloadreg)
	{
	  /* Encapsulate RELOADREG so its machine mode matches what
	     used to be there.  Note that gen_lowpart_common will
	     do the wrong thing if RELOADREG is multi-word.  RELOADREG
	     will always be a REG here.  */
	  if (GET_MODE (reloadreg) != r->mode && r->mode != VOIDmode)
	    reloadreg = gen_rtx (REG, r->mode, REGNO (reloadreg));

	  /* If we are putting this into a SUBREG and RELOADREG is a
	     SUBREG, we would be making nested SUBREGs, so we have to fix
	     this up.  Note that r->where == &SUBREG_REG (*r->subreg_loc).  */

	  if (r->subreg_loc != 0 && GET_CODE (reloadreg) == SUBREG)
	    {
	      if (GET_MODE (*r->subreg_loc)
		  == GET_MODE (SUBREG_REG (reloadreg)))
		*r->subreg_loc = SUBREG_REG (reloadreg);
	      else
		{
		  *r->where = SUBREG_REG (reloadreg);
		  SUBREG_WORD (*r->subreg_loc) += SUBREG_WORD (reloadreg);
		}
	    }
	  else
	    *r->where = reloadreg;
	}
      /* If reload got no reg and isn't optional, something's wrong.  */
      else if (! reload_optional[r->what])
	abort ();
    }
}

/* Make a copy of any replacements being done into X and move those copies
   to locations in Y, a copy of X.  We only look at the highest level of
   the RTL.  */

void
copy_replacements (x, y)
     rtx x;
     rtx y;
{
  int i, j;
  enum rtx_code code = GET_CODE (x);
  char *fmt = GET_RTX_FORMAT (code);
  struct replacement *r;

  /* We can't support X being a SUBREG because we might then need to know its
     location if something inside it was replaced.  */
  if (code == SUBREG)
    abort ();

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      for (j = 0; j < n_replacements; j++)
	{
	  if (replacements[j].subreg_loc == &XEXP (x, i))
	    {
	      r = &replacements[n_replacements++];
	      r->where = replacements[j].where;
	      r->subreg_loc = &XEXP (y, i);
	      r->what = replacements[j].what;
	      r->mode = replacements[j].mode;
	    }
	  else if (replacements[j].where == &XEXP (x, i))
	    {
	      r = &replacements[n_replacements++];
	      r->where = &XEXP (y, i);
	      r->subreg_loc = 0;
	      r->what = replacements[j].what;
	      r->mode = replacements[j].mode;
	    }
	}
}

/* If LOC was scheduled to be replaced by something, return the replacement.
   Otherwise, return *LOC.  */

rtx
find_replacement (loc)
     rtx *loc;
{
  struct replacement *r;

  for (r = &replacements[0]; r < &replacements[n_replacements]; r++)
    {
      rtx reloadreg = reload_reg_rtx[r->what];

      if (reloadreg && r->where == loc)
	{
	  if (r->mode != VOIDmode && GET_MODE (reloadreg) != r->mode)
	    reloadreg = gen_rtx (REG, r->mode, REGNO (reloadreg));

	  return reloadreg;
	}
      else if (reloadreg && r->subreg_loc == loc)
	{
	  /* RELOADREG must be either a REG or a SUBREG.

	     ??? Is it actually still ever a SUBREG?  If so, why?  */

	  if (GET_CODE (reloadreg) == REG)
	    return gen_rtx (REG, GET_MODE (*loc),
			    REGNO (reloadreg) + SUBREG_WORD (*loc));
	  else if (GET_MODE (reloadreg) == GET_MODE (*loc))
	    return reloadreg;
	  else
	    return gen_rtx (SUBREG, GET_MODE (*loc), SUBREG_REG (reloadreg),
			    SUBREG_WORD (reloadreg) + SUBREG_WORD (*loc));
	}
    }

  return *loc;
}

/* Return nonzero if register in range [REGNO, ENDREGNO)
   appears either explicitly or implicitly in X
   other than being stored into (except for earlyclobber operands).

   References contained within the substructure at LOC do not count.
   LOC may be zero, meaning don't ignore anything.

   This is similar to refers_to_regno_p in rtlanal.c except that we
   look at equivalences for pseudos that didn't get hard registers.  */

int
refers_to_regno_for_reload_p (regno, endregno, x, loc)
     int regno, endregno;
     rtx x;
     rtx *loc;
{
  register int i;
  register RTX_CODE code;
  register char *fmt;

  if (x == 0)
    return 0;

 repeat:
  code = GET_CODE (x);

  switch (code)
    {
    case REG:
      i = REGNO (x);

      /* If this is a pseudo, a hard register must not have been allocated.
	 X must therefore either be a constant or be in memory.  */
      if (i >= FIRST_PSEUDO_REGISTER)
	{
	  if (reg_equiv_memory_loc[i])
	    return refers_to_regno_for_reload_p (regno, endregno,
						 reg_equiv_memory_loc[i],
						 NULL_PTR);

	  if (reg_equiv_constant[i])
	    return 0;

	  abort ();
	}

      return (endregno > i
	      && regno < i + (i < FIRST_PSEUDO_REGISTER 
			      ? HARD_REGNO_NREGS (i, GET_MODE (x))
			      : 1));

    case SUBREG:
      /* If this is a SUBREG of a hard reg, we can see exactly which
	 registers are being modified.  Otherwise, handle normally.  */
      if (GET_CODE (SUBREG_REG (x)) == REG
	  && REGNO (SUBREG_REG (x)) < FIRST_PSEUDO_REGISTER)
	{
	  int inner_regno = REGNO (SUBREG_REG (x)) + SUBREG_WORD (x);
	  int inner_endregno
	    = inner_regno + (inner_regno < FIRST_PSEUDO_REGISTER
			     ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);

	  return endregno > inner_regno && regno < inner_endregno;
	}
      break;

    case CLOBBER:
    case SET:
      if (&SET_DEST (x) != loc
	  /* Note setting a SUBREG counts as referring to the REG it is in for
	     a pseudo but not for hard registers since we can
	     treat each word individually.  */
	  && ((GET_CODE (SET_DEST (x)) == SUBREG
	       && loc != &SUBREG_REG (SET_DEST (x))
	       && GET_CODE (SUBREG_REG (SET_DEST (x))) == REG
	       && REGNO (SUBREG_REG (SET_DEST (x))) >= FIRST_PSEUDO_REGISTER
	       && refers_to_regno_for_reload_p (regno, endregno,
						SUBREG_REG (SET_DEST (x)),
						loc))
	      /* If the ouput is an earlyclobber operand, this is
		 a conflict.  */
	      || ((GET_CODE (SET_DEST (x)) != REG
		   || earlyclobber_operand_p (SET_DEST (x)))
		  && refers_to_regno_for_reload_p (regno, endregno,
						   SET_DEST (x), loc))))
	return 1;

      if (code == CLOBBER || loc == &SET_SRC (x))
	return 0;
      x = SET_SRC (x);
      goto repeat;
    }

  /* X does not match, so try its subexpressions.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && loc != &XEXP (x, i))
	{
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  else
	    if (refers_to_regno_for_reload_p (regno, endregno,
					      XEXP (x, i), loc))
	      return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (loc != &XVECEXP (x, i, j)
		&& refers_to_regno_for_reload_p (regno, endregno,
						 XVECEXP (x, i, j), loc))
	      return 1;
	}
    }
  return 0;
}

/* Nonzero if modifying X will affect IN.  If X is a register or a SUBREG,
   we check if any register number in X conflicts with the relevant register
   numbers.  If X is a constant, return 0.  If X is a MEM, return 1 iff IN
   contains a MEM (we don't bother checking for memory addresses that can't
   conflict because we expect this to be a rare case. 

   This function is similar to reg_overlap_mention_p in rtlanal.c except
   that we look at equivalences for pseudos that didn't get hard registers.  */

int
reg_overlap_mentioned_for_reload_p (x, in)
     rtx x, in;
{
  int regno, endregno;

  if (GET_CODE (x) == SUBREG)
    {
      regno = REGNO (SUBREG_REG (x));
      if (regno < FIRST_PSEUDO_REGISTER)
	regno += SUBREG_WORD (x);
    }
  else if (GET_CODE (x) == REG)
    {
      regno = REGNO (x);

      /* If this is a pseudo, it must not have been assigned a hard register.
	 Therefore, it must either be in memory or be a constant.  */

      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  if (reg_equiv_memory_loc[regno])
	    return refers_to_mem_for_reload_p (in);
	  else if (reg_equiv_constant[regno])
	    return 0;
	  abort ();
	}
    }
  else if (CONSTANT_P (x))
    return 0;
  else if (GET_CODE (x) == MEM)
    return refers_to_mem_for_reload_p (in);
  else if (GET_CODE (x) == SCRATCH || GET_CODE (x) == PC
	   || GET_CODE (x) == CC0)
    return reg_mentioned_p (x, in);
  else
    abort ();

  endregno = regno + (regno < FIRST_PSEUDO_REGISTER
		      ? HARD_REGNO_NREGS (regno, GET_MODE (x)) : 1);

  return refers_to_regno_for_reload_p (regno, endregno, in, NULL_PTR);
}

/* Return nonzero if anything in X contains a MEM.  Look also for pseudo
   registers.  */

int
refers_to_mem_for_reload_p (x)
     rtx x;
{
  char *fmt;
  int i;

  if (GET_CODE (x) == MEM)
    return 1;

  if (GET_CODE (x) == REG)
    return (REGNO (x) >= FIRST_PSEUDO_REGISTER
	    && reg_equiv_memory_loc[REGNO (x)]);
			
  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    if (fmt[i] == 'e'
	&& (GET_CODE (XEXP (x, i)) == MEM
	    || refers_to_mem_for_reload_p (XEXP (x, i))))
      return 1;
  
  return 0;
}

/* Check the insns before INSN to see if there is a suitable register
   containing the same value as GOAL.
   If OTHER is -1, look for a register in class CLASS.
   Otherwise, just see if register number OTHER shares GOAL's value.

   Return an rtx for the register found, or zero if none is found.

   If RELOAD_REG_P is (short *)1,
   we reject any hard reg that appears in reload_reg_rtx
   because such a hard reg is also needed coming into this insn.

   If RELOAD_REG_P is any other nonzero value,
   it is a vector indexed by hard reg number
   and we reject any hard reg whose element in the vector is nonnegative
   as well as any that appears in reload_reg_rtx.

   If GOAL is zero, then GOALREG is a register number; we look
   for an equivalent for that register.

   MODE is the machine mode of the value we want an equivalence for.
   If GOAL is nonzero and not VOIDmode, then it must have mode MODE.

   This function is used by jump.c as well as in the reload pass.

   If GOAL is the sum of the stack pointer and a constant, we treat it
   as if it were a constant except that sp is required to be unchanging.  */

rtx
find_equiv_reg (goal, insn, class, other, reload_reg_p, goalreg, mode)
     register rtx goal;
     rtx insn;
     enum reg_class class;
     register int other;
     short *reload_reg_p;
     int goalreg;
     enum machine_mode mode;
{
  register rtx p = insn;
  rtx goaltry, valtry, value, where;
  register rtx pat;
  register int regno = -1;
  int valueno;
  int goal_mem = 0;
  int goal_const = 0;
  int goal_mem_addr_varies = 0;
  int need_stable_sp = 0;
  int nregs;
  int valuenregs;

  if (goal == 0)
    regno = goalreg;
  else if (GET_CODE (goal) == REG)
    regno = REGNO (goal);
  else if (GET_CODE (goal) == MEM)
    {
      enum rtx_code code = GET_CODE (XEXP (goal, 0));
      if (MEM_VOLATILE_P (goal))
	return 0;
      if (flag_float_store && GET_MODE_CLASS (GET_MODE (goal)) == MODE_FLOAT)
	return 0;
      /* An address with side effects must be reexecuted.  */
      switch (code)
	{
	case POST_INC:
	case PRE_INC:
	case POST_DEC:
	case PRE_DEC:
	  return 0;
	}
      goal_mem = 1;
    }
  else if (CONSTANT_P (goal))
    goal_const = 1;
  else if (GET_CODE (goal) == PLUS
	   && XEXP (goal, 0) == stack_pointer_rtx
	   && CONSTANT_P (XEXP (goal, 1)))
    goal_const = need_stable_sp = 1;
  else
    return 0;

  /* On some machines, certain regs must always be rejected
     because they don't behave the way ordinary registers do.  */
  
#ifdef OVERLAPPING_REGNO_P
   if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER
       && OVERLAPPING_REGNO_P (regno))
     return 0;
#endif      

  /* Scan insns back from INSN, looking for one that copies
     a value into or out of GOAL.
     Stop and give up if we reach a label.  */

  while (1)
    {
      p = PREV_INSN (p);
      if (p == 0 || GET_CODE (p) == CODE_LABEL)
	return 0;
      if (GET_CODE (p) == INSN
	  /* If we don't want spill regs ... */
	  && (! (reload_reg_p != 0
		 && reload_reg_p != (short *) (HOST_WIDE_INT) 1)
	  /* ... then ignore insns introduced by reload; they aren't useful
	     and can cause results in reload_as_needed to be different
	     from what they were when calculating the need for spills.
	     If we notice an input-reload insn here, we will reject it below,
	     but it might hide a usable equivalent.  That makes bad code.
	     It may even abort: perhaps no reg was spilled for this insn
	     because it was assumed we would find that equivalent.  */
	      || INSN_UID (p) < reload_first_uid))
	{
	  rtx tem;
	  pat = single_set (p);
	  /* First check for something that sets some reg equal to GOAL.  */
	  if (pat != 0
	      && ((regno >= 0
		   && true_regnum (SET_SRC (pat)) == regno
		   && (valueno = true_regnum (valtry = SET_DEST (pat))) >= 0)
		  ||
		  (regno >= 0
		   && true_regnum (SET_DEST (pat)) == regno
		   && (valueno = true_regnum (valtry = SET_SRC (pat))) >= 0)
		  ||
		  (goal_const && rtx_equal_p (SET_SRC (pat), goal)
		   && (valueno = true_regnum (valtry = SET_DEST (pat))) >= 0)
		  || (goal_mem
		      && (valueno = true_regnum (valtry = SET_DEST (pat))) >= 0
		      && rtx_renumbered_equal_p (goal, SET_SRC (pat)))
		  || (goal_mem
		      && (valueno = true_regnum (valtry = SET_SRC (pat))) >= 0
		      && rtx_renumbered_equal_p (goal, SET_DEST (pat)))
		  /* If we are looking for a constant,
		     and something equivalent to that constant was copied
		     into a reg, we can use that reg.  */
		  || (goal_const && (tem = find_reg_note (p, REG_EQUIV,
							  NULL_RTX))
		      && rtx_equal_p (XEXP (tem, 0), goal)
		      && (valueno = true_regnum (valtry = SET_DEST (pat))) >= 0)
		  || (goal_const && (tem = find_reg_note (p, REG_EQUIV,
							  NULL_RTX))
		      && GET_CODE (SET_DEST (pat)) == REG
		      && GET_CODE (XEXP (tem, 0)) == CONST_DOUBLE
		      && GET_MODE_CLASS (GET_MODE (XEXP (tem, 0))) == MODE_FLOAT
		      && GET_CODE (goal) == CONST_INT
		      && 0 != (goaltry = operand_subword (XEXP (tem, 0), 0, 0,
							  VOIDmode))
		      && rtx_equal_p (goal, goaltry)
		      && (valtry = operand_subword (SET_DEST (pat), 0, 0,
						    VOIDmode))
		      && (valueno = true_regnum (valtry)) >= 0)
		  || (goal_const && (tem = find_reg_note (p, REG_EQUIV,
							  NULL_RTX))
		      && GET_CODE (SET_DEST (pat)) == REG
		      && GET_CODE (XEXP (tem, 0)) == CONST_DOUBLE
		      && GET_MODE_CLASS (GET_MODE (XEXP (tem, 0))) == MODE_FLOAT
		      && GET_CODE (goal) == CONST_INT
		      && 0 != (goaltry = operand_subword (XEXP (tem, 0), 1, 0,
							  VOIDmode))
		      && rtx_equal_p (goal, goaltry)
		      && (valtry
			  = operand_subword (SET_DEST (pat), 1, 0, VOIDmode))
		      && (valueno = true_regnum (valtry)) >= 0)))
	    if (other >= 0
		? valueno == other
		: ((unsigned) valueno < FIRST_PSEUDO_REGISTER
		   && TEST_HARD_REG_BIT (reg_class_contents[(int) class],
					 valueno)))
	      {
		value = valtry;
		where = p;
		break;
	      }
	}
    }

  /* We found a previous insn copying GOAL into a suitable other reg VALUE
     (or copying VALUE into GOAL, if GOAL is also a register).
     Now verify that VALUE is really valid.  */

  /* VALUENO is the register number of VALUE; a hard register.  */

  /* Don't try to re-use something that is killed in this insn.  We want
     to be able to trust REG_UNUSED notes.  */
  if (find_reg_note (where, REG_UNUSED, value))
    return 0;

  /* If we propose to get the value from the stack pointer or if GOAL is
     a MEM based on the stack pointer, we need a stable SP.  */
  if (valueno == STACK_POINTER_REGNUM
      || (goal_mem && reg_overlap_mentioned_for_reload_p (stack_pointer_rtx,
							  goal)))
    need_stable_sp = 1;

  /* Reject VALUE if the copy-insn moved the wrong sort of datum.  */
  if (GET_MODE (value) != mode)
    return 0;

  /* Reject VALUE if it was loaded from GOAL
     and is also a register that appears in the address of GOAL.  */

  if (goal_mem && value == SET_DEST (PATTERN (where))
      && refers_to_regno_for_reload_p (valueno,
				       (valueno
					+ HARD_REGNO_NREGS (valueno, mode)),
				       goal, NULL_PTR))
    return 0;

  /* Reject registers that overlap GOAL.  */

  if (!goal_mem && !goal_const
      && regno + HARD_REGNO_NREGS (regno, mode) > valueno
      && regno < valueno + HARD_REGNO_NREGS (valueno, mode))
    return 0;

  /* Reject VALUE if it is one of the regs reserved for reloads.
     Reload1 knows how to reuse them anyway, and it would get
     confused if we allocated one without its knowledge.
     (Now that insns introduced by reload are ignored above,
     this case shouldn't happen, but I'm not positive.)  */

  if (reload_reg_p != 0 && reload_reg_p != (short *) (HOST_WIDE_INT) 1
      && reload_reg_p[valueno] >= 0)
    return 0;

  /* On some machines, certain regs must always be rejected
     because they don't behave the way ordinary registers do.  */
  
#ifdef OVERLAPPING_REGNO_P
  if (OVERLAPPING_REGNO_P (valueno))
    return 0;
#endif      

  nregs = HARD_REGNO_NREGS (regno, mode);
  valuenregs = HARD_REGNO_NREGS (valueno, mode);

  /* Reject VALUE if it is a register being used for an input reload
     even if it is not one of those reserved.  */

  if (reload_reg_p != 0)
    {
      int i;
      for (i = 0; i < n_reloads; i++)
	if (reload_reg_rtx[i] != 0 && reload_in[i])
	  {
	    int regno1 = REGNO (reload_reg_rtx[i]);
	    int nregs1 = HARD_REGNO_NREGS (regno1,
					   GET_MODE (reload_reg_rtx[i]));
	    if (regno1 < valueno + valuenregs
		&& regno1 + nregs1 > valueno)
	      return 0;
	  }
    }

  if (goal_mem)
    /* We must treat frame pointer as varying here,
       since it can vary--in a nonlocal goto as generated by expand_goto.  */
    goal_mem_addr_varies = !CONSTANT_ADDRESS_P (XEXP (goal, 0));

  /* Now verify that the values of GOAL and VALUE remain unaltered
     until INSN is reached.  */

  p = insn;
  while (1)
    {
      p = PREV_INSN (p);
      if (p == where)
	return value;

      /* Don't trust the conversion past a function call
	 if either of the two is in a call-clobbered register, or memory.  */
      if (GET_CODE (p) == CALL_INSN
	  && ((regno >= 0 && regno < FIRST_PSEUDO_REGISTER
	       && call_used_regs[regno])
	      ||
	      (valueno >= 0 && valueno < FIRST_PSEUDO_REGISTER
	       && call_used_regs[valueno])
	      ||
	      goal_mem
	      || need_stable_sp))
	return 0;

#ifdef INSN_CLOBBERS_REGNO_P
      if ((valueno >= 0 && valueno < FIRST_PSEUDO_REGISTER
	  && INSN_CLOBBERS_REGNO_P (p, valueno))
	  || (regno >= 0 && regno < FIRST_PSEUDO_REGISTER
	  && INSN_CLOBBERS_REGNO_P (p, regno)))
	return 0;
#endif

      if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
	{
	  /* If this insn P stores in either GOAL or VALUE, return 0.
	     If GOAL is a memory ref and this insn writes memory, return 0.
	     If GOAL is a memory ref and its address is not constant,
	     and this insn P changes a register used in GOAL, return 0.  */

	  pat = PATTERN (p);
	  if (GET_CODE (pat) == SET || GET_CODE (pat) == CLOBBER)
	    {
	      register rtx dest = SET_DEST (pat);
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == REG)
		{
		  register int xregno = REGNO (dest);
		  int xnregs;
		  if (REGNO (dest) < FIRST_PSEUDO_REGISTER)
		    xnregs = HARD_REGNO_NREGS (xregno, GET_MODE (dest));
		  else
		    xnregs = 1;
		  if (xregno < regno + nregs && xregno + xnregs > regno)
		    return 0;
		  if (xregno < valueno + valuenregs
		      && xregno + xnregs > valueno)
		    return 0;
		  if (goal_mem_addr_varies
		      && reg_overlap_mentioned_for_reload_p (dest, goal))
		    return 0;
		}
	      else if (goal_mem && GET_CODE (dest) == MEM
		       && ! push_operand (dest, GET_MODE (dest)))
		return 0;
	      else if (need_stable_sp && push_operand (dest, GET_MODE (dest)))
		return 0;
	    }
	  else if (GET_CODE (pat) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
		{
		  register rtx v1 = XVECEXP (pat, 0, i);
		  if (GET_CODE (v1) == SET || GET_CODE (v1) == CLOBBER)
		    {
		      register rtx dest = SET_DEST (v1);
		      while (GET_CODE (dest) == SUBREG
			     || GET_CODE (dest) == ZERO_EXTRACT
			     || GET_CODE (dest) == SIGN_EXTRACT
			     || GET_CODE (dest) == STRICT_LOW_PART)
			dest = XEXP (dest, 0);
		      if (GET_CODE (dest) == REG)
			{
			  register int xregno = REGNO (dest);
			  int xnregs;
			  if (REGNO (dest) < FIRST_PSEUDO_REGISTER)
			    xnregs = HARD_REGNO_NREGS (xregno, GET_MODE (dest));
			  else
			    xnregs = 1;
			  if (xregno < regno + nregs
			      && xregno + xnregs > regno)
			    return 0;
			  if (xregno < valueno + valuenregs
			      && xregno + xnregs > valueno)
			    return 0;
			  if (goal_mem_addr_varies
			      && reg_overlap_mentioned_for_reload_p (dest,
								     goal))
			    return 0;
			}
		      else if (goal_mem && GET_CODE (dest) == MEM
			       && ! push_operand (dest, GET_MODE (dest)))
			return 0;
		      else if (need_stable_sp
			       && push_operand (dest, GET_MODE (dest)))
			return 0;
		    }
		}
	    }

#ifdef AUTO_INC_DEC
	  /* If this insn auto-increments or auto-decrements
	     either regno or valueno, return 0 now.
	     If GOAL is a memory ref and its address is not constant,
	     and this insn P increments a register used in GOAL, return 0.  */
	  {
	    register rtx link;

	    for (link = REG_NOTES (p); link; link = XEXP (link, 1))
	      if (REG_NOTE_KIND (link) == REG_INC
		  && GET_CODE (XEXP (link, 0)) == REG)
		{
		  register int incno = REGNO (XEXP (link, 0));
		  if (incno < regno + nregs && incno >= regno)
		    return 0;
		  if (incno < valueno + valuenregs && incno >= valueno)
		    return 0;
		  if (goal_mem_addr_varies
		      && reg_overlap_mentioned_for_reload_p (XEXP (link, 0),
							     goal))
		    return 0;
		}
	  }
#endif
	}
    }
}

/* Find a place where INCED appears in an increment or decrement operator
   within X, and return the amount INCED is incremented or decremented by.
   The value is always positive.  */

static int
find_inc_amount (x, inced)
     rtx x, inced;
{
  register enum rtx_code code = GET_CODE (x);
  register char *fmt;
  register int i;

  if (code == MEM)
    {
      register rtx addr = XEXP (x, 0);
      if ((GET_CODE (addr) == PRE_DEC
	   || GET_CODE (addr) == POST_DEC
	   || GET_CODE (addr) == PRE_INC
	   || GET_CODE (addr) == POST_INC)
	  && XEXP (addr, 0) == inced)
	return GET_MODE_SIZE (GET_MODE (x));
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  register int tem = find_inc_amount (XEXP (x, i), inced);
	  if (tem != 0)
	    return tem;
	}
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      register int tem = find_inc_amount (XVECEXP (x, i, j), inced);
	      if (tem != 0)
		return tem;
	    }
	}
    }

  return 0;
}

/* Return 1 if register REGNO is the subject of a clobber in insn INSN.  */

int
regno_clobbered_p (regno, insn)
     int regno;
     rtx insn;
{
  if (GET_CODE (PATTERN (insn)) == CLOBBER
      && GET_CODE (XEXP (PATTERN (insn), 0)) == REG)
    return REGNO (XEXP (PATTERN (insn), 0)) == regno;

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      int i = XVECLEN (PATTERN (insn), 0) - 1;

      for (; i >= 0; i--)
	{
	  rtx elt = XVECEXP (PATTERN (insn), 0, i);
	  if (GET_CODE (elt) == CLOBBER && GET_CODE (XEXP (elt, 0)) == REG
	      && REGNO (XEXP (elt, 0)) == regno)
	    return 1;
	}
    }

  return 0;
}

static char *reload_when_needed_name[] =
{
  "RELOAD_FOR_INPUT", 
  "RELOAD_FOR_OUTPUT", 
  "RELOAD_FOR_INSN",
  "RELOAD_FOR_INPUT_ADDRESS", 
  "RELOAD_FOR_OUTPUT_ADDRESS",
  "RELOAD_FOR_OPERAND_ADDRESS", 
  "RELOAD_FOR_OPADDR_ADDR",
  "RELOAD_OTHER", 
  "RELOAD_FOR_OTHER_ADDRESS"
};

static char *reg_class_names[] = REG_CLASS_NAMES;

/* This function is used to print the variables set by 'find_reloads' */

void
debug_reload()
{
  int r;

  fprintf (stderr, "\nn_reloads = %d\n", n_reloads);

  for (r = 0; r < n_reloads; r++)
    {
      fprintf (stderr, "\nRELOAD %d\n", r);

      if (reload_in[r])
	{
	  fprintf (stderr, "\nreload_in (%s) = ", mode_name[reload_inmode[r]]);
	  debug_rtx (reload_in[r]);
	}

      if (reload_out[r])
	{
	  fprintf (stderr, "\nreload_out (%s) = ", mode_name[reload_outmode[r]]);
	  debug_rtx (reload_out[r]);
	}

      fprintf (stderr, "%s, ", reg_class_names[(int) reload_reg_class[r]]);

      fprintf (stderr, "%s (opnum = %d)", reload_when_needed_name[(int)reload_when_needed[r]],
	       reload_opnum[r]);

      if (reload_optional[r])
	fprintf (stderr, ", optional");

      if (reload_in[r])
	fprintf (stderr, ", inc by %d\n", reload_inc[r]);

      if (reload_nocombine[r])
	fprintf (stderr, ", can combine", reload_nocombine[r]);

      if (reload_secondary_p[r])
	fprintf (stderr, ", secondary_reload_p");

      if (reload_in_reg[r])
	{
	  fprintf (stderr, "\nreload_in_reg:\t\t\t");
	  debug_rtx (reload_in_reg[r]);
	}

      if (reload_reg_rtx[r])
	{
	  fprintf (stderr, "\nreload_reg_rtx:\t\t\t");
	  debug_rtx (reload_reg_rtx[r]);
	}

      if (reload_secondary_in_reload[r] != -1)
	{
	  fprintf (stderr, "\nsecondary_in_reload = ");
	  fprintf (stderr, "%d ", reload_secondary_in_reload[r]);
	}

      if (reload_secondary_out_reload[r] != -1)
	{
	  if (reload_secondary_in_reload[r] != -1)
	    fprintf (stderr, ", secondary_out_reload = ");
	  else
	    fprintf (stderr, "\nsecondary_out_reload = ");

	  fprintf (stderr, "%d", reload_secondary_out_reload[r]);
	}


      if (reload_secondary_in_icode[r] != CODE_FOR_nothing)
	{
	  fprintf (stderr, "\nsecondary_in_icode = ");
	  fprintf (stderr, "%s", insn_name[r]);
	}

      if (reload_secondary_out_icode[r] != CODE_FOR_nothing)
	{
	  if (reload_secondary_in_icode[r] != CODE_FOR_nothing)
	    fprintf (stderr, ", secondary_out_icode = ");
	  else
	    fprintf (stderr, "\nsecondary_out_icode = ");

	  fprintf (stderr, "%s ", insn_name[r]);
	}
      fprintf (stderr, "\n");
    }

  fprintf (stderr, "\n");
}
