/* Communication between reload.c and reload1.c.
   Copyright (C) 1987, 1991, 1992, 1993, 1994 Free Software Foundation, Inc.

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


/* If secondary reloads are the same for inputs and outputs, define those
   macros here.  */

#ifdef SECONDARY_RELOAD_CLASS
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
  SECONDARY_RELOAD_CLASS (CLASS, MODE, X)
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
  SECONDARY_RELOAD_CLASS (CLASS, MODE, X)
#endif

/* If either macro is defined, show that we need secondary reloads.  */
#if defined(SECONDARY_INPUT_RELOAD_CLASS) || defined(SECONDARY_OUTPUT_RELOAD_CLASS)
#define HAVE_SECONDARY_RELOADS
#endif

/* See reload.c and reload1.c for comments on these variables.  */

/* Maximum number of reloads we can need.  */
#define MAX_RELOADS (2 * MAX_RECOG_OPERANDS * (MAX_REGS_PER_ADDRESS + 1))

extern rtx reload_in[MAX_RELOADS];
extern rtx reload_out[MAX_RELOADS];
extern rtx reload_in_reg[MAX_RELOADS];
extern enum reg_class reload_reg_class[MAX_RELOADS];
extern enum machine_mode reload_inmode[MAX_RELOADS];
extern enum machine_mode reload_outmode[MAX_RELOADS];
extern char reload_optional[MAX_RELOADS];
extern int reload_inc[MAX_RELOADS];
extern int reload_opnum[MAX_RELOADS];
extern int reload_secondary_p[MAX_RELOADS];
extern int reload_secondary_in_reload[MAX_RELOADS];
extern int reload_secondary_out_reload[MAX_RELOADS];
#ifdef MAX_INSN_CODE
extern enum insn_code reload_secondary_in_icode[MAX_RELOADS];
extern enum insn_code reload_secondary_out_icode[MAX_RELOADS];
#endif
extern int n_reloads;

extern rtx reload_reg_rtx[MAX_RELOADS];

/* Encode the usage of a reload.  The following codes are supported:

   RELOAD_FOR_INPUT		reload of an input operand
   RELOAD_FOR_OUTPUT		likewise, for output
   RELOAD_FOR_INSN		a reload that must not conflict with anything
				used in the insn, but may conflict with
				something used before or after the insn
   RELOAD_FOR_INPUT_ADDRESS	reload for parts of the address of an object
				that is an input reload
   RELOAD_FOR_OUTPUT_ADDRESS	likewise, for output reload
   RELOAD_FOR_OPERAND_ADDRESS	reload for the address of a non-reloaded
				operand; these don't conflict with
				any other addresses.
   RELOAD_FOR_OPADDR_ADDR	reload needed for RELOAD_FOR_OPERAND_ADDRESS
                                reloads; usually secondary reloads
   RELOAD_OTHER			none of the above, usually multiple uses
   RELOAD_FOR_OTHER_ADDRESS     reload for part of the address of an input
   				that is marked RELOAD_OTHER.

   This used to be "enum reload_when_needed" but some debuggers have trouble
   with an enum tag and variable of the same name.  */

enum reload_type
{
  RELOAD_FOR_INPUT, RELOAD_FOR_OUTPUT, RELOAD_FOR_INSN, 
  RELOAD_FOR_INPUT_ADDRESS, RELOAD_FOR_OUTPUT_ADDRESS,
  RELOAD_FOR_OPERAND_ADDRESS, RELOAD_FOR_OPADDR_ADDR,
  RELOAD_OTHER, RELOAD_FOR_OTHER_ADDRESS
};

extern enum reload_type reload_when_needed[MAX_RELOADS];

extern rtx *reg_equiv_constant;
extern rtx *reg_equiv_memory_loc;
extern rtx *reg_equiv_address;
extern rtx *reg_equiv_mem;

/* All the "earlyclobber" operands of the current insn
   are recorded here.  */
extern int n_earlyclobbers;
extern rtx reload_earlyclobbers[MAX_RECOG_OPERANDS];

/* Save the number of operands.  */
extern int reload_n_operands;

/* First uid used by insns created by reload in this function.
   Used in find_equiv_reg.  */
extern int reload_first_uid;

/* Nonzero if indirect addressing is supported when the innermost MEM is
   of the form (MEM (SYMBOL_REF sym)).  It is assumed that the level to
   which these are valid is the same as spill_indirect_levels, above.   */

extern char indirect_symref_ok;

/* Nonzero if an address (plus (reg frame_pointer) (reg ...)) is valid.  */
extern char double_reg_address_ok;

#ifdef MAX_INSN_CODE
/* These arrays record the insn_code of insns that may be needed to
   perform input and output reloads of special objects.  They provide a
   place to pass a scratch register.  */
extern enum insn_code reload_in_optab[];
extern enum insn_code reload_out_optab[];
#endif

/* Functions from reload.c:  */

/* Return a memory location that will be used to copy X in mode MODE.  
   If we haven't already made a location for this mode in this insn,
   call find_reloads_address on the location being returned.  */
extern rtx get_secondary_mem PROTO((rtx, enum machine_mode,
				    int, enum reload_type));

/* Clear any secondary memory locations we've made.  */
extern void clear_secondary_mem PROTO((void));

/* Transfer all replacements that used to be in reload FROM to be in
   reload TO.  */
extern void transfer_replacements PROTO((int, int));

/* Return 1 if ADDR is a valid memory address for mode MODE,
   and check that each pseudo reg has the proper kind of
   hard reg.  */
extern int strict_memory_address_p PROTO((enum machine_mode, rtx));

/* Like rtx_equal_p except that it allows a REG and a SUBREG to match
   if they are the same hard reg, and has special hacks for
   autoincrement and autodecrement.  */
extern int operands_match_p PROTO((rtx, rtx));

/* Return the number of times character C occurs in string S.  */
extern int n_occurrences PROTO((int, char *));

/* Return 1 if altering OP will not modify the value of CLOBBER. */
extern int safe_from_earlyclobber PROTO((rtx, rtx));

/* Search the body of INSN for values that need reloading and record them
   with push_reload.  REPLACE nonzero means record also where the values occur
   so that subst_reloads can be used.  */
extern void find_reloads PROTO((rtx, int, int, int, short *));

/* Compute the sum of X and Y, making canonicalizations assumed in an
   address, namely: sum constant integers, surround the sum of two
   constants with a CONST, put the constant as the second operand, and
   group the constant on the outermost sum.  */
extern rtx form_sum PROTO((rtx, rtx));

/* Substitute into the current INSN the registers into which we have reloaded
   the things that need reloading.  */
extern void subst_reloads PROTO((void));

/* Make a copy of any replacements being done into X and move those copies
   to locations in Y, a copy of X.  We only look at the highest level of
   the RTL.  */
extern void copy_replacements PROTO((rtx, rtx));

/* If LOC was scheduled to be replaced by something, return the replacement.
   Otherwise, return *LOC.  */
extern rtx find_replacement PROTO((rtx *));

/* Return nonzero if register in range [REGNO, ENDREGNO)
   appears either explicitly or implicitly in X
   other than being stored into.  */
extern int refers_to_regno_for_reload_p PROTO((int, int, rtx, rtx *));

/* Nonzero if modifying X will affect IN.  */
extern int reg_overlap_mentioned_for_reload_p PROTO((rtx, rtx));

/* Return nonzero if anything in X contains a MEM.  Look also for pseudo
   registers.  */
extern int refers_to_mem_for_reload_p PROTO((rtx));

/* Check the insns before INSN to see if there is a suitable register
   containing the same value as GOAL.  */
extern rtx find_equiv_reg PROTO((rtx, rtx, enum reg_class, int, short *,
				 int, enum machine_mode));

/* Return 1 if register REGNO is the subject of a clobber in insn INSN.  */
extern int regno_clobbered_p PROTO((int, rtx));


/* Functions in reload1.c:  */

/* Initialize the reload pass once per compilation.  */
extern void init_reload PROTO((void));

/* The reload pass itself.  */
extern int reload STDIO_PROTO((rtx, int, FILE *));

/* Mark the slots in regs_ever_live for the hard regs
   used by pseudo-reg number REGNO.  */
extern void mark_home_live PROTO((int));

/* Scan X and replace any eliminable registers (such as fp) with a
   replacement (such as sp), plus an offset.  */
extern rtx eliminate_regs PROTO((rtx, enum machine_mode, rtx));

/* Emit code to perform a reload from IN (which may be a reload register) to
   OUT (which may also be a reload register).  IN or OUT is from operand
   OPNUM with reload type TYPE.  */
extern rtx gen_reload PROTO((rtx, rtx, int, enum reload_type));

/* Functions in caller-save.c:  */

/* Initialize for caller-save.  */
extern void init_caller_save PROTO((void));

/* Initialize save areas by showing that we haven't allocated any yet.  */
extern void init_save_areas PROTO((void));

/* Allocate save areas for any hard registers that might need saving.  */
extern int setup_save_areas PROTO((int *));

/* Find the places where hard regs are live across calls and save them.  */
extern void save_call_clobbered_regs PROTO((enum machine_mode));
