/* Definitions for Unix assembler syntax for the Intel 80386.
   Copyright (C) 1988, 1994, 1999 Free Software Foundation, Inc.

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

/* This file defines the aspects of assembler syntax
   that are the same for all the i386 Unix systems
   (though they may differ in non-Unix systems).  */

/* Define some concatenation macros to concatenate an opcode
   and one, two or three operands.  In other assembler syntaxes
   they may alter the order of ther operands.  */

/* Note that the other files fail to use these
   in some of the places where they should.  */

#if defined(__STDC__) || defined(ALMOST_STDC)
#define AS2(a,b,c) #a " " #b "," #c
#define AS2C(b,c) " " #b "," #c
#define AS3(a,b,c,d) #a " " #b "," #c "," #d
#define AS1(a,b) #a " " #b
#else
#define AS1(a,b) "a b"
#define AS2(a,b,c) "a b,c"
#define AS2C(b,c) " b,c"
#define AS3(a,b,c,d) "a b,c,d"
#endif  

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.  This macro controls what to do: by default, don't
   print %cl.  */
#define SHIFT_DOUBLE_OMITS_COUNT 1
#define AS3_SHIFT_DOUBLE(a,b,c,d) \
	(SHIFT_DOUBLE_OMITS_COUNT ? AS2 (a,c,d) : AS3 (a,b,c,d))

/* Output the size-letter for an opcode.
   CODE is the letter used in an operand spec (L, B, W, S or Q).
   CH is the corresponding lower case letter
     (except if CODE is `Q' then CH is `l', unless GAS_MNEMONICS).  */
#define PUT_OP_SIZE(CODE,CH,FILE) putc (CH,(FILE))

/* Opcode suffix for fullword insn.  */
#define L_SIZE "l"

/* Prefix for register names in this syntax.  */
#define RP "%"

/* Prefix for immediate operands in this syntax.  */
#define IP "$"

/* Indirect call instructions should use `*'.  */
#define USE_STAR 1

/* Prefix for a memory-operand X.  */
#define PRINT_PTR(X, FILE)

/* Delimiters that surround base reg and index reg.  */
#define ADDR_BEG(FILE) putc('(', (FILE))
#define ADDR_END(FILE) putc(')', (FILE))

/* Print an index register (whose rtx is IREG).  */
#define PRINT_IREG(FILE,IREG) \
  do								\
  { fputs (",", (FILE)); PRINT_REG ((IREG), 0, (FILE)); }	\
  while (0)
  
/* Print an index scale factor SCALE.  */
#define PRINT_SCALE(FILE,SCALE) \
  if ((SCALE) != 1) fprintf ((FILE), ",%d", (SCALE))

/* Print a base/index combination.
   BREG is the base reg rtx, IREG is the index reg rtx,
   and SCALE is the index scale factor (an integer).  */

#define PRINT_B_I_S(BREG,IREG,SCALE,FILE) \
  { ADDR_BEG (FILE); 				\
    if (BREG) PRINT_REG ((BREG), 0, (FILE));	\
    if ((IREG) != 0)				\
      { PRINT_IREG ((FILE), (IREG));		\
        PRINT_SCALE ((FILE), (SCALE)); }	\
    ADDR_END (FILE); }

/* Define the syntax of pseudo-ops, labels and comments.  */

/* String containing the assembler's comment-starter.  */

#define ASM_COMMENT_START "/"

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "/APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "/NO_APP\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable (initialized) data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Output before writable (uninitialized) data.  */

#define BSS_SECTION_ASM_OP ".bss"

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  (fputs (".globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387.  */

#define TARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS)

/* Floating-point return values come in the FP register.  */

#define VALUE_REGNO(MODE) \
  (GET_MODE_CLASS (MODE) == MODE_FLOAT				\
   && TARGET_FLOAT_RETURNS_IN_80387 ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 0 || ((N)== FIRST_FLOAT_REG && TARGET_FLOAT_RETURNS_IN_80387))

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	      \
do {									      \
  tree parm;								      \
									      \
  if (i386_regparm > 0)							      \
    parm = TYPE_ARG_TYPES (TREE_TYPE (function));			      \
  else									      \
    parm = NULL_TREE;							      \
  for (; parm; parm = TREE_CHAIN (parm))				      \
    if (TREE_VALUE (parm) == void_type_node)				      \
      break;								      \
  fprintf (FILE, "\taddl $%d,%s\n", DELTA,				      \
	   parm ? "%eax"						      \
	   : aggregate_value_p (TREE_TYPE (TREE_TYPE (FUNCTION))) ? "8(%esp)" \
	   : "4(%esp)");						      \
									      \
  if (flag_pic)								      \
    {									      \
      rtx xops[2];							      \
      xops[0] = pic_offset_table_rtx;					      \
      xops[1] = (rtx) gen_label_rtx ();					      \
									      \
      if (i386_regparm > 2)						      \
	abort ();							      \
      output_asm_insn ("push%L0 %0", xops);				      \
      output_asm_insn (AS1 (call,%P1), xops);				      \
      ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", CODE_LABEL_NUMBER (xops[1]));     \
      output_asm_insn (AS1 (pop%L0,%0), xops);				      \
      output_asm_insn ("addl $%__GLOBAL_OFFSET_TABLE_+[.-%P1],%0", xops);     \
      fprintf (FILE, "\tmovl ");					      \
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	      \
      fprintf (FILE, "@GOT(%%ebx),%%ecx\n\tpopl %%ebx\n\tjmp *%%ecx\n");      \
    }									      \
  else									      \
    {									      \
      fprintf (FILE, "\tjmp ");						      \
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	      \
      fprintf (FILE, "\n");						      \
    }									      \
} while (0)
