/* Definitions for Unix assembler syntax for the Intel 80386.
   Copyright (C) 1988, 1994, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#define DEFAULT_ASSEMBLER_DIALECT 0

/* Define macro used to output shift-double opcodes when the shift
   count is in %cl.  Some assemblers require %cl as an argument;
   some don't.  This macro controls what to do: by default, don't
   print %cl.  */
#define SHIFT_DOUBLE_OMITS_COUNT 1

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

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable (initialized) data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* Output before writable (uninitialized) data.  */

#define BSS_SECTION_ASM_OP "\t.bss"

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  (fputs (".globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387.  */

#define TARGET_SUBTARGET_DEFAULT (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS)

/* Floating-point return values come in the FP register.  */

#define VALUE_REGNO(MODE) \
  (GET_MODE_CLASS (MODE) == MODE_FLOAT				\
   && TARGET_FLOAT_RETURNS_IN_80387 ? FIRST_FLOAT_REG		\
   : (MODE) == TImode || VECTOR_MODE_P (MODE) ? FIRST_SSE_REG	\
   : 0)

/* Output code to add DELTA to the first argument, and then jump to FUNCTION.
   Used for C++ multiple inheritance.  */
#define ASM_OUTPUT_MI_THUNK(FILE, THUNK_FNDECL, DELTA, FUNCTION)	    \
do {									    \
  tree parm;								    \
  rtx xops[3];								    \
									    \
  if (ix86_regparm > 0)							    \
    parm = TYPE_ARG_TYPES (TREE_TYPE (function));			    \
  else									    \
    parm = NULL_TREE;							    \
  for (; parm; parm = TREE_CHAIN (parm))				    \
    if (TREE_VALUE (parm) == void_type_node)				    \
      break;								    \
									    \
  xops[0] = GEN_INT (DELTA);						    \
  if (parm)								    \
    xops[1] = gen_rtx_REG (SImode, 0);					    \
  else if (aggregate_value_p (TREE_TYPE (TREE_TYPE (FUNCTION))))	    \
    xops[1] = gen_rtx_MEM (SImode, plus_constant (stack_pointer_rtx, 8));   \
  else									    \
    xops[1] = gen_rtx_MEM (SImode, plus_constant (stack_pointer_rtx, 4));   \
  output_asm_insn ("add{l} {%0, %1|%1, %0}", xops);			    \
									    \
  if (flag_pic && !TARGET_64BIT)					    \
    {									    \
      xops[0] = pic_offset_table_rtx;					    \
      xops[1] = gen_label_rtx ();					    \
      xops[2] = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");        \
									    \
      if (ix86_regparm > 2)						    \
	abort ();							    \
      output_asm_insn ("push{l}\t%0", xops);				    \
      output_asm_insn ("call\t%P1", xops);				    \
      ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", CODE_LABEL_NUMBER (xops[1]));   \
      output_asm_insn ("pop{l}\t%0", xops);				    \
      output_asm_insn ("add{l}\t{%2+[.-%P1], %0|%0, OFFSET FLAT: %2+[.-%P1]}", xops); \
      xops[0] = gen_rtx_MEM (SImode, XEXP (DECL_RTL (FUNCTION), 0));	    \
      output_asm_insn ("mov{l}\t{%0@GOT(%%ebx), %%ecx|%%ecx, %0@GOT[%%ebx]}",\
	               xops);						    \
      asm_fprintf (FILE, "\tpop{l\t%%ebx|\t%%ebx}\n");			    \
      asm_fprintf (FILE, "\tjmp\t{*%%ecx|%%ecx}\n");			    \
    }									    \
  else if (flag_pic && TARGET_64BIT)					    \
    {									    \
      fprintf (FILE, "\tjmp *");					    \
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	    \
      fprintf (FILE, "@GOTPCREL(%%rip)\n");				    \
    }									    \
  else									    \
    {									    \
      fprintf (FILE, "\tjmp ");						    \
      assemble_name (FILE, XSTR (XEXP (DECL_RTL (FUNCTION), 0), 0));	    \
      fprintf (FILE, "\n");						    \
    }									    \
} while (0)
