/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

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

#define OPEN_VMS 1

/* This enables certain macros in alpha.h, which will make an indirect
   reference to an external symbol an invalid address.  This needs to be
   defined before we include alpha.h, since it determines which macros
   are used for GO_IF_*.  */

#define NO_EXTERNAL_INDIRECT_ADDRESS

#include "alpha/alpha.h"

/* Predefine this in CPP because VMS limits the size of command options
   and GNU CPP is not used on VMS except with GNU C.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
"-Dalpha -D__ALPHA -Dvms -DVMS -D__alpha__ -D__alpha -D__vms__ -D__VMS__\
 -D__VMS_VER=70000022 \
 -D__GNUC__=2 -D__GNUC_MINOR__=7 -Asystem(vms) -Acpu(alpha) -Amachine(alpha)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{mfloat-ieee:-D__IEEE_FLOAT} \
%{mfloat-vax:-D__G_FLOAT} \
%{!mfloat-vax:-D__IEEE_FLOAT} \
%{!.S:	-D__LANGUAGE_C__ -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}  \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C}"

/* We allow $'s in identifiers unless -ansi is used .. */

#define DOLLARS_IN_IDENTIFIERS 2

/* These match the definitions used in DECCRTL, the VMS C run-time library

#define SIZE_TYPE	"unsigned int"
#define PTRDIFF_TYPE	"int"
*/

/* Use memcpy for structure copying, and so forth.  */
#define TARGET_MEM_FUNCTIONS

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 2

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_FP|MASK_FPREGS|MASK_GAS|MASK_OPEN_VMS)
#undef TARGET_NAME   
#define TARGET_NAME "OpenVMS/Alpha"
#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (%s)", TARGET_NAME);           

/* The structure return address arrives as an "argument" on VMS.  */
#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE 0
#undef PCC_STATIC_STRUCT_RETURN

/* no floating emulation.  */
#undef REAL_ARITHMETIC

/* "long" is 32 bits.  */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

/* Pointer is 32 bits but the hardware has 64-bit addresses, sign extended. */
#undef POINTER_SIZE
#define POINTER_SIZE 32
#define POINTERS_EXTEND_UNSIGNED 0

/* No data type wants to be aligned rounder than this. */
#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 128       /* X Complex */
#define MAX_OFILE_ALIGNMENT 524288  /* 8 x 2^16 by DEC Test CD40VRA */

#undef FIXED_REGISTERS
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 }

#undef CALL_USED_REGISTERS
#define CALL_USED_REGISTERS  \
 {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 }

#undef HARD_FRAME_POINTER_REGNUM
#define HARD_FRAME_POINTER_REGNUM 29

#undef CAN_ELIMINATE
#define CAN_ELIMINATE(FROM, TO)  \
((TO) != STACK_POINTER_REGNUM || ! alpha_using_fp ())

#undef INITIAL_ELIMINATION_OFFSET
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
{ if ((FROM) == FRAME_POINTER_REGNUM)					\
    (OFFSET) = alpha_sa_size () + alpha_pv_save_size ();		\
  else if ((FROM) == ARG_POINTER_REGNUM)				\
    (OFFSET) = (ALPHA_ROUND (alpha_sa_size () + alpha_pv_save_size ()	\
			     + get_frame_size ()			\
			     + current_function_pretend_args_size)	\
		- current_function_pretend_args_size);			\
  if ((TO) == STACK_POINTER_REGNUM)					\
    (OFFSET) += ALPHA_ROUND (current_function_outgoing_args_size);	\
}

#undef FUNCTION_ARG_ADVANCE
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  if (MUST_PASS_IN_STACK (MODE, TYPE))					\
    (CUM) = (CUM & ~0xff) + 6;						\
  else									\
    (CUM) += ALPHA_ARG_SIZE (MODE, TYPE, NAMED)

#undef FUNCTION_ARG_PARTIAL_NREGS
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED)		\
((CUM & 0xff) < 6 && 6 < (CUM & 0xff)					\
   + ALPHA_ARG_SIZE (MODE, TYPE, NAMED)					\
 ? 6 - (CUM & 0xff) : 0)

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments. 

   CUM is as for INIT_CUMULATIVE_ARGS.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed. 

   For VMS, we allocate space for all 6 arg registers plus a count.

   However, if NO registers need to be saved, don't allocate any space.
   This is not only because we won't need the space, but because AP includes
   the current_pretend_args_size and we don't want to mess up any
   ap-relative addresses already made.   */

#undef SETUP_INCOMING_VARARGS
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM) < 6)					\
    {							\
      if (! (NO_RTL))					\
	{						\
	  emit_move_insn (gen_rtx (REG, DImode, 1),	\
			  virtual_incoming_args_rtx);	\
	  emit_insn (gen_arg_home ());			\
	}						\
						        \
      PRETEND_SIZE = 7 * UNITS_PER_WORD;		\
    }							\
}

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{								\
  alpha_write_verstamp (FILE);					\
  fprintf (FILE, "\t.set noreorder\n");				\
  fprintf (FILE, "\t.set volatile\n");				\
  ASM_OUTPUT_SOURCE_FILENAME (FILE, main_input_filename);	\
}

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  {									\
    if (REAL_VALUE_ISINF (VALUE)					\
        || REAL_VALUE_ISNAN (VALUE)					\
	|| REAL_VALUE_MINUS_ZERO (VALUE))				\
      {									\
	long t;								\
	REAL_VALUE_TO_TARGET_SINGLE ((VALUE), t);			\
	fprintf (FILE, "\t.long 0x%lx\n", t & 0xffffffff);		\
      }									\
    else								\
      {									\
	char str[30];							\
	REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", str);			\
	fprintf (FILE, "\t.%c_floating %s\n", (TARGET_FLOAT_VAX)?'f':'s', str);	\
      }									\
  }

#define LINK_SECTION_ASM_OP ".link"

#define READONLY_SECTION_ASM_OP ".rdata"

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS	in_link, in_rdata

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
void								\
readonly_section ()						\
{								\
  if (in_section != in_rdata)				\
    {								\
      fprintf (asm_out_file, "%s\n", READONLY_SECTION_ASM_OP);	\
      in_section = in_rdata;				\
    }								\
}								\
void								\
link_section ()							\
{								\
  if (in_section != in_link)					\
    {								\
      fprintf (asm_out_file, "%s\n", LINK_SECTION_ASM_OP); 	\
      in_section = in_link;					\
    }								\
}

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) abort ()

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE) \
  fprintf (FILE, "\t.quad $L%d\n", (VALUE))

#undef READONLY_DATA_SECTION
#define READONLY_DATA_SECTION readonly_section

#define ASM_FILE_END(FILE) alpha_write_linkage (FILE);

#undef FUNCTION_ARG
void *function_arg ();
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)	function_arg (&CUM, MODE, TYPE, NAMED)

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE DImode
#undef CASE_VECTOR_PC_RELATIVE

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 3); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

#define NO_MD_PROTOTYPES

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.

   The trampoline should set the static chain pointer to value placed
   into the trampoline and should branch to the specified routine.  
   Note that $27 has been set to the address of the trampoline, so we can
   use it for addressability of the two data items.  Trampolines are always
   aligned to FUNCTION_BOUNDARY, which is 64 bits.  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)		\
{						\
  fprintf (FILE, "\t.quad 0\n");		\
  fprintf (FILE, "\t.linkage __tramp\n");	\
}

/* Length in units of the trampoline for entering a nested function.  */

#undef TRAMPOLINE_SIZE
#define TRAMPOLINE_SIZE    24

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

#undef INITIALIZE_TRAMPOLINE
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx (MEM, Pmode, (TRAMP)), (FNADDR));		\
  emit_move_insn (gen_rtx (MEM, Pmode,					\
			   memory_address (Pmode,			\
					   plus_constant ((TRAMP), 16))), \
		  (CXT));						\
}

#undef TRANSFER_FROM_TRAMPOLINE

#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO

#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO
#endif

#define DWARF2_DEBUGGING_INFO

#ifdef PREFERRED_DEBUGGING_TYPE
#undef PREFERRED_DEBUGGING_TYPE
#endif
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#ifdef ASM_FORMAT_PRIVATE_NAME
#undef ASM_FORMAT_PRIVATE_NAME
#endif
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

#undef ASM_SPEC
#define ASM_SPEC "-nocpp %{pg}"

#undef ASM_FINAL_SPEC

#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-lgcc2 -lgcclib"

#define OPTIMIZATION_OPTIONS                       \
{                                                  \
   write_symbols = PREFERRED_DEBUGGING_TYPE;       \
   debug_info_level = (enum debug_info_level) 2;   \
}

#undef OVERRIDE_OPTIONS
#define OVERRIDE_OPTIONS                           \
{                                                  \
   if (write_symbols == NO_DEBUG)                  \
     debug_info_level = (enum debug_info_level) 0; \
   override_options ();                            \
}

#undef LINK_SPEC
#define LINK_SPEC "%{g3:-g3} %{g0:-g0}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "gnu:[000000]crt0.obj"

/* Define the names of the division and modulus functions.  */
#define DIVSI3_LIBCALL "OTS$DIV_I"
#define DIVDI3_LIBCALL "OTS$DIV_L"
#define UDIVSI3_LIBCALL "OTS$DIV_UI"
#define UDIVDI3_LIBCALL "OTS$DIV_UL"
#define MODSI3_LIBCALL "OTS$REM_I"
#define MODDI3_LIBCALL "OTS$REM_L"
#define UMODSI3_LIBCALL "OTS$REM_UI"
#define UMODDI3_LIBCALL "OTS$REM_UL"
