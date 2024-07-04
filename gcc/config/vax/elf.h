/* Target definitions for GNU compiler for VAX using ELF
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
   Contributed by Matt Thomas <matt@3am-software.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#undef TARGET_ELF
#define TARGET_ELF 1

#undef REGISTER_PREFIX
#undef REGISTER_NAMES
#define REGISTER_PREFIX "%"
#define REGISTER_NAMES \
  { "%r0", "%r1",  "%r2",  "%r3", "%r4", "%r5", "%r6", "%r7", \
    "%r8", "%r9", "%r10", "%r11", "%ap", "%fp", "%sp", "%pc", \
    "%psl" }

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

/* Profiling routine.  */
#undef VAX_FUNCTION_PROFILER_NAME
#define VAX_FUNCTION_PROFILER_NAME "__mcount"

/*  Let's be re-entrant.  */
#undef PCC_STATIC_STRUCT_RETURN

/* Before the prologue, the top of the frame is below the argument
   count pushed by the CALLS and before the start of the saved registers.  */
#define INCOMING_FRAME_SP_OFFSET 0

/* Offset from the frame pointer register value to the top of the stack.  */
#define FRAME_POINTER_CFA_OFFSET(FNDECL) 0

/* We use R2-R5 (call-clobbered) registers for exceptions.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 2 : INVALID_REGNUM)

/* Place the top of the stack for the DWARF2 EH stackadj value.  */
#define EH_RETURN_STACKADJ_RTX						\
  gen_rtx_MEM (SImode,							\
	       plus_constant (Pmode,					\
			      gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM),\
			      -4))

/* Simple store the return handler into the call frame.  */
#define EH_RETURN_HANDLER_RTX						\
  gen_rtx_MEM (Pmode,							\
	       plus_constant (Pmode,					\
			      gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM),\
			      16))


/* The VAX wants no space between the case instruction and the jump table.  */
#undef  ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE, PREFIX, NUM, TABLE)

#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS			\
  do							\
    {							\
      /* Turn off function CSE if we're doing PIC.  */	\
      if (flag_pic)					\
	flag_no_function_cse = 1;			\
    }							\
  while (0)

/* Don't allow *foo which foo is non-local */
#define NO_EXTERNAL_INDIRECT_ADDRESS

#undef VAX_CC1_AND_CC1PLUS_SPEC
#define VAX_CC1_AND_CC1PLUS_SPEC \
  "%{!fno-pic: \
     %{!fpic: \
       %{!fPIC:-fPIC}}}"

/* Don't let the LTO compiler switch the PIC options off.  */
#define VAX_CC1_SPEC \
  VAX_CC1_AND_CC1PLUS_SPEC \
  " %{flinker-output=exec" \
  ":%{no-pie:-flinker-output=exec;:-flinker-output=pie};" \
  ":%{flinker-output=*}}" \
  "%<flinker-output*"
#define VAX_CC1PLUS_SPEC \
  VAX_CC1_AND_CC1PLUS_SPEC

/* VAX ELF is always gas; override the generic VAX ASM_SPEC.  */

#undef ASM_SPEC
#define ASM_SPEC "%{!fno-pic: %{!mno-asm-pic:-k}}"

/*  We want PCREL dwarf output.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)	\
  ((GLOBAL ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

/* Emit a PC-relative relocation.  */
#define ASM_OUTPUT_DWARF_PCREL(FILE, SIZE, LABEL)	\
  do {							\
    fputs (integer_asm_op (SIZE, FALSE), FILE);		\
    fprintf (FILE, "%%pcrel%d(", SIZE * 8);		\
    assemble_name (FILE, LABEL);			\
    fputc (')', FILE);					\
  } while (0)
