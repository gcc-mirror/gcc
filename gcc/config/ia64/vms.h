/* Definitions of target machine GNU compiler. IA64-VMS version.
   Copyright (C) 2003-2011 Free Software Foundation, Inc.
   Contributed by Douglas B Rupp (rupp@gnat.com).

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

#define OBJECT_FORMAT_ELF

#define SUBTARGET_OS_CPP_BUILTINS()		\
    do {					\
	builtin_define ("__IA64");		\
	builtin_define ("__IEEE_FLOAT");	\
    } while (0)

/* Need .debug_line info generated from gcc and gas.  */
#undef TARGET_DEFAULT
#if POINTER_SIZE == 64
#define TARGET_DEFAULT (MASK_DWARF2_ASM | MASK_GNU_AS | MASK_MALLOC64)
#else
#define TARGET_DEFAULT (MASK_DWARF2_ASM | MASK_GNU_AS)
#endif

#define VMS_DEBUG_MAIN_POINTER "TRANSFER$BREAK$GO"

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 524288  /* 8 x 2^16 by DEC Ada Test CD40VRA */

/* Widest floating-point type efficiently supported by hardware and OS.  */
#undef WIDEST_HARDWARE_FP_SIZE
#define WIDEST_HARDWARE_FP_SIZE 64

/* The structure return address arrives as an "argument" on VMS.  */
#undef PCC_STATIC_STRUCT_RETURN

/* Turn on VMS specific Dwarf2 features.  */
#define VMS_DEBUGGING_INFO 1

#define ASM_OUTPUT_DWARF_VMS_DELTA(FILE,SIZE,LABEL1,LABEL2) \
do {                                          \
  fprintf (FILE, "\tdata4.ua\t@slotcount(");  \
  assemble_name (FILE, LABEL1);               \
  fprintf (FILE, "-");                        \
  assemble_name (FILE, LABEL2);               \
  fprintf (FILE, ")");                        \
} while (0)

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
"%{!shared:%{mvms-return-codes:vcrt0.o%s} %{!mvms-return-codes:pcrt0.o%s} \
    crtbegin.o%s} \
 %{!static:%{shared:crtinitS.o%s crtbeginS.o%s}}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
"%{!shared:crtend.o%s} %{!static:%{shared:crtendS.o%s}}"

#define LINK_GCC_C_SEQUENCE_SPEC "%G"

#undef LINK_SPEC
#define LINK_SPEC "%{g*} %{map} %{save-temps} %{shared} %{v}"

#undef LIB_SPEC
#define LIB_SPEC ""

#undef ASM_SPEC
#define ASM_SPEC \
"%{mno-gnu-as:-N so -N vms_upcase -W DVLoc_off} %{mconstant-gp:-M const_gp} \
 %{mauto-pic:-M no_plabel} %{source-listing:-ahdl=%b.lis}"

#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)			\
do {								\
  (*targetm.asm_out.globalize_label) (FILE, XSTR (FUN, 0));	\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, XSTR (FUN, 0), "function");	\
} while (0)

/* Set the function to change the names of the division and modulus
   functions.   */
#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS ia64_vms_init_libfuncs

#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

#define CTOR_LIST_BEGIN asm (".global\tLIB$INITIALIZE#\n");                  \
STATIC func_ptr __CTOR_LIST__[1]                                             \
  __attribute__ ((__unused__, section(".ctors"), aligned(sizeof(func_ptr)))) \
  = { (func_ptr) (-1) };

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP ".section\tLIB$INITIALIZE#,\"a\",@progbits"

#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)      \
  asm (SECTION_OP "\n\tdata4 @fptr(" #FUNC"#)\n");      \
  FORCE_CODE_SECTION_ALIGN                            \
  asm (TEXT_SECTION_ASM_OP);

#undef FINI_SECTION_ASM_OP

/* Maybe same as HPUX?  Needs to be checked.  */
#define JMP_BUF_SIZE  (8 * 76)

#undef SUBTARGET_OPTIMIZATION_OPTIONS
#define SUBTARGET_OPTIMIZATION_OPTIONS			\
  { OPT_LEVELS_ALL, OPT_fmerge_constants, NULL, 0 }

/* Define this to be nonzero if static stack checking is supported.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* Minimum amount of stack required to recover from an anticipated stack
   overflow detection.  The default value conveys an estimate of the amount
   of stack required to propagate an exception.  */
#define STACK_CHECK_PROTECT (24 * 1024)

#undef ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN) \
  ia64_vms_output_aligned_decl_common (FILE, DECL, NAME, SIZE, ALIGN)

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE ia64_vms_valid_pointer_mode

#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION ia64_vms_elf_asm_named_section
