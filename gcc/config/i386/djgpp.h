/* Configuration for an i386 running MS-DOS with DJGPP.
   Copyright (C) 1997-2024 Free Software Foundation, Inc.

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

/* Support generation of DWARF2 debugging info.  */
#define DWARF2_DEBUGGING_INFO 1

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP "\t.section\t.bss"

/* Define the name of the .data section.  */
#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.section .data"

/* Define the name of the .ident op.  */
#undef TARGET_ASM_OUTPUT_IDENT
#define TARGET_ASM_OUTPUT_IDENT default_asm_output_ident_directive

/* Enable alias attribute support.  */
#ifndef SET_ASM_OP
#define SET_ASM_OP "\t.set\t"
#endif

/* Define the name of the .text section.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.section .text"

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
        if (!flag_iso)                          \
	   builtin_define_with_int_value ("DJGPP",2);  \
	builtin_define_with_int_value ("__DJGPP",2);   \
	builtin_define_with_int_value ("__DJGPP__",2); \
	builtin_define_std ("MSDOS");		\
	builtin_define_std ("GO32");		\
	builtin_define_std ("unix");		\
	builtin_assert ("system=msdos");	\
    }						\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "-remap %{posix:-D_POSIX_SOURCE}"

#undef POST_LINK_SPEC
#define POST_LINK_SPEC "stubify %{v} %{o*:%*} %{!o*:a.out}"

/* Always just link in 'libc.a'.  */
#undef LIB_SPEC
#define LIB_SPEC "-lc"

/* Pick the right startup code depending on the -pg flag.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:crt0.o%s}"

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  i386_djgpp_asm_named_section

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG) != 0) fprintf ((FILE), "\t.p2align %d\n", LOG)

/* This is how to output a global symbol in the BSS section.  */
#undef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* Write the extra assembler code needed to declare a function properly.  */

#ifndef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      ASM_OUTPUT_FUNCTION_LABEL (FILE, NAME, DECL);		\
    }								\
  while (0)
#endif

/* This is how to tell assembler that a symbol is weak  */
#undef ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* djgpp automatically calls its own version of __main, so don't define one
   in libgcc, nor call one in main().  */
#define HAS_INIT_SECTION

#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION no_c99_libc_has_function

/* Definitions for types and sizes. Wide characters are 16-bits long so
   Win32 compiler add-ons will be wide character compatible.  */
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 16

#undef WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"

#undef WINT_TYPE
#define WINT_TYPE "int"

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef DEBUGGER_REGNO
#define DEBUGGER_REGNO(n) svr4_debugger_register_map[n]

/* Default to pcc-struct-return.  */
#define DEFAULT_PCC_STRUCT_RETURN 1

/* Ignore (with warning) -fPIC for DJGPP */
#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS                                      \
    do {                                                                \
        if (flag_pic)                                                   \
        {                                                               \
            fnotice(stdout, "-f%s ignored (not supported for DJGPP)\n", \
                (flag_pic > 1) ? "PIC" : "pic");                        \
            flag_pic = 0;                                               \
        }                                                               \
                                                                        \
        /* Don't emit DWARF3/4 unless specifically selected. */         \
        /* DWARF3/4 currently does not work for DJGPP.  */              \
        if (!OPTION_SET_P (dwarf_version))                        \
            dwarf_version = 2;                                          \
                                                                        \
        }                                                               \
    while (0)

/* Support for C++ templates.  */
#undef MAKE_DECL_ONE_ONLY
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

#undef TARGET_COFF
#define TARGET_COFF 1

/* Kludge because of missing COFF support for early LTO debug.  */
#undef  TARGET_ASM_LTO_START
#define TARGET_ASM_LTO_START i386_djgpp_asm_lto_start
#undef  TARGET_ASM_LTO_END
#define TARGET_ASM_LTO_END i386_djgpp_asm_lto_end

/* Function protypes for gcc/i386/djgpp.cc */

void
i386_djgpp_asm_named_section(const char *name, unsigned int flags,
			     tree decl);
void i386_djgpp_asm_lto_start (void);
void i386_djgpp_asm_lto_end (void);
