/* Configuration for  a Mips ABI32 OpenBSD target.
   Copyright (C) 1999 Free Software Foundation, Inc.

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

/* Definitions needed for OpenBSD, to avoid picking mips 'defaults'.  */

/* GAS must know this.  */
#define SUBTARGET_ASM_SPEC "%{fPIC:-KPIC} %|"

/* CPP specific OpenBSD specs.  */
#define SUBTARGET_CPP_SPEC OBSD_CPP_SPEC

/* Needed for ELF (inspired by netbsd-elf).  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#define LOCAL_LABEL_PREFIX	"."

/* The profiling lib spec here is not really correct but we leave
   it as it is until we have some kind of profiling working.  */
#define LIB_SPEC OBSD_LIB_SPEC

/* By default, OpenBSD mips is little endian.  This is important to set
   here as mips/mips.h defaults to big endian unless DECSTATION.  */
#ifndef TARGET_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT 0
#endif

#include <mips/mips.h>

/* Get generic OpenBSD definitions.  */
#define OBSD_HAS_DECLARE_FUNCTION_NAME
#define OBSD_HAS_DECLARE_OBJECT
#define OBSD_HAS_CORRECT_SPECS
#include <openbsd.h>

/* mips assembler uses .set for arcane purposes.  __attribute__((alias))
   and friends won't work until we get recent binutils with .weakext
	support.  */
#undef SET_ASM_OP

/* Run-time target specifications.  */
#if TARGET_ENDIAN_DEFAULT != 0
#define CPP_PREDEFINES "-D__SYSTYPE_BSD__ -D__NO_LEADING_UNDERSCORES__ \
-D__GP_SUPPORT__ -D__MIPSEB__ -D__unix__  -D__OpenBSD__ -D__mips__ \
-Asystem(unix) -Asystem(OpenBSD) -Acpu(mips) -Amachine(mips) -Aendian(big)"
#else
#define CPP_PREDEFINES "-D__SYSTYPE_BSD__ -D__NO_LEADING_UNDERSCORES__ \
-D__GP_SUPPORT__ -D__MIPSEL__ -D__unix__  -D__OpenBSD__ -D__mips__ \
-Asystem(unix) -Asystem(OpenBSD) -Acpu(mips) -Amachine(mips) -Aendian(little)"
#endif

/* Layout of source language data types.  */

/* This must agree with <machine/ansi.h>.  */
#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Controlling the compilation driver.  */

/* LINK_SPEC appropriate for OpenBSD:  support for GCC options 
   -static, -assert, and -nostdlib. Dynamic loader control.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} \
   %{bestGnum} %{shared} %{non_shared} \
   %{call_shared} %{no_archive} %{exact_version} \
   %{!shared: %{!non_shared: %{!call_shared: -non_shared}}} \
   %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.so} \
   %{!nostdlib:%{!r*:%{!e*:-e __start}}} -dc -dp \
   %{static:-Bstatic} %{!static:-Bdynamic} %{assert*}"

/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */
#undef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 0


/* Since gas and gld are standard on OpenBSD, we don't need these.  */
#undef ASM_FINAL_SPEC
#undef STARTFILE_SPEC

/* A C statement to output something to the assembler file to switch to 
   section NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL 
   or NULL_TREE.  Some target formats do not support arbitrary sections.  
   Do not define this macro in such cases. mips.h doesn't define this, 
   do it here.  */
#define ASM_OUTPUT_SECTION_NAME(F, DECL, NAME, RELOC)                        \
do {                                                                         \
  extern FILE *asm_out_text_file;                                            \
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)                           \
    fprintf (asm_out_text_file, "\t.section %s,\"ax\",@progbits\n", (NAME)); \
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))                    \
    fprintf (F, "\t.section %s,\"a\",@progbits\n", (NAME));                  \
  else                                                                       \
    fprintf (F, "\t.section %s,\"aw\",@progbits\n", (NAME));                 \
} while (0)

/* collect2 support (Macros for initialization).  */

/* Mips default configuration is COFF-only, and confuses collect2.  */
#undef OBJECT_FORMAT_COFF
#undef EXTENDED_COFF
