/* Definitions for 64-bit SPARC running Linux with ELF
   Copyright 1996, 1997 Free Software Foundation, Inc.
   Contributed by David S. Miller (davem@caip.rutgers.edu)

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

#define LINUX_DEFAULT_ELF

/* This is a v9 only compiler.  -mv8 is not expected to work.  If you want
   a v8/v9 compiler, this isn't the place to do it.  */

#define SPARC_V9 1	/* See sparc.h.  */
#define SPARC_ARCH64 1

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

#include <sparc/sysv4.h>

#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */
#undef ASM_FILE_START
#define ASM_FILE_START(FILE)                                            \
  do {                                                                  \
        output_file_directive (FILE, main_input_filename);              \
        fprintf (FILE, "\t.version\t\"01.01\"\n");                      \
  } while (0)
  
#undef ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-Av9a"

#undef  LIBGCC_SPEC
#define LIBGCC_SPEC \
  "%{!shared:-lgcc}"

/* Provide a STARTFILE_SPEC appropriate for Linux.  Here we add
   the Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'. */
   
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for Linux.  Here we tack on
   the Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc64 Linux/ELF)");

/* A v9 compiler with stack-bias, 32 bit integers, 64 bit longs and
   64 bit pointers, in a Medium/Anywhere code model environment.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_ARCH64 + MASK_LONG64 + MASK_PTR64 /* + MASK_HARD_QUAD */ \
   + MASK_STACK_BIAS + MASK_MEDANY + MASK_APP_REGS + MASK_EPILOGUE + MASK_FPU)

#undef SIZE_TYPE
#define SIZE_TYPE "long long unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long long int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD
    
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__sparc__ -D__sparc__ -D__sparc_v9__ -D__arch64__ -D__ELF__ -Dunix -Dsparc -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(sparc) -Amachine(sparc)"

#undef CPP_SPEC
#define CPP_SPEC "\
%{fPIC:-D__PIC__ -D__pic__} \
%{fpic:-D__PIC__ -D__pic__} \
%{mint64:-D__INT_MAX__=9223372036854775807LL -D__LONG_MAX__=9223372036854775807LL} \
%{mlong64:-D__LONG_MAX__=9223372036854775807LL} \
%{mlittle-endian:-D__LITTLE_ENDIAN__} \
%{msparclite:-D__sparclite__} \
%{mv8:-D__sparc_v8__} \
%{msupersparc:-D__supersparc__ -D__sparc_v8__} \
%{posix:-D_POSIX_SOURCE} \
"
/* We no longer link with libc_p.a or libg.a by default. If you
 * want to profile or debug the Linux C library, please add
 * -lc_p or -ggdb to LDFLAGS at the link time, respectively.
 */
#undef LIB_SPEC
#define LIB_SPEC \
  "%{!shared: %{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} \
     %{!ggdb:-lc} %{ggdb:-lg}}"

/* Provide a LINK_SPEC appropriate for Linux.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time. We like to support here for
   as many of the other GNU linker options as possible. But I don't
   have the time to search for those flags. I am sure how to add
   support for -soname shared_object_name. H.J.

   I took out %{v:%{!V:-V}}. It is too much :-(. They can use
   -Wl,-V.

   When the -shared link option is used a final link is not being
   done.  */

/* If ELF is the default format, we should not use /lib/elf. */

#undef  LINK_SPEC
#define LINK_SPEC "-m elf64_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.1}} \
        %{static:-static}}} \
%{mlittle-endian:-EL} \
"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC "\
%{V} \
%{v:%{!V:-V}} \
%{!Qn:-Qy} \
%{n} \
%{T} \
%{Ym,*} \
%{Wa,*:%*} \
-s %{fpic:-K PIC} %{fPIC:-K PIC} \
%{mlittle-endian:-EL} \
%(asm_cpu) \
"

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* System V Release 4 uses DWARF debugging info.  Buf DWARF1 doesn't do
   64-bit anything, so we use DWARF2.  */

#undef DWARF2_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common"

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%d", PREFIX, NUM)

/* Stabs doesn't use this, and it confuses a simulator.  */
/* ??? Need to see what DWARF needs, if anything.  */
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE)

/* Define the names of various pseudo-ops used by the Sparc/svr4 assembler.
   ??? If ints are 64 bits then UNALIGNED_INT_ASM_OP (defined elsewhere) is
   misnamed.  These should all refer to explicit sizes (half/word/xword?),
   anything other than short/int/long/etc.  */

#define UNALIGNED_DOUBLE_INT_ASM_OP	".uaxword"

/* DWARF bits.  */

/* Follow Irix 6 and not the Dwarf2 draft in using 64-bit offsets. 
   Obviously the Dwarf2 folks havn't tried to actually build systems
   with their spec.  On a 64-bit system, only 64-bit relocs become
   RELATIVE relocations.  */

/* #define DWARF_OFFSET_SIZE PTR_SIZE */
