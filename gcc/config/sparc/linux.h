/* Definitions for SPARC running Linux-based GNU systems with ELF.
   Copyright (C) 1996, 1997, 1998, 1999 Free Software Foundation, Inc.
   Contributed by Eddie C. Dost (ecd@skynet.be)

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

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

/* GNU/Linux uses ctype from glibc.a. I am not sure how complete it is.
   For now, we play safe. It may change later. */

#if 0
#undef MULTIBYTE_CHARS
#define MULTIBYTE_CHARS 1
#endif

#ifndef USE_GNULIBC_1
#undef DEFAULT_VTABLE_THUNKS
#define DEFAULT_VTABLE_THUNKS 1
#endif

/* Use stabs instead of DWARF debug format.  */
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

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

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'. */
   
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

/* This is for -profile to use -lc_p instead of -lc. */
#undef	CC1_SPEC
#define	CC1_SPEC "%{profile:-p} \
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc GNU/Linux with ELF)");

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
{"long-double-64", -MASK_LONG_DOUBLE_128, "Use 64 bit long doubles" }, \
{"long-double-128", MASK_LONG_DOUBLE_128, "Use 128 bit long doubles" },

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef MAX_WCHAR_TYPE_SIZE
    
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ELF__ -Dunix -D__sparc__ -Dlinux -Asystem(unix) -Asystem(posix)"

#undef CPP_SUBTARGET_SPEC
#ifdef USE_GNULIBC_1
#define CPP_SUBTARGET_SPEC \
"%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE} \
%{mlong-double-128:-D__LONG_DOUBLE_128__}"
#else
#define CPP_SUBTARGET_SPEC \
"%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{posix:-D_POSIX_SOURCE} \
%{pthread:-D_REENTRANT} %{mlong-double-128:-D__LONG_DOUBLE_128__}"
#endif

#undef LIB_SPEC
/* We no longer link with libc_p.a or libg.a by default. If you
   want to profile or debug the GNU/Linux C library, please add
   -lc_p or -ggdb to LDFLAGS at the link time, respectively.  */
#if 1
#ifdef USE_GNULIBC_1
#define LIB_SPEC \
  "%{!shared: %{p:-lgmon} %{pg:-lgmon} %{profile:-lgmon -lc_p} \
     %{!profile:%{!ggdb:-lc} %{ggdb:-lg}}}"
#else
#define LIB_SPEC \
  "%{shared: -lc} \
   %{!shared: %{mieee-fp:-lieee} %{pthread:-lpthread} \
     %{profile:-lc_p} %{!profile: -lc}}"
#endif
#else
#define LIB_SPEC \
  "%{!shared: \
     %{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
       %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}}"
#endif

/* Provide a LINK_SPEC appropriate for GNU/Linux.  Here we provide support
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
#ifdef USE_GNULIBC_1
#ifndef LINUX_DEFAULT_ELF
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/elf/ld-linux.so.1} \
        %{!rpath:-rpath /lib/elf/}} %{static:-static}}}"
#else
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.1}} \
        %{static:-static}}}"
#endif
#else
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker /lib/ld-linux.so.2}} \
        %{static:-static}}}"
#endif

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC \
  "%{V} %{v:%{!V:-V}} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s %{fpic:-K PIC} %{fPIC:-K PIC}"

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* We use stabs-in-elf for debugging, because that is what the native
   toolchain uses.  XXX */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

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


/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

/* Constant which presents upper bound of the above value.  */
#define MAX_LONG_DOUBLE_TYPE_SIZE 128

/* Define this to set long double type size to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __LONG_DOUBLE_128__
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 128
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#endif

/* No weird SPARC variants on Linux */
#undef TARGET_LIVE_G0
#define TARGET_LIVE_G0			0
#undef TARGET_BROKEN_SAVERESTORE
#define TARGET_BROKEN_SAVERESTORE	0

/* Override MACHINE_STATE_{SAVE,RESTORE} because we have special
   traps available which can get and set the condition codes
   reliably.  */
#undef MACHINE_STATE_SAVE
#define MACHINE_STATE_SAVE(ID)				\
  unsigned long int ms_flags, ms_saveret;		\
  asm volatile("ta	0x20\n\t"			\
	       "mov	%%g1, %0\n\t"			\
	       "mov	%%g2, %1\n\t"			\
	       : "=r" (ms_flags), "=r" (ms_saveret));

#undef MACHINE_STATE_RESTORE
#define MACHINE_STATE_RESTORE(ID)			\
  asm volatile("mov	%0, %%g1\n\t"			\
	       "mov	%1, %%g2\n\t"			\
	       "ta	0x21\n\t"			\
	       : /* no outputs */			\
	       : "r" (ms_flags), "r" (ms_saveret));
