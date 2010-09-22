/* Definitions for SPARC running Linux-based GNU systems with ELF.
   Copyright (C) 1996, 1997, 1998, 1999, 2000, 2002, 2003, 2004, 2005, 2006,
   2007, 2008, 2009 Free Software Foundation, Inc.
   Contributed by Eddie C. Dost (ecd@skynet.be)

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

#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      LINUX_TARGET_OS_CPP_BUILTINS();		\
      if (TARGET_LONG_DOUBLE_128)       	\
	builtin_define ("__LONG_DOUBLE_128__");	\
    }						\
  while (0)

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s\
   %{ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"

/* This is for -profile to use -lc_p instead of -lc.  */
#undef	CC1_SPEC
#define	CC1_SPEC "%{profile:-p} \
%{sun4:} %{target:} \
%{mcypress:-mcpu=cypress} \
%{msparclite:-mcpu=sparclite} %{mf930:-mcpu=f930} %{mf934:-mcpu=f934} \
%{mv8:-mcpu=v8} %{msupersparc:-mcpu=supersparc} \
"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (sparc GNU/Linux with ELF)");

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC \
"%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

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

/* If ELF is the default format, we should not use /lib/elf.  */

#define GLIBC_DYNAMIC_LINKER "/lib/ld-linux.so.2"

#undef  LINK_SPEC
#define LINK_SPEC "-m elf32_sparc -Y P,/usr/lib %{shared:-shared} \
  %{!mno-relax:%{!r:-relax}} \
  %{!shared: \
    %{!ibcs: \
      %{!static: \
        %{rdynamic:-export-dynamic} \
        %{!dynamic-linker:-dynamic-linker " LINUX_DYNAMIC_LINKER "}} \
        %{static:-static}}}"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used.  */
#undef ASM_SPEC
#define ASM_SPEC "\
%{V} \
%{v:%{!V:-V}} \
%{!Qn:-Qy} \
%{n} \
%{T} \
%{Ym,*} \
%{Wa,*:%*} \
-s \
%{fpic|fPIC|fpie|fPIE:-K PIC} \
%{!.c:%{findirect-dispatch:-K PIC}} \
%(asm_cpu) %(asm_relax)"

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fputs ("\t.local\t", (FILE));		\
  assemble_name ((FILE), (NAME));					\
  putc ('\n', (FILE));							\
  ASM_OUTPUT_ALIGNED_COMMON (FILE, NAME, SIZE, ALIGN);			\
} while (0)

#undef COMMON_ASM_OP
#define COMMON_ASM_OP "\t.common\t"

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.L%s%ld", PREFIX, (long)(NUM))


/* Define for support of TFmode long double.
   SPARC ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

/* Define this to set long double type size to use in libgcc2.c, which can
   not depend on target_flags.  */
#ifdef __LONG_DOUBLE_128__
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 128
#else
#define LIBGCC2_LONG_DOUBLE_TYPE_SIZE 64
#endif

#undef DITF_CONVERSION_LIBFUNCS
#define DITF_CONVERSION_LIBFUNCS 1

#ifdef HAVE_AS_TLS
#undef TARGET_SUN_TLS
#undef TARGET_GNU_TLS
#define TARGET_SUN_TLS 0
#define TARGET_GNU_TLS 1
#endif

/* We use GNU ld so undefine this so that attribute((init_priority)) works.  */
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP

#define MD_UNWIND_SUPPORT "config/sparc/linux-unwind.h"

/* Linux currently uses RMO in uniprocessor mode, which is equivalent to
   TMO, and TMO in multiprocessor mode.  But they reserve the right to
   change their minds.  */
#undef SPARC_RELAXED_ORDERING
#define SPARC_RELAXED_ORDERING true

#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* sparc glibc provides __stack_chk_guard in [%g7 + 0x14].  */
#define TARGET_THREAD_SSP_OFFSET	0x14
#endif

/* Define if long doubles should be mangled as 'g'.  */
#define TARGET_ALTERNATE_LONG_DOUBLE_MANGLING

/* We use glibc _mcount for profiling.  */
#undef NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS	1
