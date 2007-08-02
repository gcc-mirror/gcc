/* Definitions for AMD x86-64 running Linux-based GNU systems with ELF format.
   Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Jan Hubicka <jh@suse.cz>, based on linux.h.

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

#if TARGET_64BIT_DEFAULT
#define TARGET_VERSION fprintf (stderr, " (x86-64 Linux/ELF)");
#else
#define TARGET_VERSION fprintf (stderr, " (i386 Linux/ELF)");
#endif

#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
	LINUX_TARGET_OS_CPP_BUILTINS();				\
    }								\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  In the 64bit compilation we will turn this flag off in
   override_options, as we never do pcc_struct_return scheme on this target.  */
#undef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1

/* We arrange for the whole %fs segment to map the tls area.  */
#undef TARGET_TLS_DIRECT_SEG_REFS_DEFAULT
#define TARGET_TLS_DIRECT_SEG_REFS_DEFAULT MASK_TLS_DIRECT_SEG_REFS

/* Provide a LINK_SPEC.  Here we provide support for the special GCC
   options -static and -shared, which allow us to link things in one
   of these three modes by applying the appropriate combinations of
   options at link-time.

   When the -shared link option is used a final link is not being
   done.  */

#define GLIBC_DYNAMIC_LINKER32 "/lib/ld-linux.so.2"
#define GLIBC_DYNAMIC_LINKER64 "/lib64/ld-linux-x86-64.so.2"

#if TARGET_64BIT_DEFAULT
#define SPEC_32 "m32"
#define SPEC_64 "!m32"
#else
#define SPEC_32 "!m64"
#define SPEC_64 "m64"
#endif

#undef	LINK_SPEC
#define LINK_SPEC "%{" SPEC_64 ":-m elf_x86_64} %{" SPEC_32 ":-m elf_i386} \
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{" SPEC_32 ":%{!dynamic-linker:-dynamic-linker " LINUX_DYNAMIC_LINKER32 "}} \
      %{" SPEC_64 ":%{!dynamic-linker:-dynamic-linker " LINUX_DYNAMIC_LINKER64 "}}} \
    %{static:-static}}"

/* Similar to standard Linux, but adding -ffast-math support.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{mpc32:crtprec32.o%s} \
   %{mpc64:crtprec64.o%s} \
   %{mpc80:crtprec80.o%s} \
   %{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#if TARGET_64BIT_DEFAULT
#define MULTILIB_DEFAULTS { "m64" }
#else
#define MULTILIB_DEFAULTS { "m32" }
#endif

/* Put all *tf routines in libgcc.  */
#undef LIBGCC2_HAS_TF_MODE
#define LIBGCC2_HAS_TF_MODE TARGET_64BIT
#define LIBGCC2_TF_CEXT q
#define TF_SIZE 113

#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#define MD_UNWIND_SUPPORT "config/i386/linux-unwind.h"

/* This macro may be overridden in i386/k*bsd-gnu.h.  */
#define REG_NAME(reg) reg

#ifdef TARGET_LIBC_PROVIDES_SSP
/* i386 glibc provides __stack_chk_guard in %gs:0x14,
   x86_64 glibc provides it in %fs:0x28.  */
#define TARGET_THREAD_SSP_OFFSET	(TARGET_64BIT ? 0x28 : 0x14)
#endif
