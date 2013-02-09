/* Definitions for AMD x86-64 using GNU userspace.
   Copyright (C) 2001, 2002, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
	GNU_USER_TARGET_OS_CPP_BUILTINS();			\
    }								\
  while (0)

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

#undef CC1_SPEC
#define CC1_SPEC "%(cc1_cpu) %{profile:-p}"

/* The svr4 ABI for the i386 says that records and unions are returned
   in memory.  In the 64bit compilation we will turn this flag off in
   ix86_option_override_internal, as we never do pcc_struct_return
   scheme on this target.  */
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

#if TARGET_64BIT_DEFAULT
#define SPEC_32 "m32"
#define SPEC_64 "m32|mx32:;"
#define SPEC_X32 "mx32"
#else
#define SPEC_32 "m64|mx32:;"
#define SPEC_64 "m64"
#define SPEC_X32 "mx32"
#endif

#undef ASM_SPEC
#define ASM_SPEC "%{" SPEC_32 ":--32} \
 %{" SPEC_64 ":--64} \
 %{" SPEC_X32 ":--x32} \
 %{!mno-sse2avx:%{mavx:-msse2avx}} %{msse2avx:%{!mavx:-msse2avx}}"

#undef	LINK_SPEC
#define LINK_SPEC "%{" SPEC_64 ":-m " GNU_USER_LINK_EMULATION64 "} \
                   %{" SPEC_32 ":-m " GNU_USER_LINK_EMULATION32 "} \
                   %{" SPEC_X32 ":-m " GNU_USER_LINK_EMULATIONX32 "} \
  %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{" SPEC_32 ":-dynamic-linker " GNU_USER_DYNAMIC_LINKER32 "} \
      %{" SPEC_64 ":-dynamic-linker " GNU_USER_DYNAMIC_LINKER64 "} \
      %{" SPEC_X32 ":-dynamic-linker " GNU_USER_DYNAMIC_LINKERX32 "}} \
    %{static:-static}}"

/* Similar to standard GNU userspace, but adding -ffast-math support.  */
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
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
#define LIBGCC2_HAS_TF_MODE 1
#define LIBGCC2_TF_CEXT q
#define TF_SIZE 113

#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

/* The stack pointer needs to be moved while checking the stack.  */
#define STACK_CHECK_MOVING_SP 1

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* i386 glibc provides __stack_chk_guard in %gs:0x14,
   x32 glibc provides it in %fs:0x18. 
   x86_64 glibc provides it in %fs:0x28.  */
#define TARGET_THREAD_SSP_OFFSET \
  (TARGET_64BIT ? (TARGET_X32 ? 0x18 : 0x28) : 0x14)

/* We only build the -fsplit-stack support in libgcc if the
   assembler has full support for the CFI directives.  */
#if HAVE_GAS_CFI_PERSONALITY_DIRECTIVE
#define TARGET_CAN_SPLIT_STACK
#endif
/* We steal the last transactional memory word.  */
#define TARGET_THREAD_SPLIT_STACK_OFFSET \
  (TARGET_64BIT ? (TARGET_X32 ? 0x40 : 0x70) : 0x30)
#endif
