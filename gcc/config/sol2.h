/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system.
   Copyright 2002 Free Software Foundation, Inc.

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

/* We use stabs-in-elf for debugging, because that is what the native
   toolchain uses.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Solaris 2 (at least as of 2.5.1) uses a 32-bit wchar_t.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Solaris 2 uses a wint_t different from the default. This is required
   by the SCD 2.4.1, p. 6-83, Figure 6-66.  */
#undef	WINT_TYPE
#define	WINT_TYPE "long int"

#undef	WINT_TYPE_SIZE
#define	WINT_TYPE_SIZE BITS_PER_WORD

#define HANDLE_PRAGMA_REDEFINE_EXTNAME 1

/* ??? Note: in order for -compat-bsd to work fully,
   we must somehow arrange to fixincludes /usr/ucbinclude
   and put the result in $(libsubdir)/ucbinclude.  */

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{pthreads:-D_REENTRANT -D_PTHREADS} \
%{!pthreads:%{threads:-D_REENTRANT -D_SOLARIS_THREADS}} \
%{compat-bsd:-iwithprefixbefore ucbinclude -I/usr/ucbinclude} \
"

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("unix");			\
	builtin_define_std ("sun");			\
	builtin_define ("__svr4__");			\
	builtin_define ("__SVR4");			\
	builtin_define ("__PRAGMA_REDEFINE_EXTNAME");	\
	builtin_assert ("system=unix");			\
	builtin_assert ("system=svr4");			\
	/* For C++ we need to add some additional macro \
	   definitions required by the C++ standard	\
	   library.  */					\
	if (c_language == clk_cplusplus)		\
	  {						\
	    builtin_define ("_XOPEN_SOURCE=500");	\
	    builtin_define ("_LARGEFILE_SOURCE=1");	\
	    builtin_define ("_LARGEFILE64_SOURCE=1");	\
	    builtin_define ("__EXTENSIONS__");		\
	  }						\
    } while (0)

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used.  */
#undef ASM_SPEC
#define ASM_SPEC "\
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
%{fpic:-K PIC} %{fPIC:-K PIC} \
%(asm_cpu) \
"

/* We don't use the standard LIB_SPEC only because we don't yet support c++.  */
#undef LIB_SPEC
#define LIB_SPEC \
  "%{compat-bsd:-lucb -lsocket -lnsl -lelf -laio} \
   %{!shared:\
     %{!symbolic:\
       %{pthreads:-lpthread} \
       %{!pthreads:%{threads:-lthread}} \
       %{p|pg:-ldl} -lc}}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{p:mcrt1.o%s} \
                          %{!p: \
	                    %{pg:gcrt1.o%s gmon.o%s} \
                            %{!pg:crt1.o%s}}}} \
			crti.o%s %(startfile_arch) \
			crtbegin.o%s"

#undef STARTFILE_ARCH32_SPEC
#define STARTFILE_ARCH32_SPEC "%{ansi:values-Xc.o%s} \
			    %{!ansi:values-Xa.o%s}"

#undef STARTFILE_ARCH_SPEC
#define STARTFILE_ARCH_SPEC STARTFILE_ARCH32_SPEC

#undef LINK_ARCH32_SPEC_BASE
#define LINK_ARCH32_SPEC_BASE \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{p|pg:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
             %{!p:%{!pg:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}}} \
             -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{p|pg:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
             %{!p:%{!pg:-Y P,/usr/ccs/lib:/usr/lib}}}}"

#undef LINK_ARCH32_SPEC
#define LINK_ARCH32_SPEC LINK_ARCH32_SPEC_BASE

#undef LINK_ARCH_SPEC
#define LINK_ARCH_SPEC LINK_ARCH32_SPEC

/* This should be the same as in svr4.h, except with -R added.  */
#undef  LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %(link_arch) \
   %{Qy:} %{!Qn:-Qy}"

/* This defines which switch letters take arguments.
   It is as in svr4.h but with -R added.  */
#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (DEFAULT_SWITCH_TAKES_ARG(CHAR) \
   || (CHAR) == 'R' \
   || (CHAR) == 'h' \
   || (CHAR) == 'z')

#define STDC_0_IN_SYSTEM_HEADERS 1

/*
 * Attempt to turn on access permissions for the stack.
 *
 * This code must be defined when compiling gcc but not when compiling
 * libgcc2.a, unless we're generating code for 64-bit SPARC
 *
 * _SC_STACK_PROT is only defined for post 2.6, but we want this code
 * to run always.  2.6 can change the stack protection but has no way to
 * query it.
 *
 */

/* This declares mprotect (used in TRANSFER_FROM_TRAMPOLINE) for
   libgcc2.c.  */
/* We don't want to include this because sys/mman.h is not present on
   some non-Solaris configurations that use sol2.h.  */
#if 0 /* def L_trampoline */
#include <sys/mman.h>
#endif

#define TRANSFER_FROM_TRAMPOLINE					\
									\
/* #define STACK_PROT_RWX (PROT_READ | PROT_WRITE | PROT_EXEC) */	\
									\
static int need_enable_exec_stack;					\
									\
static void check_enabling(void) __attribute__ ((constructor));		\
static void check_enabling(void)					\
{									\
  extern long sysconf(int);						\
									\
  int prot = (int) sysconf(515 /* _SC_STACK_PROT */);			\
  if (prot != 7 /* STACK_PROT_RWX */)					\
    need_enable_exec_stack = 1;						\
}									\
									\
extern void __enable_execute_stack (void *);				\
void									\
__enable_execute_stack (addr)						\
     void *addr;							\
{									\
  if (!need_enable_exec_stack)						\
    return;								\
  else {								\
    long size = getpagesize ();						\
    long mask = ~(size-1);						\
    char *page = (char *) (((long) addr) & mask); 			\
    char *end  = (char *) ((((long) (addr + TRAMPOLINE_SIZE)) & mask) + size); \
									\
    if (mprotect (page, end - page, 7 /* STACK_PROT_RWX */) < 0)	\
      perror ("mprotect of trampoline code");				\
  }									\
}
