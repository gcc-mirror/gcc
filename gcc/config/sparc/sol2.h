/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   Copyright 1992, 1995-8, 1999 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@netcom.com).
   Additional changes by David V. Henkel-Wallace (gumby@cygnus.com).

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

/* Supposedly the same as vanilla sparc svr4, except for the stuff below: */
#include "sparc/sysv4.h"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
"-Dsparc -Dsun -Dunix -D__svr4__ -D__SVR4 \
-Asystem(unix) -Asystem(svr4)"

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{pthreads:-D_REENTRANT -D_PTHREADS} \
%{!pthreads:%{threads:-D_REENTRANT -D_SOLARIS_THREADS}} \
%{compat-bsd:-iwithprefixbefore ucbinclude -I/usr/ucbinclude} \
"

/* The sun bundled assembler doesn't accept -Yd, (and neither does gas).
   It's safe to pass -s always, even if -g is not used. */
#undef ASM_SPEC
#define ASM_SPEC "\
%{v:-V} %{Qy:} %{!Qn:-Qy} %{n} %{T} %{Ym,*} %{Wa,*:%*} -s \
%{fpic:-K PIC} %{fPIC:-K PIC} \
%(asm_cpu) \
"

/* This is here rather than in sparc.h because it's not known what
   other assemblers will accept.  */
#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v8plus"
#endif
#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v8plusa"
#endif
#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=v8plus:-xarch=v8plus} \
%{mcpu=ultrasparc:-xarch=v8plusa} \
%{!mcpu*:%(asm_cpu_default)} \
"

/* However it appears that Solaris 2.0 uses the same reg numbering as
   the old BSD-style system did. */

#undef DBX_REGISTER_NUMBER
/* Same as sparc.h */
#define DBX_REGISTER_NUMBER(REGNO) \
  (TARGET_FLAT && REGNO == FRAME_POINTER_REGNUM ? 31 : REGNO)

/* We use stabs-in-elf for debugging, because that is what the native
   toolchain uses.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* The Solaris 2 assembler uses .skip, not .zero, so put this back. */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

/* Use .uahalf/.uaword so packed structure members don't generate
   assembler errors when using the native assembler.  */
#undef ASM_SHORT
#define ASM_SHORT ".uahalf"
#undef ASM_LONG
#define ASM_LONG ".uaword"

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
  sprintf ((LABEL), "*.L%s%ld", (PREFIX), (long)(NUM))


/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.
   We don't use the standard LIB_SPEC only because we don't yet support c++ */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{p:mcrt1.o%s} \
                          %{!p: \
	                    %{pg:gcrt1.o%s gmon.o%s} \
                            %{!pg:crt1.o%s}}}} \
			crti.o%s \
			%{ansi:values-Xc.o%s} \
			%{!ansi: \
			 %{traditional:values-Xt.o%s} \
			 %{!traditional:values-Xa.o%s}} \
			crtbegin.o%s"

/* ??? Note: in order for -compat-bsd to work fully,
   we must somehow arrange to fixincludes /usr/ucbinclude
   and put the result in $(libsubdir)/ucbinclude.  */

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

/* This should be the same as in svr4.h, except with -R added.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{b} %{Wl,*:%*} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %{G:-G} \
   %{YP,*} \
   %{R*} \
   %{compat-bsd: \
     %{!YP,*:%{pg:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
             %{!pg:%{p:-Y P,/usr/ucblib:/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
                   %{!p:-Y P,/usr/ucblib:/usr/ccs/lib:/usr/lib}}} \
             -R /usr/ucblib} \
   %{!compat-bsd: \
     %{!YP,*:%{pg:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
             %{!pg:%{p:-Y P,/usr/ccs/lib/libp:/usr/lib/libp:/usr/ccs/lib:/usr/lib} \
                   %{!p:-Y P,/usr/ccs/lib:/usr/lib}}}} \
   %{Qy:} %{!Qn:-Qy}"

/* This defines which switch letters take arguments.
   It is as in svr4.h but with -R added.  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) \
  (DEFAULT_SWITCH_TAKES_ARG(CHAR) \
   || (CHAR) == 'R' \
   || (CHAR) == 'h' \
   || (CHAR) == 'x' \
   || (CHAR) == 'z')

/* ??? This does not work in SunOS 4.x, so it is not enabled in sparc.h.
   Instead, it is enabled here, because it does work under Solaris.  */
/* Define for support of TFmode long double and REAL_ARITHMETIC.
   Sparc ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128

/* But indicate that it isn't supported by the hardware.  */
#define WIDEST_HARDWARE_FP_SIZE 64

#define STDC_0_IN_SYSTEM_HEADERS

#define MULDI3_LIBCALL "__mul64"
#define DIVDI3_LIBCALL "__div64"
#define UDIVDI3_LIBCALL "__udiv64"
#define MODDI3_LIBCALL "__rem64"
#define UMODDI3_LIBCALL "__urem64"

#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS						\
  fixsfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__ftol" : "__ftoll");		\
  fixunssfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__ftoul" : "__ftoull");	\
  fixdfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__dtol" : "__dtoll");		\
  fixunsdfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__dtoul" : "__dtoull")

/* No weird SPARC variants on Solaris */
#undef TARGET_LIVE_G0
#define TARGET_LIVE_G0	0
#undef TARGET_BROKEN_SAVERESTORE
#define TARGET_BROKEN_SAVERESTORE 0

/* Solaris allows 64 bit out and global registers in 32 bit mode.
   sparc_override_options will disable V8+ if not generating V9 code.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_EPILOGUE + MASK_FPU + MASK_V8PLUS)

#if TARGET_ARCH32
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
#endif /* sparc32 */

/*
 * Attempt to turn on access permissions for the stack.
 *
 * This code must be defined when compiling gcc but not when compiling
 * libgcc2.a, unless we're generating code for 64 bits SPARC
 *
 * _SC_STACK_PROT is only defined for post 2.6, but we want this code
 * to run always.  2.6 can change the stack protection but has no way to
 * query it.
 *
 */

#define TRANSFER_FROM_TRAMPOLINE					\
static int need_enable_exec_stack;					\
									\
static void check_enabling(void) __attribute__ ((constructor));		\
static void check_enabling(void)					\
{									\
  extern long sysconf(int);						\
									\
  int prot = (int) sysconf(515 /*_SC_STACK_PROT */);			\
  if (prot != 7)							\
    need_enable_exec_stack = 1;						\
}									\
									\
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
    /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */ 			\
    if (mprotect (page, end - page, 7) < 0)				\
      perror ("mprotect of trampoline code");				\
  }									\
}
