/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX version 4.3.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by David Edelsohn (edelsohn@gnu.org).

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


#include "rs6000/rs6000.h"
#include "rs6000/aix.h"

/* AIX 4.3 and above support 64-bit executables.  */
#undef  SUBSUBTARGET_SWITCHES
#define SUBSUBTARGET_SWITCHES					\
  {"aix64", 		MASK_64BIT | MASK_POWERPC64 | MASK_POWERPC,	\
   "Compile for 64-bit pointers" },					\
  {"aix32",		- (MASK_64BIT | MASK_POWERPC64),		\
   "Compile for 32-bit pointers" },					\
  {"threads",		0,						\
   "Use the thread library and reentrant C library" },			\
  {"pe",		0,						\
   "Support message passing with the Parallel Environment" },

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   The macro SUBTARGET_OVERRIDE_OPTIONS is provided for subtargets, to
   get control.  */

#define NON_POWERPC_MASKS (MASK_POWER | MASK_POWER2 | MASK_STRING)
#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (TARGET_64BIT && (target_flags & NON_POWERPC_MASKS))		\
    {									\
      target_flags &= ~NON_POWERPC_MASKS;				\
      warning ("-maix64 and POWER architecture are incompatible.");	\
    }									\
  if (TARGET_64BIT && ! (target_flags & MASK_POWERPC64))		\
    {									\
      target_flags |= MASK_POWERPC64;					\
      warning ("-maix64 requires PowerPC64 architecture remain enabled."); \
    }									\
} while (0);

#undef ASM_SPEC
#define ASM_SPEC "-u %{maix64:-a64 -mppc64} %(asm_cpu)"

/* Common ASM definitions used by ASM_SPEC amonst the various targets
   for handling -mcpu=xxx switches.  */
#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC \
"%{!mcpu*: %{!maix64: \
  %{mpower: %{!mpower2: -mpwr}} \
  %{mpower2: -mpwr2} \
  %{mpowerpc*: %{!mpowerpc64: -mppc}} \
  %{mpowerpc64: -mppc64} \
  %{!mpower*: %{!mpowerpc*: %(asm_default)}}}} \
%{mcpu=common: -mcom} \
%{mcpu=power: -mpwr} \
%{mcpu=power2: -mpwr2} \
%{mcpu=powerpc: -mppc} \
%{mcpu=rios: -mpwr} \
%{mcpu=rios1: -mpwr} \
%{mcpu=rios2: -mpwr2} \
%{mcpu=rsc: -mpwr} \
%{mcpu=rsc1: -mpwr} \
%{mcpu=rs64a: -mppc} \
%{mcpu=403: -mppc} \
%{mcpu=505: -mppc} \
%{mcpu=601: -m601} \
%{mcpu=602: -mppc} \
%{mcpu=603: -m603} \
%{mcpu=603e: -m603} \
%{mcpu=604: -m604} \
%{mcpu=604e: -m604} \
%{mcpu=620: -mppc} \
%{mcpu=630: -mppc} \
%{mcpu=821: -mppc} \
%{mcpu=860: -mppc}"

#undef	ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mcom"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_IBMR2 -D_POWER -D_AIX -D_AIX32 -D_AIX41 -D_AIX43 \
-D_LONG_LONG -Asystem(unix) -Asystem(aix)"

#undef CPP_SPEC
#define CPP_SPEC "%{posix: -D_POSIX_SOURCE}\
   %{maix64: -D__64BIT__ -D_ARCH_PPC}\
   %{mpe: -I/usr/lpp/ppe.poe/include}\
   %{mthreads: -D_THREAD_SAFE}\
   %(cpp_cpu)"

/* Common CPP definitions used by CPP_SPEC among the various targets
   for handling -mcpu=xxx switches.  */
#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC \
"%{!mcpu*: %{!maix64: \
  %{mpower: %{!mpower2: -D_ARCH_PWR}} \
  %{mpower2: -D_ARCH_PWR2} \
  %{mpowerpc*: -D_ARCH_PPC} \
  %{!mpower*: %{!mpowerpc*: %(cpp_default)}}}} \
%{mcpu=common: -D_ARCH_COM} \
%{mcpu=power: -D_ARCH_PWR} \
%{mcpu=power2: -D_ARCH_PWR2} \
%{mcpu=powerpc: -D_ARCH_PPC} \
%{mcpu=rios: -D_ARCH_PWR} \
%{mcpu=rios1: -D_ARCH_PWR} \
%{mcpu=rios2: -D_ARCH_PWR2} \
%{mcpu=rsc: -D_ARCH_PWR} \
%{mcpu=rsc1: -D_ARCH_PWR} \
%{mcpu=rs64a: -D_ARCH_PPC} \
%{mcpu=403: -D_ARCH_PPC} \
%{mcpu=505: -D_ARCH_PPC} \
%{mcpu=601: -D_ARCH_PPC -D_ARCH_PWR} \
%{mcpu=602: -D_ARCH_PPC} \
%{mcpu=603: -D_ARCH_PPC} \
%{mcpu=603e: -D_ARCH_PPC} \
%{mcpu=604: -D_ARCH_PPC} \
%{mcpu=620: -D_ARCH_PPC} \
%{mcpu=630: -D_ARCH_PPC} \
%{mcpu=821: -D_ARCH_PPC} \
%{mcpu=860: -D_ARCH_PPC}"

#undef	CPP_DEFAULT_SPEC
#define CPP_DEFAULT_SPEC "-D_ARCH_COM"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_NEW_MNEMONICS

#undef PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC604

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef	MULTILIB_DEFAULTS
#define	MULTILIB_DEFAULTS { "mcpu=common" }

#undef LIB_SPEC
#define LIB_SPEC "%{pg:-L/lib/profiled -L/usr/lib/profiled}\
   %{p:-L/lib/profiled -L/usr/lib/profiled}\
   %{!maix64:%{!shared:%{g*:-lg}}}\
   %{mpe:-L/usr/lpp/ppe.poe/lib -lmpi -lvtd}\
   %{mthreads:-L/usr/lib/threads -lpthreads -lc_r /usr/lib/libc.a}\
   %{!mthreads:-lc}"

#undef LINK_SPEC
#define LINK_SPEC "-bpT:0x10000000 -bpD:0x20000000 %{!r:-btextro} -bnodelcsect\
   %{static:-bnso %(link_syscalls) } %{shared:-bM:SRE %{!e:-bnoentry}}\
   %{!maix64:%{!shared:%{g*: %(link_libg) }}} %{maix64:-b64}"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:\
   %{mpe:%{pg:/usr/lpp/ppe.poe/lib/gcrt0.o}\
         %{!pg:%{p:/usr/lpp/ppe.poe/lib/mcrt0.o}\
               %{!p:/usr/lpp/ppe.poe/lib/crt0.o}}}\
   %{!mpe:\
     %{maix64:%{pg:gcrt0_64%O%s}%{!pg:%{p:mcrt0_64%O%s}%{!p:crt0_64%O%s}}}\
     %{!maix64:\
       %{mthreads:%{pg:gcrt0_r%O%s}%{!pg:%{p:mcrt0_r%O%s}%{!p:crt0_r%O%s}}}\
       %{!mthreads:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}}}}"

/* AIX 4.3 typedefs ptrdiff_t as "long" while earlier releases used "int".  */

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

/* AIX 4 uses PowerPC nop (ori 0,0,0) instruction as call glue for PowerPC
   and "cror 31,31,31" for POWER architecture.  */

#undef RS6000_CALL_GLUE
#define RS6000_CALL_GLUE "{cror 31,31,31|nop}"

/* AIX 4.2 and above provides initialization and finalization function
   support from linker command line.  */
#undef HAS_INIT_SECTION
#define HAS_INIT_SECTION

#undef LD_INIT_SWITCH
#define LD_INIT_SWITCH "-binitfini"
