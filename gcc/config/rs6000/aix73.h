// SPDX-License-Identifier: GPL-3.0-or-later
/* Definitions of target machine for GNU compiler,
   for IBM RS/6000 POWER running AIX V7.3.
   Copyright (C) 2002-2023 Free Software Foundation, Inc.
   Contributed by David Edelsohn (edelsohn@gnu.org).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* The macro SUBTARGET_OVERRIDE_OPTIONS is provided for subtargets, to
   get control in TARGET_OPTION_OVERRIDE.  */

#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (TARGET_64BIT && ! TARGET_POWERPC64)				\
    {									\
      rs6000_isa_flags |= OPTION_MASK_POWERPC64;			\
      warning (0, "%<-maix64%> requires PowerPC64 architecture remain enabled"); \
    }									\
  if (TARGET_SOFT_FLOAT && TARGET_LONG_DOUBLE_128)			\
    {									\
      rs6000_long_double_type_size = 64;				\
      if (OPTION_SET_P (rs6000_long_double_type_size))		\
	warning (0, "%<soft-float%> and long-double-128 are incompatible");	\
    }									\
  if (TARGET_POWERPC64 && ! TARGET_64BIT)				\
    {									\
      error ("%<-maix64%> required: 64-bit computation with 32-bit addressing not yet supported"); \
    }									\
  if ((rs6000_isa_flags_explicit					\
       & OPTION_MASK_MINIMAL_TOC) != 0)					\
    {									\
      if (OPTION_SET_P (rs6000_current_cmodel)			\
	  && rs6000_current_cmodel != CMODEL_SMALL)			\
	error ("%<-mcmodel%> incompatible with other toc options"); 	\
      SET_CMODEL (CMODEL_SMALL);					\
    }									\
  if (rs6000_current_cmodel != CMODEL_SMALL)				\
    {									\
      TARGET_NO_FP_IN_TOC = 1;						\
      TARGET_NO_SUM_IN_TOC = 1;						\
    }									\
  if (rs6000_current_cmodel == CMODEL_MEDIUM)				\
    {									\
      rs6000_current_cmodel = CMODEL_LARGE;				\
    }									\
  if (! strcmp (lang_hooks.name, "GNU Go")				\
      && TARGET_32BIT)							\
    {									\
      /* aix/ppc doesn't support -mvsx and -maltivec with Go */		\
      rs6000_isa_flags &= ~(OPTION_MASK_VSX | OPTION_MASK_ALTIVEC);	\
    }									\
  if (!OPTION_SET_P (dwarf_version))				\
    /* AIX only supports DWARF 4.  */					\
    dwarf_version = 4;							\
} while (0)

#define ASM_SPEC32 "-a32"
#define ASM_SPEC64 "-a64"
#define ASM_SPEC_COMMON "-u %(asm_cpu)"

/* Common ASM definitions used by ASM_SPEC amongst the various targets for
   handling -mcpu=xxx switches.  There is a parallel list in driver-rs6000.cc to
   provide the default assembler options if the user uses -mcpu=native, so if
   you make changes here, make them there also.  */
#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC \
"%{mcpu=native: %(asm_cpu_native); \
  mcpu=power10: -mpwr10; \
  mcpu=power9: -mpwr9; \
  mcpu=power8: -mpwr8; \
  mcpu=power7: -mpwr7; \
  mcpu=power6x|mcpu=power6: -mpwr6; \
  mcpu=power5+: -mpwr5x; \
  mcpu=power5: -mpwr5; \
  mcpu=power4: -mpwr4; \
  mcpu=power3: -m620; \
  mcpu=powerpc: -mppc; \
  mcpu=rs64: -mppc; \
  mcpu=603: -m603; \
  mcpu=603e: -m603; \
  mcpu=604: -m604; \
  mcpu=604e: -m604; \
  mcpu=620: -m620; \
  mcpu=630: -m620; \
  mcpu=970|mcpu=G5: -m970; \
  !mcpu*: %(asm_default)} \
-many"

#undef	ASM_DEFAULT_SPEC
#define ASM_DEFAULT_SPEC "-mpwr7"

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()     \
  do                                 \
    {                                \
      builtin_define ("_AIX43");     \
      builtin_define ("_AIX51");     \
      builtin_define ("_AIX52");     \
      builtin_define ("_AIX53");     \
      builtin_define ("_AIX61");     \
      builtin_define ("_AIX71");     \
      builtin_define ("_AIX72");     \
      builtin_define ("_AIX73");     \
      TARGET_OS_AIX_CPP_BUILTINS (); \
    }                                \
  while (0)

#define CPP_SPEC32 ""
#define CPP_SPEC64 "-D__64BIT__"
#define CPP_SPEC_COMMON "%{posix: -D_POSIX_SOURCE} \
  %{ansi: -D_ANSI_C_SOURCE}			\
  %{mpe: -I%R/usr/lpp/ppe.poe/include}		\
  %{pthread: -D_THREAD_SAFE}"

/* The GNU C++ standard library requires that these macros be
   defined.  Synchronize with libstdc++ os_defines.h.  */
#define CPLUSPLUS_CPP_SPEC_COMMON		\
  "-D_ALL_SOURCE -D__COMPATMATH__		\
   %{mpe: -I%R/usr/lpp/ppe.poe/include}		\
   %{pthread: -D_THREAD_SAFE}"

#define RS6000_CPU(NAME, CPU, FLAGS)
#include "rs6000-cpus.def"
#undef RS6000_CPU

#undef  TARGET_DEFAULT
#ifdef RS6000_BI_ARCH
#define TARGET_DEFAULT (ISA_2_6_MASKS_EMBEDDED | MASK_POWERPC64 | MASK_64BIT)
#else
#define TARGET_DEFAULT ISA_2_6_MASKS_EMBEDDED
#endif

#undef  PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_POWER7
#undef  PROCESSOR_DEFAULT64
#define PROCESSOR_DEFAULT64 PROCESSOR_POWER7

/* AIX 7.2 kernel and assembler have necessary support for Altivec and VSX.  */
#undef OS_MISSING_ALTIVEC

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#undef MULTILIB_DEFAULTS

#define DEFAULT_ARCH64_P (TARGET_DEFAULT & MASK_64BIT)

#define LIB_SPEC32 "%{!shared:%{g*:-lg}}"
#define LIB_SPEC64 ""
#define LIB_SPEC_COMMON "%{pg:-L%R/lib/profiled -L%R/usr/lib/profiled}\
   %{p:-L%R/lib/profiled -L%R/usr/lib/profiled}\
   %{fprofile-arcs|fprofile-generate*|coverage:-lpthreads}\
   %{mpe:-L%R/usr/lpp/ppe.poe/lib -lmpi -lvtd}\
   %{mlong-double-128:-lc128}\
   %{pthread:-lpthreads} -lc"

#define LINK_SPEC32 "%{!shared:%{g*: %(link_libg) }} -b32"
#define LINK_SPEC64 "-b64"
#define LINK_SPEC_COMMON "-bpT:0x10000000 -bpD:0x20000000 %{!r:-btextro}\
   %{static:-bnso %(link_syscalls) } %{shared:-bM:SRE %{!e:-bnoentry}}\
   %{mpe:-binitfini:poe_remote_main} "

#undef STARTFILE_SPEC
#if DEFAULT_ARCH64_P
#define STARTFILE_SPEC "%{!shared:\
   %{!maix32:%{pg:gcrt0_64%O%s;:%{p:mcrt0_64%O%s;:crt0_64%O%s}};:\
     %{pthread:%{pg:gcrt0_r%O%s;:%{p:mcrt0_r%O%s;:crt0_r%O%s}};:\
       %{pg:gcrt0%O%s;:%{p:mcrt0%O%s;:crt0%O%s}}}}}\
   %{!maix32:%{shared:crtcxa_64_s%O%s;:crtcxa_64%O%s} crtdbase_64%O%s;:\
     %{shared:crtcxa_s%O%s;:crtcxa%O%s} crtdbase%O%s}"
#else
#define STARTFILE_SPEC "%{!shared:\
   %{maix64:%{pg:gcrt0_64%O%s;:%{p:mcrt0_64%O%s;:crt0_64%O%s}};:\
     %{pthread:%{pg:gcrt0_r%O%s;:%{p:mcrt0_r%O%s;:crt0_r%O%s}};:\
       %{pg:gcrt0%O%s;:%{p:mcrt0%O%s;:crt0%O%s}}}}}\
   %{maix64:%{shared:crtcxa_64_s%O%s;:crtcxa_64%O%s} crtdbase_64%O%s;:\
     %{shared:crtcxa_s%O%s;:crtcxa%O%s} crtdbase%O%s}"
#endif


#undef ASM_SPEC
#undef CPP_SPEC
#undef CPLUSPLUS_CPP_SPEC
#undef LIB_SPEC
#undef LINK_SPEC

#if DEFAULT_ARCH64_P
#define ASM_SPEC "%{maix32:%(asm_spec32);:%(asm_spec64)} %(asm_spec_common)"
#define CPP_SPEC "%{maix32:%(cpp_spec32);:%(cpp_spec64)} %(cpp_spec_common)"
#define CPLUSPLUS_CPP_SPEC "%{maix32:%(cpp_spec32);:%(cpp_spec64)} %(cplusplus_cpp_spec_common)"
#define LIB_SPEC "%{maix32:%(lib_spec32);:%(lib_spec64)} %(lib_spec_common)"
#define LINK_SPEC "%{maix32:%(link_spec32);:%(link_spec64)} %(link_spec_common)"
#else
#define ASM_SPEC "%{maix64:%(asm_spec64);:%(asm_spec32)} %(asm_spec_common)"
#define CPP_SPEC "%{maix64:%(cpp_spec64);:%(cpp_spec32)} %(cpp_spec_common)"
#define CPLUSPLUS_CPP_SPEC "%{maix64:%(cpp_spec64);:%(cpp_spec32)} %(cplusplus_cpp_spec_common)"
#define LIB_SPEC "%{maix64:%(lib_spec64);:%(lib_spec32)} %(lib_spec_common)"
#define LINK_SPEC "%{maix64:%(link_spec64);:%(link_spec32)} %(link_spec_common)"
#endif

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS					\
  { "asm_spec_common",		ASM_SPEC_COMMON },		\
  { "asm_spec32",		ASM_SPEC32 },			\
  { "asm_spec64",		ASM_SPEC64 },			\
  { "cpp_spec_common",		CPP_SPEC_COMMON },		\
  { "cplusplus_cpp_spec_common", CPLUSPLUS_CPP_SPEC_COMMON },	\
  { "cpp_spec32",		CPP_SPEC32 },			\
  { "cpp_spec64",		CPP_SPEC64 },			\
  { "lib_spec_common",		LIB_SPEC_COMMON },		\
  { "lib_spec32",		LIB_SPEC32 },			\
  { "lib_spec64",		LIB_SPEC64 },			\
  { "link_spec_common",		LINK_SPEC_COMMON },		\
  { "link_spec32",		LINK_SPEC32 },			\
  { "link_spec64",		LINK_SPEC64 },

/* AIX V5 typedefs ptrdiff_t as "long" while earlier releases used "int".  */

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

/* Type used for wchar_t, as a string used in a declaration.  */
#undef  WCHAR_TYPE
#define WCHAR_TYPE (!TARGET_64BIT ? "short unsigned int" : "unsigned int")

/* Width of wchar_t in bits.  */
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE (!TARGET_64BIT ? 16 : 32)

/* AIX 4.2 and above provides initialization and finalization function
   support from linker command line.  */
#undef HAS_INIT_SECTION
#define HAS_INIT_SECTION

#undef LD_INIT_SWITCH
#define LD_INIT_SWITCH "-binitfini"

#ifndef _AIX52
extern long long int    atoll(const char *);
#endif

/* This target uses the aix64.opt file.  */
#define TARGET_USES_AIX64_OPT 1

/* Large TOC Support */
#ifdef HAVE_LD_LARGE_TOC
#undef TARGET_CMODEL
#define TARGET_CMODEL rs6000_current_cmodel
#define SET_CMODEL(opt) rs6000_current_cmodel = opt
#else
#define SET_CMODEL(opt) do {} while (0)
#endif

/* This target defines SUPPORTS_WEAK and TARGET_ASM_NAMED_SECTION,
   but does not have crtbegin/end.  */

#define TARGET_AIX_VERSION 73

/* AIX 7.2 supports DWARF3+ debugging.  */
#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#define DEBUG_INFO_SECTION	"0x10000"
#define DEBUG_LINE_SECTION	"0x20000"
#define DEBUG_PUBNAMES_SECTION	"0x30000"
#define DEBUG_PUBTYPES_SECTION	"0x40000"
#define DEBUG_ARANGES_SECTION	"0x50000"
#define DEBUG_ABBREV_SECTION	"0x60000"
#define DEBUG_STR_SECTION	"0x70000"
#define DEBUG_RANGES_SECTION	"0x80000"
#define DEBUG_LOC_SECTION	"0x90000"
#define DEBUG_FRAME_SECTION	"0xA0000"
#define DEBUG_MACINFO_SECTION	"0xB0000"
#define DEBUG_MACRO_SECTION	"0xB0000"

