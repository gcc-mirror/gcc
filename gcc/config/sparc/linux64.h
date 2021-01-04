/* Definitions for 64-bit SPARC running Linux-based GNU systems with ELF.
   Copyright (C) 1996-2021 Free Software Foundation, Inc.
   Contributed by David S. Miller (davem@caip.rutgers.edu)

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
      GNU_USER_TARGET_OS_CPP_BUILTINS();	\
      if (TARGET_ARCH64)			\
        builtin_define ("_LONGLONG");		\
      if (TARGET_ARCH32				\
          && TARGET_LONG_DOUBLE_128)		\
	builtin_define ("__LONG_DOUBLE_128__");	\
    }						\
  while (0)

/* On Linux, the combination sparc64-* --with-cpu=v8 is supported and
   selects a 32-bit compiler.  */
#if defined(TARGET_64BIT_DEFAULT) && TARGET_CPU_DEFAULT >= TARGET_CPU_v9
#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_PTR64 + MASK_64BIT + MASK_STACK_BIAS + \
   MASK_APP_REGS + MASK_FPU + MASK_LONG_DOUBLE_128)
#endif

/* This must be v9a not just v9 because by default we enable
   -mvis.  */
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC "-Av9a"

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  GNU_USER_TARGET_ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s}"

/* The default code model.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDLOW

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Define for support of TFmode long double.
   SPARC ABI says that long double is 4 words.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (TARGET_LONG_DOUBLE_128 ? 128 : 64)

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{posix:-D_POSIX_SOURCE} \
%{pthread:-D_REENTRANT} \
"

/* Provide a LINK_SPEC appropriate for GNU/Linux.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time.

   When the -shared link option is used a final link is not being
   done.  */

#define GLIBC_DYNAMIC_LINKER32 "/lib/ld-linux.so.2"
#define GLIBC_DYNAMIC_LINKER64 "/lib64/ld-linux.so.2"

#ifdef SPARC_BI_ARCH

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "link_arch32",       LINK_ARCH32_SPEC },              \
  { "link_arch64",       LINK_ARCH64_SPEC },              \
  { "link_arch_default", LINK_ARCH_DEFAULT_SPEC },	  \
  { "link_arch",	 LINK_ARCH_SPEC },

#define LINK_ARCH32_SPEC "-m elf32_sparc %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER32 "} \
      %{static:-static}} \
"

#define LINK_ARCH64_SPEC "-m elf64_sparc %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER64 "} \
      %{static:-static}} \
"

#define LINK_ARCH_SPEC "\
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef  LINK_SPEC
#define LINK_SPEC "\
%(link_arch) \
%{!mno-relax:%{!r:-relax}} \
"

/* -mcpu=native handling only makes sense with compiler running on
   a SPARC chip.  */
#if defined(__sparc__) && defined(__linux__)
extern const char *host_detect_local_cpu (int argc, const char **argv);
# define EXTRA_SPEC_FUNCTIONS						\
  { "local_cpu_detect", host_detect_local_cpu },

# define MCPU_MTUNE_NATIVE_SPECS					\
   " %{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)}"		\
   " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
# define MCPU_MTUNE_NATIVE_SPECS ""
#endif

#define DRIVER_SELF_SPECS MCPU_MTUNE_NATIVE_SPECS

/* -fsanitize=address is currently only supported for 32-bit.  */
#define ASAN_REJECT_SPEC \
  "%{!%:sanitize(thread):%e-fsanitize=address is not supported in this configuration}"

#undef  ASAN_CC1_SPEC
#if DEFAULT_ARCH32_P
#define ASAN_CC1_SPEC \
  "%{%:sanitize(address):-funwind-tables %{m64:" ASAN_REJECT_SPEC "}}"
#else
#define ASAN_CC1_SPEC \
  "%{%:sanitize(address):-funwind-tables %{!m32:" ASAN_REJECT_SPEC "}}"
#endif

#undef  CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC GNU_USER_TARGET_CC1_SPEC ASAN_CC1_SPEC \
"%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m64:-mptr64 -mstack-bias -mlong-double-128 \
  %{!mcpu*:-mcpu=ultrasparc} \
  %{!mno-vis:%{!mcpu=v9:-mvis}}}"
#else
#define CC1_SPEC GNU_USER_TARGET_CC1_SPEC ASAN_CC1_SPEC \
"%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m32:-mptr32 -mno-stack-bias %{!mlong-double-128:-mlong-double-64} \
  %{!mcpu*:-mcpu=cypress}} \
%{mv8plus:-mptr32 -mno-stack-bias %{!mlong-double-128:-mlong-double-64} \
  %{!mcpu*:-mcpu=v9}} \
%{!m32:%{!mcpu*:-mcpu=ultrasparc}} \
%{!mno-vis:%{!m32:%{!mcpu=v9:-mvis}}}"
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-cpu is ignored if -mcpu is specified; likewise --with-cpu-32
     and --with-cpu-64.
   --with-tune is ignored if -mtune is specified; likewise --with-tune-32
     and --with-tune-64.
   --with-float is ignored if -mhard-float, -msoft-float, -mfpu, or -mno-fpu
     are specified.
   In the SPARC_BI_ARCH compiler we cannot pass %{!mcpu=*:-mcpu=%(VALUE)}
   here, otherwise say -mcpu=v7 would be passed even when -m64.
   CC1_SPEC above takes care of this instead.

   Note that the order of the cpu* and tune* options matters: the
   config.gcc file always sets with_cpu to some value, even if the
   user didn't use --with-cpu when invoking the configure script.
   This value is based on the target name.  Therefore we have to make
   sure that --with-cpu-32 takes precedence to --with-cpu in < v9
   systems, and that --with-cpu-64 takes precedence to --with-cpu in
   >= v9 systems.  As for the tune* options, in some platforms
   config.gcc also sets a default value for it if the user didn't use
   --with-tune when invoking the configure script.  */
#undef OPTION_DEFAULT_SPECS
#if DEFAULT_ARCH32_P
#define OPTION_DEFAULT_SPECS \
  {"cpu_32", "%{!m64:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"cpu_64", "%{m64:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"cpu", "%{!m64:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"tune_32", "%{!m64:%{!mtune=*:-mtune=%(VALUE)}}" }, \
  {"tune_64", "%{m64:%{!mtune=*:-mtune=%(VALUE)}}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:%{!mfpu:%{!mno-fpu:-m%(VALUE)-float}}}}" }
#else
#define OPTION_DEFAULT_SPECS \
  {"cpu_32", "%{m32:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"cpu_64", "%{!m32:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"cpu", "%{!m32:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"tune_32", "%{m32:%{!mtune=*:-mtune=%(VALUE)}}" },	\
  {"tune_64", "%{!m32:%{!mtune=*:-mtune=%(VALUE)}}" },	\
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:%{!mfpu:%{!mno-fpu:-m%(VALUE)-float}}}}" }
#endif

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#else /* !SPARC_BI_ARCH */

#undef LINK_SPEC
#define LINK_SPEC "-m elf64_sparc -Y P,%R/usr/lib64 %{shared:-shared} \
  %{!shared: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      -dynamic-linker " GNU_USER_DYNAMIC_LINKER64 "} \
    %{static:-static}} \
%{!mno-relax:%{!r:-relax}} \
"

#endif /* !SPARC_BI_ARCH */

/* It's safe to pass -s always, even if -g is not used.  */
#undef ASM_SPEC
#define ASM_SPEC "\
-s \
%{" FPIE_OR_FPIC_SPEC ":-K PIC} \
%{!.c:%{findirect-dispatch:-K PIC}} \
%(asm_cpu) %(asm_arch) %(asm_relax)"

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

/* DWARF bits.  */

/* Follow Irix 6 and not the Dwarf2 draft in using 64-bit offsets. 
   Obviously the Dwarf2 folks haven't tried to actually build systems
   with their spec.  On a 64-bit system, only 64-bit relocs become
   RELATIVE relocations.  */

/* #define DWARF_OFFSET_SIZE PTR_SIZE */

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

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#undef NEED_INDICATE_EXEC_STACK
#define NEED_INDICATE_EXEC_STACK 1

#ifdef TARGET_LIBC_PROVIDES_SSP
/* sparc glibc provides __stack_chk_guard in [%g7 + 0x14],
   sparc64 glibc provides it at [%g7 + 0x28].  */
#define TARGET_THREAD_SSP_OFFSET	(TARGET_ARCH64 ? 0x28 : 0x14)
#endif

/* Define if long doubles should be mangled as 'g'.  */
#define TARGET_ALTERNATE_LONG_DOUBLE_MANGLING

/* We use glibc _mcount for profiling.  */
#undef NO_PROFILE_COUNTERS
#define NO_PROFILE_COUNTERS	1
