/* Definitions for 64-bit PowerPC running FreeBSD using the ELF format
   Copyright (C) 2012-2025 Free Software Foundation, Inc.

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

/* Undef gnu-user.h macros we don't want.  */
#undef CPLUSPLUS_CPP_SPEC
#undef LINK_GCC_C_SEQUENCE_SPEC

/* Override the defaults, which exist to force the proper definition.  */

#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __powerpc64__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

#undef	TARGET_AIX
#define	TARGET_AIX TARGET_64BIT

#ifdef HAVE_LD_NO_DOT_SYMS
/* New ABI uses a local sym for the function entry point.  */
extern int dot_symbols;
#undef DOT_SYMBOLS
#define DOT_SYMBOLS dot_symbols
#endif

#define TARGET_USES_LINUX64_OPT 1
#ifdef HAVE_LD_LARGE_TOC
#undef TARGET_CMODEL
#define TARGET_CMODEL rs6000_current_cmodel
#define SET_CMODEL(opt) rs6000_current_cmodel = opt
#else
#define SET_CMODEL(opt) do {} while (0)
#endif

#undef  PROCESSOR_DEFAULT
#define PROCESSOR_DEFAULT PROCESSOR_PPC7450
#undef  PROCESSOR_DEFAULT64
#define PROCESSOR_DEFAULT64 PROCESSOR_POWER8

/* We don't need to generate entries in .fixup, except when
   -mrelocatable or -mrelocatable-lib is given.  */
#undef RELOCATABLE_NEEDS_FIXUP
#define RELOCATABLE_NEEDS_FIXUP \
  (rs6000_isa_flags & rs6000_isa_flags_explicit & OPTION_MASK_RELOCATABLE)

#undef  RS6000_ABI_NAME
#define RS6000_ABI_NAME "freebsd"

#define INVALID_64BIT "-m%s not supported in this configuration"
#define INVALID_32BIT INVALID_64BIT

/* Use LINUX64 instead of FREEBSD64 for compat with e.g. sysv4le.h */
#ifdef LINUX64_DEFAULT_ABI_ELFv2
#define ELFv2_ABI_CHECK (rs6000_elf_abi != 1)
#else
#define ELFv2_ABI_CHECK (rs6000_elf_abi == 2)
#endif

#undef  SUBSUBTARGET_OVERRIDE_OPTIONS
#define SUBSUBTARGET_OVERRIDE_OPTIONS				\
  do rs6000_linux64_override_options (); while (0)

#undef	ASM_SPEC
#undef	LINK_OS_FREEBSD_SPEC

#define	ASM_SPEC	 "%{m32:%(asm_spec32)}%{!m32:%(asm_spec64)} %(asm_spec_common)"
#define	LINK_OS_FREEBSD_SPEC "%{m32:%(link_os_freebsd_spec32)}%{!m32:%(link_os_freebsd_spec64)}"

#define ASM_SPEC32 "-a32 \
%{mrelocatable} %{mrelocatable-lib} %{" FPIE_OR_FPIC_SPEC ":-K PIC} \
%{memb} %{!memb: %{msdata=eabi: -memb}} \
%{!mlittle: %{!mlittle-endian: %{!mbig: %{!mbig-endian: \
    %{mcall-freebsd: -mbig} \
    %{mcall-i960-old: -mlittle} \
    %{mcall-linux: -mbig} \
    %{mcall-gnu: -mbig} \
    %{mcall-netbsd: -mbig} \
}}}}"

#define ASM_SPEC64 "-a64"

#define ASM_SPEC_COMMON "%(asm_cpu) \
%{,assembler|,assembler-with-cpp: %{mregnames} %{mno-regnames}}" \
  ENDIAN_SELECT(" -mbig", " -mlittle", DEFAULT_ASM_ENDIAN)

#undef	SUBSUBTARGET_EXTRA_SPECS
#define SUBSUBTARGET_EXTRA_SPECS					\
  { "asm_spec_common",		ASM_SPEC_COMMON },			\
  { "asm_spec32",		ASM_SPEC32 },				\
  { "asm_spec64",		ASM_SPEC64 },				\
  { "link_os_freebsd_spec32",	LINK_OS_FREEBSD_SPEC32 },     		\
  { "link_os_freebsd_spec64",	LINK_OS_FREEBSD_SPEC64 },

#define LINK_OS_FREEBSD_SPEC_DEF "\
  %{p:%nconsider using `-pg' instead of `-p' with gprof(1)} \
  " FBSD_LINK_PG_NOTE " \
  %{v:-V} \
  %{assert*} %{R*} %{rpath*} %{defsym*} \
  %{shared:-Bshareable %{h*} %{soname*}} \
  %{!shared: \
    %{!static: \
      %{rdynamic: -export-dynamic} \
      %{!dynamic-linker:-dynamic-linker " FBSD_DYNAMIC_LINKER "}} \
    %{static:-Bstatic}} \
  %{symbolic:-Bsymbolic}"

#undef  DEFAULT_ASM_ENDIAN
#define LINK_OS_FREEBSD_SPEC32 "-melf32ppc_fbsd " LINK_OS_FREEBSD_SPEC_DEF
#if (TARGET_DEFAULT & MASK_LITTLE_ENDIAN)
#define DEFAULT_ASM_ENDIAN " -mlittle"
#define LINK_OS_FREEBSD_SPEC64 "-melf64lppc_fbsd " LINK_OS_FREEBSD_SPEC_DEF
#else
#define DEFAULT_ASM_ENDIAN " -mbig"
#define LINK_OS_FREEBSD_SPEC64 "-melf64ppc_fbsd " LINK_OS_FREEBSD_SPEC_DEF
#endif

#undef	MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "m64" }

/* PowerPC-64 FreeBSD increases natural record alignment to doubleword if
   the first field is an FP double, only if in power alignment mode.  */
#undef  ROUND_TYPE_ALIGN
#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED)			\
  ((TARGET_64BIT							\
    && RECORD_OR_UNION_TYPE_P (STRUCT)			\
    && TARGET_ALIGN_NATURAL == 0)					\
   ? rs6000_special_round_type_align (STRUCT, COMPUTED, SPECIFIED)	\
   : MAX ((COMPUTED), (SPECIFIED)))

/* Use the default for compiling target libs.  */
#ifdef IN_TARGET_LIBS
#undef TARGET_ALIGN_NATURAL
#define TARGET_ALIGN_NATURAL 1
#endif

/* Indicate that jump tables go in the text section.  */
#undef  JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION TARGET_64BIT

/* The linux ppc64 ABI isn't explicit on whether aggregates smaller
   than a doubleword should be padded upward or downward.  You could
   reasonably assume that they follow the normal rules for structure
   layout treating the parameter area as any other block of memory,
   then map the reg param area to registers.  i.e. pad upward.
   Setting both of the following defines results in this behavior.
   Setting just the first one will result in aggregates that fit in a
   doubleword being padded downward, and others being padded upward.
   Not a bad idea as this results in struct { int x; } being passed
   the same way as an int.  */
#define AGGREGATE_PADDING_FIXED TARGET_64BIT
#define AGGREGATES_PAD_UPWARD_ALWAYS 0

/* Specify padding for the last element of a block move between
   registers and memory.  FIRST is nonzero if this is the only
   element.  */
#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  (!(FIRST) ? PAD_UPWARD : targetm.calls.function_arg_padding (MODE, TYPE))

/* FreeBSD doesn't support saving and restoring 64-bit regs with a 32-bit
   kernel. This is supported when running on a 64-bit kernel with
   COMPAT_FREEBSD32, but tell GCC it isn't so that our 32-bit binaries
   are compatible. */
#define OS_MISSING_POWERPC64 !TARGET_64BIT

#undef  FBSD_TARGET_CPU_CPP_BUILTINS
#define FBSD_TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__PPC__");			\
      builtin_define ("__ppc__");			\
      builtin_define ("__powerpc__");			\
      if (TARGET_64BIT)					\
	{						\
	  builtin_define ("__arch64__");		\
	  builtin_define ("__LP64__");			\
	  builtin_define ("__PPC64__");			\
	  builtin_define ("__powerpc64__");		\
	  builtin_assert ("cpu=powerpc64");		\
	  builtin_assert ("machine=powerpc64");		\
	}						\
      else						\
	{						\
	  builtin_define_std ("PPC");			\
	  builtin_define_std ("powerpc");		\
	  builtin_assert ("cpu=powerpc");		\
	  builtin_assert ("machine=powerpc");		\
	  TARGET_OS_SYSV_CPP_BUILTINS ();		\
	}						\
    }							\
  while (0)

#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_freebsd)"

#undef CPP_OS_FREEBSD_SPEC
#define CPP_OS_FREEBSD_SPEC ""

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_freebsd)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_freebsd)"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_freebsd)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_freebsd)"

#undef	LINK_OS_DEFAULT_SPEC
#define	LINK_OS_DEFAULT_SPEC "%(link_os_freebsd)"

/* XXX: This is wrong for many platforms in sysv4.h.
   We should work on getting that definition fixed.  */
#undef  LINK_SHLIB_SPEC
#define LINK_SHLIB_SPEC "%{shared:-shared} %{!shared: %{static:-static}}"


/************************[  Target stuff  ]***********************************/

/* Define the actual types of some ANSI-mandated types.
   Needs to agree with <machine/ansi.h>.  GCC defaults come from c-decl.cc,
   c-common.cc, and config/<arch>/<arch>.h.  */


#undef  SIZE_TYPE
#define SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "unsigned int")

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE	(TARGET_64BIT ? "long int" : "int")

/* rs6000.h gets this wrong for FreeBSD.  We use the GCC defaults instead.  */
#undef WCHAR_TYPE

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Function profiling bits */
#undef  RS6000_MCOUNT
#define RS6000_MCOUNT "_mcount"

#define PROFILE_HOOK(LABEL) \
  do { if (TARGET_64BIT) output_profile_hook (LABEL); } while (0)

/* _init and _fini functions are built from bits spread across many
   object files, each potentially with a different TOC pointer.  For
   that reason, place a nop after the call so that the linker can
   restore the TOC pointer if a TOC adjusting call stub is needed.  */
#ifdef __powerpc64__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
  asm (SECTION_OP "\n"					\
"	bl " #FUNC "\n"					\
"	nop\n"						\
"	.previous");
#endif

/* FP save and restore routines.  */
#undef  SAVE_FP_PREFIX
#define SAVE_FP_PREFIX (TARGET_64BIT ? "._savef" : "_savefpr_")
#undef  SAVE_FP_SUFFIX
#define SAVE_FP_SUFFIX ""
#undef  RESTORE_FP_PREFIX
#define RESTORE_FP_PREFIX (TARGET_64BIT ? "._restf" : "_restfpr_")
#undef  RESTORE_FP_SUFFIX
#define RESTORE_FP_SUFFIX ""

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  */
#undef	ASM_PREFERRED_EH_DATA_FORMAT
#define	ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (TARGET_64BIT || flag_pic						\
   ? (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel		\
      | (TARGET_64BIT ? DW_EH_PE_udata8 : DW_EH_PE_sdata4))		\
   : DW_EH_PE_absptr)

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

/* The default value isn't sufficient in 64-bit mode.  */
#define STACK_CHECK_PROTECT (TARGET_64BIT ? 16 * 1024 : 12 * 1024)

/* Use standard DWARF numbering for DWARF debugging information.  */
#define RS6000_USE_DWARF_NUMBERING

/* PowerPC64 Linux word-aligns FP doubles when -malign-power is given.  */
#undef  ADJUST_FIELD_ALIGN
#define ADJUST_FIELD_ALIGN(FIELD, TYPE, COMPUTED) \
  ((TARGET_64BIT							\
    && TARGET_ALIGN_NATURAL == 0					\
    && TYPE_MODE (strip_array_types (TYPE)) == DFmode)   		\
   ? MIN ((COMPUTED), 32)						\
   : (COMPUTED))

#undef  TOC_SECTION_ASM_OP
#define TOC_SECTION_ASM_OP \
  (TARGET_64BIT                                         \
   ? "\t.section\t\".toc\",\"aw\""                      \
   : "\t.section\t\".got\",\"aw\"")

#undef  MINIMAL_TOC_SECTION_ASM_OP
#define MINIMAL_TOC_SECTION_ASM_OP \
  (TARGET_64BIT                                         \
   ? "\t.section\t\".toc1\",\"aw\""                     \
   : (flag_pic						\
      ? "\t.section\t\".got2\",\"aw\""                  \
      : "\t.section\t\".got1\",\"aw\""))

/* This is how to declare the size of a function.  */
#undef  ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)                    \
  do                                                                    \
    {                                                                   \
      if (!flag_inhibit_size_directive)                                 \
        {                                                               \
          fputs ("\t.size\t", (FILE));                                  \
          if (TARGET_64BIT && DOT_SYMBOLS)                              \
            putc ('.', (FILE));                                         \
          assemble_name ((FILE), (FNAME));                              \
          fputs (",.-", (FILE));                                        \
          rs6000_output_function_entry (FILE, FNAME);                   \
          putc ('\n', (FILE));                                          \
        }                                                               \
    }                                                                   \
  while (0)

#undef  ASM_OUTPUT_SPECIAL_POOL_ENTRY_P
#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)                        \
  (TARGET_TOC                                                           \
   && (SYMBOL_REF_P (X)							\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS      \
           && SYMBOL_REF_P (XEXP (XEXP (X, 0), 0)))			\
       || GET_CODE (X) == LABEL_REF                                     \
       || (CONST_INT_P (X)						\
           && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))      \
       || (CONST_DOUBLE_P (X)						\
           && ((TARGET_64BIT                                            \
                && (TARGET_MINIMAL_TOC                                  \
                    || (SCALAR_FLOAT_MODE_P (GET_MODE (X))              \
                        && ! TARGET_NO_FP_IN_TOC)))                     \
               || (!TARGET_64BIT                                        \
                   && !TARGET_NO_FP_IN_TOC                              \
                   && SCALAR_FLOAT_MODE_P (GET_MODE (X))                \
                   && BITS_PER_WORD == HOST_BITS_PER_INT)))))

/* Use --as-needed -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

#define POWERPC_FREEBSD
