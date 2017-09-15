/* Definitions for rtems targeting a PowerPC using elf.
   Copyright (C) 1996-2017 Free Software Foundation, Inc.
   Contributed by Joel Sherrill (joel@OARcorp.com).

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

/* Copy and paste from linux64.h and freebsd64.h */
#ifdef IN_LIBGCC2
#undef TARGET_64BIT
#ifdef __powerpc64__
#define TARGET_64BIT 1
#else
#define TARGET_64BIT 0
#endif
#endif

/* Copy and paste from linux64.h and freebsd64.h */
#undef	TARGET_AIX
#define	TARGET_AIX TARGET_64BIT

/* Simplified copy and paste from linux64.h and freebsd64.h */
#undef DOT_SYMBOLS
#define DOT_SYMBOLS 0

/* Copy and paste from linux64.h and freebsd64.h */
#undef TARGET_CMODEL
#define TARGET_CMODEL rs6000_current_cmodel
#define SET_CMODEL(opt) rs6000_current_cmodel = opt

#undef TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()			\
  do							\
    {							\
      builtin_define ("__rtems__");			\
      builtin_define ("__USE_INIT_FINI__");		\
      builtin_assert ("system=rtems");			\
      if (TARGET_64BIT)					\
	{						\
	  builtin_define ("__PPC__");			\
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

/* Copy and paste from linux64.h and freebsd64.h */
#undef RELOCATABLE_NEEDS_FIXUP
#define RELOCATABLE_NEEDS_FIXUP \
  (rs6000_isa_flags & rs6000_isa_flags_explicit & OPTION_MASK_RELOCATABLE)

/* Copy and paste from linux64.h */
#undef	RS6000_ABI_NAME
#define	RS6000_ABI_NAME "linux"

/* Copy and paste from linux64.h and freebsd64.h */
#define INVALID_64BIT "-m%s not supported in this configuration"

/* A lot of copy and paste from linux64.h and freebsd64.h */
#undef	SUBSUBTARGET_OVERRIDE_OPTIONS
#define	SUBSUBTARGET_OVERRIDE_OPTIONS				\
  do								\
    {								\
      if (rs6000_isa_flags & OPTION_MASK_64BIT)			\
	{							\
	  rs6000_elf_abi = 2;					\
	  rs6000_current_abi = ABI_ELFv2;			\
	  if (rs6000_isa_flags & OPTION_MASK_RELOCATABLE)	\
	    {							\
	      rs6000_isa_flags &= ~OPTION_MASK_RELOCATABLE;	\
	      error (INVALID_64BIT, "relocatable");		\
	    }							\
	  if (rs6000_isa_flags & OPTION_MASK_EABI)		\
	    {							\
	      rs6000_isa_flags &= ~OPTION_MASK_EABI;		\
	      error (INVALID_64BIT, "eabi");			\
	    }							\
	  if (TARGET_PROTOTYPE)					\
	    {							\
	      target_prototype = 0;				\
	      error (INVALID_64BIT, "prototype");		\
	    }							\
	  if ((rs6000_isa_flags & OPTION_MASK_POWERPC64) == 0)	\
	    {							\
	      rs6000_isa_flags |= OPTION_MASK_POWERPC64;	\
	      error ("-m64 requires a PowerPC64 cpu");		\
	    }							\
	  if ((rs6000_isa_flags_explicit			\
		& OPTION_MASK_MINIMAL_TOC) != 0)		\
	    {							\
	      if (global_options_set.x_rs6000_current_cmodel	\
		  && rs6000_current_cmodel != CMODEL_SMALL)	\
		error ("-mcmodel incompatible with other toc options"); \
	      SET_CMODEL (CMODEL_SMALL);			\
	    }							\
	  else							\
	    {							\
	      if (!global_options_set.x_rs6000_current_cmodel)	\
		SET_CMODEL (CMODEL_MEDIUM);			\
	      if (rs6000_current_cmodel != CMODEL_SMALL)	\
		{						\
		  TARGET_NO_FP_IN_TOC = 0;			\
		  TARGET_NO_SUM_IN_TOC = 0;			\
		}						\
	    }							\
	}							\
    }								\
  while (0)

#undef TARGET_LIBGCC_SDATA_SECTION
#define TARGET_LIBGCC_SDATA_SECTION ".sdata"

/* Copy and paste from linux64.h and freebsd64.h */
#undef	SIZE_TYPE
#define	SIZE_TYPE (TARGET_64BIT ? "long unsigned int" : "unsigned int")

/* Copy and paste from linux64.h and freebsd64.h */
#undef	PTRDIFF_TYPE
#define	PTRDIFF_TYPE (TARGET_64BIT ? "long int" : "int")

/* Copy and paste from freebsd64.h */
#undef WCHAR_TYPE

/* Copy and paste from freebsd64.h */
#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Copy and paste from linux64.h and freebsd64.h */
#ifdef __powerpc64__
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)	\
  asm (SECTION_OP "\n"					\
"	bl " #FUNC "\n"					\
"	nop\n"						\
"	.previous");
#endif

/* This could be also POWERPC_FREEBSD.  It is related to the save/restore
   defines below.  */
#define POWERPC_LINUX

/* Copy and paste from linux64.h and freebsd64.h */
#undef  SAVE_FP_PREFIX
#define SAVE_FP_PREFIX (TARGET_64BIT ? "._savef" : "_savefpr_")
#undef  SAVE_FP_SUFFIX
#define SAVE_FP_SUFFIX ""
#undef  RESTORE_FP_PREFIX
#define RESTORE_FP_PREFIX (TARGET_64BIT ? "._restf" : "_restfpr_")
#undef  RESTORE_FP_SUFFIX
#define RESTORE_FP_SUFFIX ""

/* Copy and paste from linux64.h and freebsd64.h */
#undef	ASM_PREFERRED_EH_DATA_FORMAT
#define	ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (TARGET_64BIT || flag_pic						\
   ? (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel		\
      | (TARGET_64BIT ? DW_EH_PE_udata8 : DW_EH_PE_sdata4))		\
   : DW_EH_PE_absptr)

/* Copy and paste from linux64.h and freebsd64.h */
#undef  TOC_SECTION_ASM_OP
#define TOC_SECTION_ASM_OP \
  (TARGET_64BIT						\
   ? "\t.section\t\".toc\",\"aw\""			\
   : "\t.section\t\".got\",\"aw\"")

/* Copy and paste from linux64.h and freebsd64.h */
#undef  MINIMAL_TOC_SECTION_ASM_OP
#define MINIMAL_TOC_SECTION_ASM_OP \
  (TARGET_64BIT						\
   ? "\t.section\t\".toc1\",\"aw\""			\
   : (flag_pic						\
      ? "\t.section\t\".got2\",\"aw\""			\
      : "\t.section\t\".got1\",\"aw\""))

/* Copy and paste from linux64.h and freebsd64.h */
#undef	ASM_DECLARE_FUNCTION_SIZE
#define	ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)			\
  do									\
    {									\
      if (!flag_inhibit_size_directive)					\
	{								\
	  fputs ("\t.size\t", (FILE));					\
	  if (TARGET_64BIT && DOT_SYMBOLS)				\
	    putc ('.', (FILE));						\
	  assemble_name ((FILE), (FNAME));				\
	  fputs (",.-", (FILE));					\
	  rs6000_output_function_entry (FILE, FNAME);			\
	  putc ('\n', (FILE));						\
	}								\
    }									\
  while (0)

/* Copy and paste from linux64.h and freebsd64.h */
#undef  ASM_OUTPUT_SPECIAL_POOL_ENTRY_P
#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)			\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (GET_CODE (X) == CONST_INT 					\
	   && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))	\
       || (GET_CODE (X) == CONST_DOUBLE					\
	   && ((TARGET_64BIT						\
		&& (TARGET_MINIMAL_TOC					\
		    || (SCALAR_FLOAT_MODE_P (GET_MODE (X))		\
			&& ! TARGET_NO_FP_IN_TOC)))			\
	       || (!TARGET_64BIT					\
		   && !TARGET_NO_FP_IN_TOC				\
		   && SCALAR_FLOAT_MODE_P (GET_MODE (X))		\
		   && BITS_PER_WORD == HOST_BITS_PER_INT)))))

#undef CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "\
%{!mcpu*:  %{!Dppc*: %{!Dmpc*: -Dmpc750} } }\
%{mcpu=403:  %{!Dppc*: %{!Dmpc*: -Dppc403}  } } \
%{mcpu=505:  %{!Dppc*: %{!Dmpc*: -Dmpc505}  } } \
%{mcpu=601:  %{!Dppc*: %{!Dmpc*: -Dppc601}  } } \
%{mcpu=602:  %{!Dppc*: %{!Dmpc*: -Dppc602}  } } \
%{mcpu=603:  %{!Dppc*: %{!Dmpc*: -Dppc603}  } } \
%{mcpu=603e: %{!Dppc*: %{!Dmpc*: -Dppc603e} } } \
%{mcpu=604:  %{!Dppc*: %{!Dmpc*: -Dmpc604}  } } \
%{mcpu=750:  %{!Dppc*: %{!Dmpc*: -Dmpc750}  } } \
%{mcpu=821:  %{!Dppc*: %{!Dmpc*: -Dmpc821}  } } \
%{mcpu=860:  %{!Dppc*: %{!Dmpc*: -Dmpc860}  } } \
%{mcpu=8540: %{!Dppc*: %{!Dmpc*: -Dppc8540}  } } \
%{mcpu=e6500: -D__PPC_CPU_E6500__}"

#undef	ASM_DEFAULT_SPEC
#define	ASM_DEFAULT_SPEC "-mppc%{m64:64}"

#undef	ASM_SPEC
#define	ASM_SPEC "%{!m64:%(asm_spec32)}%{m64:%(asm_spec64)} %(asm_spec_common)"

#define ASM_SPEC32 "-a32 \
%{mrelocatable} %{mrelocatable-lib} %{" FPIE_OR_FPIC_SPEC ":-K PIC} \
%{memb|msdata=eabi: -memb}"

#define ASM_SPEC64 "-a64"

#define ASM_SPEC_COMMON "%(asm_cpu) \
%{,assembler|,assembler-with-cpp: %{mregnames} %{mno-regnames}}" \
  ENDIAN_SELECT(" -mbig", " -mlittle", DEFAULT_ASM_ENDIAN)

#undef  LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC \
"%{!m64:%(link_os_spec32)}%{m64:%(link_os_spec64)}"

#define LINK_OS_SPEC32 ENDIAN_SELECT(" -m elf32ppc",		\
				     " -m elf32lppc",		\
				     " -m elf32ppc")
#define LINK_OS_SPEC64 ENDIAN_SELECT(" -m elf64ppc",		\
				     " -m elf64lppc",		\
				     " -m elf64ppc")

#undef  SUBSUBTARGET_EXTRA_SPECS
#define SUBSUBTARGET_EXTRA_SPECS \
  { "asm_spec_common",		ASM_SPEC_COMMON },			\
  { "asm_spec32",		ASM_SPEC32 },				\
  { "asm_spec64",		ASM_SPEC64 },				\
  { "link_os_spec32",		LINK_OS_SPEC32 },			\
  { "link_os_spec64",		LINK_OS_SPEC64 },
