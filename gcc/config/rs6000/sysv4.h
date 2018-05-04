/* Target definitions for GNU compiler for PowerPC running System V.4
   Copyright (C) 1995-2018 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

/* Header files should be C++ aware in general.  */
#undef  NO_IMPLICIT_EXTERN_C
#define NO_IMPLICIT_EXTERN_C

/* Yes!  We are ELF.  */
#define	TARGET_OBJECT_FORMAT OBJECT_ELF

/* Default ABI to compile code for.  */
#define DEFAULT_ABI rs6000_current_abi

/* Default ABI to use.  */
#define RS6000_ABI_NAME "sysv"

/* Override rs6000.h definition.  */
#undef	ASM_DEFAULT_SPEC
#define	ASM_DEFAULT_SPEC "-mppc"

#define	TARGET_TOC		(TARGET_64BIT				\
				 || (TARGET_MINIMAL_TOC			\
				     && flag_pic > 1)			\
				 || DEFAULT_ABI != ABI_V4)

#define	TARGET_BITFIELD_TYPE	(! TARGET_NO_BITFIELD_TYPE)
#define	TARGET_BIG_ENDIAN	(! TARGET_LITTLE_ENDIAN)
#define	TARGET_PROTOTYPE	target_prototype
#define	TARGET_NO_PROTOTYPE	(! TARGET_PROTOTYPE)
#define	TARGET_NO_TOC		(! TARGET_TOC)
#define	TARGET_NO_EABI		(! TARGET_EABI)
#define	TARGET_REGNAMES		rs6000_regnames

#ifdef HAVE_AS_REL16
#undef TARGET_SECURE_PLT
#define TARGET_SECURE_PLT	secure_plt
#endif

#define SDATA_DEFAULT_SIZE 8

/* The macro SUBTARGET_OVERRIDE_OPTIONS is provided for subtargets, to
   get control in TARGET_OPTION_OVERRIDE.  */

#define SUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if (!global_options_set.x_g_switch_value)				\
    g_switch_value = SDATA_DEFAULT_SIZE;				\
									\
  if (rs6000_abi_name == NULL)						\
    rs6000_abi_name = RS6000_ABI_NAME;					\
									\
  if (!strcmp (rs6000_abi_name, "sysv"))				\
    rs6000_current_abi = ABI_V4;					\
  else if (!strcmp (rs6000_abi_name, "sysv-noeabi"))			\
    {									\
      rs6000_current_abi = ABI_V4;					\
      rs6000_isa_flags &= ~ OPTION_MASK_EABI;				\
    }									\
  else if (!strcmp (rs6000_abi_name, "sysv-eabi")			\
	   || !strcmp (rs6000_abi_name, "eabi"))			\
    {									\
      rs6000_current_abi = ABI_V4;					\
      rs6000_isa_flags |= OPTION_MASK_EABI;				\
    }									\
  else if (!strcmp (rs6000_abi_name, "aixdesc"))			\
    rs6000_current_abi = ABI_AIX;					\
  else if (!strcmp (rs6000_abi_name, "freebsd")				\
	   || !strcmp (rs6000_abi_name, "linux"))			\
    {									\
      if (TARGET_64BIT)							\
	rs6000_current_abi = ABI_AIX;					\
      else								\
	rs6000_current_abi = ABI_V4;					\
    }									\
  else if (!strcmp (rs6000_abi_name, "netbsd"))				\
    rs6000_current_abi = ABI_V4;					\
  else if (!strcmp (rs6000_abi_name, "openbsd"))			\
    rs6000_current_abi = ABI_V4;					\
  else if (!strcmp (rs6000_abi_name, "i960-old"))			\
    {									\
      rs6000_current_abi = ABI_V4;					\
      rs6000_isa_flags |= (OPTION_MASK_LITTLE_ENDIAN | OPTION_MASK_EABI); \
      rs6000_isa_flags &= ~OPTION_MASK_STRICT_ALIGN;			\
      TARGET_NO_BITFIELD_WORD = 1;					\
    }									\
  else									\
    {									\
      rs6000_current_abi = ABI_V4;					\
      error ("bad value for %<%s-%s%>", "-mcall", rs6000_abi_name);	\
    }									\
									\
  if (rs6000_sdata_name)						\
    {									\
      if (!strcmp (rs6000_sdata_name, "none"))				\
	rs6000_sdata = SDATA_NONE;					\
      else if (!strcmp (rs6000_sdata_name, "data"))			\
	rs6000_sdata = SDATA_DATA;					\
      else if (!strcmp (rs6000_sdata_name, "default"))			\
	rs6000_sdata = (TARGET_EABI) ? SDATA_EABI : SDATA_SYSV;		\
      else if (!strcmp (rs6000_sdata_name, "sysv"))			\
	rs6000_sdata = SDATA_SYSV;					\
      else if (!strcmp (rs6000_sdata_name, "eabi"))			\
	rs6000_sdata = SDATA_EABI;					\
      else								\
	error ("bad value for %<%s=%s%>", "-msdata", rs6000_sdata_name);\
    }									\
  else if (DEFAULT_ABI == ABI_V4)					\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      rs6000_sdata_name = "data";					\
    }									\
  else									\
    {									\
      rs6000_sdata = SDATA_NONE;					\
      rs6000_sdata_name = "none";					\
    }									\
									\
  if (TARGET_RELOCATABLE &&						\
      (rs6000_sdata == SDATA_EABI || rs6000_sdata == SDATA_SYSV))	\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      error ("%qs and %<%s=%s%> are incompatible", rs6000_sdata_name,	\
	     "-mrelocatable", "-msdata");				\
    }									\
									\
  else if (flag_pic && DEFAULT_ABI == ABI_V4				\
	   && (rs6000_sdata == SDATA_EABI				\
	       || rs6000_sdata == SDATA_SYSV))				\
    {									\
      rs6000_sdata = SDATA_DATA;					\
      error ("%<-f%s%> and %<%s=%s%> are incompatible",			\
	     (flag_pic > 1) ? "PIC" : "pic",				\
	     "-msdata", rs6000_sdata_name);				\
    }									\
									\
  if ((rs6000_sdata != SDATA_NONE && DEFAULT_ABI != ABI_V4)		\
      || (rs6000_sdata == SDATA_EABI && !TARGET_EABI))			\
    {									\
      rs6000_sdata = SDATA_NONE;					\
      error ("%<%s=%s%> and %<%s-%s%> are incompatible",		\
	     "-msdata", rs6000_sdata_name, "-mcall", rs6000_abi_name);	\
    }									\
									\
  targetm.have_srodata_section = rs6000_sdata == SDATA_EABI;		\
									\
  if (TARGET_RELOCATABLE && !TARGET_MINIMAL_TOC)			\
    {									\
      rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;			\
      error ("%qs and %qs are incompatible", "-mrelocatable",		\
	     "-mno-minimal-toc");					\
    }									\
									\
  if (TARGET_RELOCATABLE && rs6000_current_abi != ABI_V4)		\
    {									\
      rs6000_isa_flags &= ~OPTION_MASK_RELOCATABLE;			\
      error ("%qs and %<%s-%s%> are incompatible",			\
	     "-mrelocatable", "-mcall", rs6000_abi_name);		\
    }									\
									\
  if (!TARGET_64BIT && flag_pic > 1 && rs6000_current_abi != ABI_V4)	\
    {									\
      flag_pic = 0;							\
      error ("%qs and %<%s-%s%> are incompatible",			\
	     "-fPIC", "-mcall", rs6000_abi_name);			\
    }									\
									\
  if (TARGET_SECURE_PLT != secure_plt)					\
    {									\
      error ("%qs not supported by your assembler", "-msecure-plt");	\
    }									\
									\
  if (flag_pic > 1 && DEFAULT_ABI == ABI_V4)				\
    {									\
      /* Note: flag_pic should not change any option flags that would	\
	 be invalid with or pessimise -fno-PIC code.  LTO turns off	\
	 flag_pic when linking/recompiling a fixed position executable. \
	 However, if the objects were originally compiled with -fPIC,	\
	 then other target options forced on here by -fPIC are restored \
	 when recompiling those objects without -fPIC.  In particular	\
	 TARGET_RELOCATABLE must not be enabled here by flag_pic.  */	\
      rs6000_isa_flags |= OPTION_MASK_MINIMAL_TOC;			\
      TARGET_NO_FP_IN_TOC = 1;						\
    }									\
									\
  if (TARGET_RELOCATABLE)						\
    {									\
      if (!flag_pic)							\
	flag_pic = 2;							\
      TARGET_NO_FP_IN_TOC = 1;						\
    }									\
} while (0)

#ifndef RS6000_BI_ARCH
# define SUBSUBTARGET_OVERRIDE_OPTIONS					\
do {									\
  if ((TARGET_DEFAULT ^ rs6000_isa_flags) & OPTION_MASK_64BIT)		\
    error ("%<-m%s%> not supported in this configuration",		\
	   (rs6000_isa_flags & OPTION_MASK_64BIT) ? "64" : "32");	\
} while (0)
#endif

/* Override rs6000.h definition.  */
#undef	TARGET_DEFAULT
#define	TARGET_DEFAULT 0

/* Override rs6000.h definition.  */
#undef	PROCESSOR_DEFAULT
#define	PROCESSOR_DEFAULT PROCESSOR_PPC750

#define FIXED_R2 1
/* System V.4 uses register 13 as a pointer to the small data area,
   so it is not available to the normal user.  */
#define FIXED_R13 1

/* Override default big endianism definitions in rs6000.h.  */
#undef	BYTES_BIG_ENDIAN
#undef	WORDS_BIG_ENDIAN
#define	BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN)
#define	WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN)

/* Put jump tables in read-only memory, rather than in .text.  */
#define JUMP_TABLES_IN_TEXT_SECTION 0

/* Prefix and suffix to use to saving floating point.  */
#define	SAVE_FP_PREFIX "_savefpr_"
#define SAVE_FP_SUFFIX ""

/* Prefix and suffix to use to restoring floating point.  */
#define	RESTORE_FP_PREFIX "_restfpr_"
#define RESTORE_FP_SUFFIX ""

/* Type used for size_t, as a string used in a declaration.  */
#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

/* Type used for ptrdiff_t, as a string used in a declaration.  */
#define PTRDIFF_TYPE "int"

#undef	WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Make int foo : 8 not cause structures to be aligned to an int boundary.  */
/* Override elfos.h definition.  */
#undef	PCC_BITFIELD_TYPE_MATTERS
#define	PCC_BITFIELD_TYPE_MATTERS (TARGET_BITFIELD_TYPE)

#undef	BITFIELD_NBYTES_LIMITED
#define	BITFIELD_NBYTES_LIMITED (TARGET_NO_BITFIELD_WORD)

/* Define this macro to be the value 1 if instructions will fail to
   work if given data not on the nominal alignment.  If instructions
   will merely go slower in that case, define this macro as 0.  */
#undef	STRICT_ALIGNMENT
#define	STRICT_ALIGNMENT (TARGET_STRICT_ALIGN)

/* Define this macro if you wish to preserve a certain alignment for
   the stack pointer, greater than what the hardware enforces.  The
   definition is a C expression for the desired alignment (measured
   in bits).  This macro must evaluate to a value equal to or larger
   than STACK_BOUNDARY.
   For the SYSV ABI and variants the alignment of the stack pointer
   is usually controlled manually in rs6000.c. However, to maintain
   alignment across alloca () in all circumstances,
   PREFERRED_STACK_BOUNDARY needs to be set as well.
   This has the additional advantage of allowing a bigger maximum
   alignment of user objects on the stack.  */

#undef PREFERRED_STACK_BOUNDARY
#define PREFERRED_STACK_BOUNDARY 128

/* Real stack boundary as mandated by the appropriate ABI.  */
#define ABI_STACK_BOUNDARY \
  ((TARGET_EABI && !TARGET_ALTIVEC && !TARGET_ALTIVEC_ABI) ? 64 : 128)

/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way is COMPUTED.  */
#define ADJUST_FIELD_ALIGN(FIELD, TYPE, COMPUTED)			      \
	(rs6000_special_adjust_field_align_p ((TYPE), (COMPUTED))	      \
	 ? 128 : COMPUTED)

#undef  BIGGEST_FIELD_ALIGNMENT

/* Use ELF style section commands.  */

#define	TEXT_SECTION_ASM_OP	"\t.section\t\".text\""

#define	DATA_SECTION_ASM_OP	"\t.section\t\".data\""

#define	BSS_SECTION_ASM_OP	"\t.section\t\".bss\""

/* Override elfos.h definition.  */
#undef	INIT_SECTION_ASM_OP
#define	INIT_SECTION_ASM_OP "\t.section\t\".init\",\"ax\""

/* Override elfos.h definition.  */
#undef	FINI_SECTION_ASM_OP
#define	FINI_SECTION_ASM_OP "\t.section\t\".fini\",\"ax\""

#define	TOC_SECTION_ASM_OP "\t.section\t\".got\",\"aw\""

/* Put PC relative got entries in .got2.  */
#define	MINIMAL_TOC_SECTION_ASM_OP \
  (flag_pic ? "\t.section\t\".got2\",\"aw\"" : "\t.section\t\".got1\",\"aw\"")

#define	SDATA_SECTION_ASM_OP "\t.section\t\".sdata\",\"aw\""
#define	SDATA2_SECTION_ASM_OP "\t.section\t\".sdata2\",\"a\""
#define	SBSS_SECTION_ASM_OP "\t.section\t\".sbss\",\"aw\",@nobits"

/* Override default elf definitions.  */
#define TARGET_ASM_INIT_SECTIONS rs6000_elf_asm_init_sections
#undef  TARGET_ASM_RELOC_RW_MASK
#define TARGET_ASM_RELOC_RW_MASK rs6000_elf_reloc_rw_mask
#undef	TARGET_ASM_SELECT_RTX_SECTION
#define	TARGET_ASM_SELECT_RTX_SECTION rs6000_elf_select_rtx_section

/* Return nonzero if this entry is to be written into the constant pool
   in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF or a CONST
   containing one of them.  If -mfp-in-toc (the default), we also do
   this for floating-point constants.  We actually can only do this
   if the FP formats of the target and host machines are the same, but
   we can't check that since not every file that uses these target macros
   includes real.h.

   Unlike AIX, we don't key off of -mminimal-toc, but instead do not
   allow floating point constants in the TOC if -mrelocatable.  */

#undef	ASM_OUTPUT_SPECIAL_POOL_ENTRY_P
#define	ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)			\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (GET_CODE (X) == CONST_INT 					\
	   && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))	\
       || (!TARGET_NO_FP_IN_TOC						\
	   && GET_CODE (X) == CONST_DOUBLE				\
	   && SCALAR_FLOAT_MODE_P (GET_MODE (X))			\
	   && BITS_PER_WORD == HOST_BITS_PER_INT)))

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare a function properly.
   Some svr4 assemblers need to also have something extra said about the
   function's return value.  We allow for that here.  */

/* Override elfos.h definition.  */
#undef	ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  rs6000_elf_declare_function_name ((FILE), (NAME), (DECL))

/* The USER_LABEL_PREFIX stuff is affected by the -fleading-underscore
   flag.  The LOCAL_LABEL_PREFIX variable is used by dbxelf.h.  */

#define	LOCAL_LABEL_PREFIX "."
#define	USER_LABEL_PREFIX ""

#define	ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)	\
  asm_fprintf (FILE, "%L%s", PREFIX)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#define	LOCAL_ASM_OP	"\t.local\t"

#define	LCOMM_ASM_OP	"\t.lcomm\t"

/* Describe how to emit uninitialized local items.  */
#define	ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
do {									\
  if ((DECL) && rs6000_elf_in_small_data_p (DECL))			\
    {									\
      switch_to_section (sbss_section);					\
      ASM_OUTPUT_ALIGN (FILE, exact_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (FILE, NAME);					\
      ASM_OUTPUT_SKIP (FILE, SIZE);					\
      if (!flag_inhibit_size_directive && (SIZE) > 0)			\
	ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, SIZE);			\
    }									\
  else									\
    {									\
      fprintf (FILE, "%s", LCOMM_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), "," HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	       (SIZE), (ALIGN) / BITS_PER_UNIT);			\
    }									\
  ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");			\
} while (0)

/* Describe how to emit uninitialized external linkage items.  */
#define	ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
do {									\
  ASM_OUTPUT_ALIGNED_DECL_LOCAL (FILE, DECL, NAME, SIZE, ALIGN);	\
} while (0)

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
/* To support -falign-* switches we need to use .p2align so
   that alignment directives in code sections will be padded
   with no-op instructions, rather than zeroes.  */
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)			\
  if ((LOG) != 0)							\
    {									\
      if ((MAX_SKIP) == 0)						\
	fprintf ((FILE), "\t.p2align %d\n", (LOG));			\
      else								\
	fprintf ((FILE), "\t.p2align %d,,%d\n",	(LOG), (MAX_SKIP));	\
    }
#endif

/* This is how to output code to push a register on the stack.
   It need not be very fast code.

   On the rs6000, we must keep the backchain up to date.  In order
   to simplify things, always allocate 16 bytes for a push (System V
   wants to keep stack aligned to a 16 byte boundary).  */

#define	ASM_OUTPUT_REG_PUSH(FILE, REGNO)				\
do {									\
  if (DEFAULT_ABI == ABI_V4)						\
    asm_fprintf (FILE,							\
		 "\tstwu %s,-16(%s)\n\tstw %s,12(%s)\n",	\
		 reg_names[1], reg_names[1], reg_names[REGNO],		\
		 reg_names[1]);						\
} while (0)

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define	ASM_OUTPUT_REG_POP(FILE, REGNO)					\
do {									\
  if (DEFAULT_ABI == ABI_V4)						\
    asm_fprintf (FILE,							\
		 "\tlwz %s,12(%s)\n\taddi %s,%s,16\n",	\
		 reg_names[REGNO], reg_names[1], reg_names[1],		\
		 reg_names[1]);						\
} while (0)

extern int fixuplabelno;

/* Handle constructors specially for -mrelocatable.  */
#define TARGET_ASM_CONSTRUCTOR  rs6000_elf_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   rs6000_elf_asm_out_destructor

/* This is the end of what might become sysv4.h.  */

/* Use DWARF 2 debugging information by default.  */
#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Historically we have also supported stabs debugging.  */
#define DBX_DEBUGGING_INFO 1

#define TARGET_ENCODE_SECTION_INFO  rs6000_elf_encode_section_info
#define TARGET_IN_SMALL_DATA_P  rs6000_elf_in_small_data_p

/* The ELF version doesn't encode [DS] or whatever at the end of symbols.  */

#define	RS6000_OUTPUT_BASENAME(FILE, NAME)	\
    assemble_name (FILE, NAME)

/* We have to output the stabs for the function name *first*, before
   outputting its label.  */

#define	DBX_FUNCTION_FIRST

/* This is the end of what might become sysv4dbx.h.  */

#define TARGET_OS_SYSV_CPP_BUILTINS()		\
  do						\
    {						\
      if (rs6000_isa_flags_explicit		\
	  & OPTION_MASK_RELOCATABLE)		\
	builtin_define ("_RELOCATABLE");	\
    }						\
  while (0)

#ifndef	TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("PPC");		\
      builtin_define_std ("unix");		\
      builtin_define ("__svr4__");		\
      builtin_assert ("system=unix");		\
      builtin_assert ("system=svr4");		\
      builtin_assert ("cpu=powerpc");		\
      builtin_assert ("machine=powerpc");	\
      TARGET_OS_SYSV_CPP_BUILTINS ();		\
    }						\
  while (0)
#endif

/* Select one of BIG_OPT, LITTLE_OPT or DEFAULT_OPT depending
   on various -mbig, -mlittle and -mcall- options.  */
#define ENDIAN_SELECT(BIG_OPT, LITTLE_OPT, DEFAULT_OPT)	\
"%{mlittle|mlittle-endian:"	LITTLE_OPT ";"	\
  "mbig|mbig-endian:"		BIG_OPT    ";"	\
  "mcall-i960-old:"		LITTLE_OPT ";"	\
  ":"				DEFAULT_OPT "}"

#define DEFAULT_ASM_ENDIAN " -mbig"

#undef	ASM_SPEC
#define	ASM_SPEC "%(asm_cpu) \
%{,assembler|,assembler-with-cpp: %{mregnames} %{mno-regnames}} \
%{mrelocatable} %{mrelocatable-lib} %{" FPIE_OR_FPIC_SPEC ":-K PIC} \
%{memb|msdata=eabi: -memb}" \
ENDIAN_SELECT(" -mbig", " -mlittle", DEFAULT_ASM_ENDIAN)

#ifndef CC1_SECURE_PLT_DEFAULT_SPEC
#define CC1_SECURE_PLT_DEFAULT_SPEC ""
#endif
#ifndef LINK_SECURE_PLT_DEFAULT_SPEC
#define LINK_SECURE_PLT_DEFAULT_SPEC ""
#endif

/* Pass -G xxx to the compiler.  */
#undef CC1_SPEC
#define	CC1_SPEC "%{G*} %(cc1_cpu)" \
"%{meabi: %{!mcall-*: -mcall-sysv }} \
%{!meabi: %{!mno-eabi: \
    %{mrelocatable: -meabi } \
    %{mcall-freebsd: -mno-eabi } \
    %{mcall-i960-old: -meabi } \
    %{mcall-linux: -mno-eabi } \
    %{mcall-netbsd: -mno-eabi } \
    %{mcall-openbsd: -mno-eabi }}} \
%{msdata: -msdata=default} \
%{mno-sdata: -msdata=none} \
%{!mbss-plt: %{!msecure-plt: %(cc1_secure_plt_default)}} \
%{profile: -p}"

/* Default starting address if specified.  */
#define LINK_START_SPEC "\
%{mads         : %(link_start_ads)         ; \
  myellowknife : %(link_start_yellowknife) ; \
  mmvme        : %(link_start_mvme)        ; \
  msim         : %(link_start_sim)         ; \
  mcall-freebsd: %(link_start_freebsd)     ; \
  mcall-linux  : %(link_start_linux)       ; \
  mcall-netbsd : %(link_start_netbsd)      ; \
  mcall-openbsd: %(link_start_openbsd)     ; \
               : %(link_start_default)     }"

#define LINK_START_DEFAULT_SPEC ""
#define LINK_SECURE_PLT_SPEC LINK_SECURE_PLT_DEFAULT_SPEC

#undef	LINK_SPEC
#define	LINK_SPEC "\
%{h*} %{v:-V} %{!msdata=none:%{G*}} %{msdata=none:-G0} \
%{R*} \
%(link_shlib) \
%{!T*: %(link_start) } \
%{!static: %{!mbss-plt: %(link_secure_plt)}} \
%(link_os)"

/* Shared libraries are not default.  */
#define LINK_SHLIB_SPEC "\
%{!mshlib: %{!shared: %{!symbolic: -dn -Bstatic}}} \
%{static: } \
%{shared:-G -dy -z text } \
%{symbolic:-Bsymbolic -G -dy -z text }"

/* Any specific OS flags.  */
#define LINK_OS_SPEC "\
%{mads         : %(link_os_ads)         ; \
  myellowknife : %(link_os_yellowknife) ; \
  mmvme        : %(link_os_mvme)        ; \
  msim         : %(link_os_sim)         ; \
  mcall-freebsd: %(link_os_freebsd)     ; \
  mcall-linux  : %(link_os_linux)       ; \
  mcall-netbsd : %(link_os_netbsd)      ; \
  mcall-openbsd: %(link_os_openbsd)     ; \
               : %(link_os_default)     }"

#define LINK_OS_DEFAULT_SPEC ""

/* Override rs6000.h definition.  */
#undef	CPP_SPEC
#define	CPP_SPEC "%{posix: -D_POSIX_SOURCE} \
%{mads         : %(cpp_os_ads)         ; \
  myellowknife : %(cpp_os_yellowknife) ; \
  mmvme        : %(cpp_os_mvme)        ; \
  msim         : %(cpp_os_sim)         ; \
  mcall-freebsd: %(cpp_os_freebsd)     ; \
  mcall-linux  : %(cpp_os_linux)       ; \
  mcall-netbsd : %(cpp_os_netbsd)      ; \
  mcall-openbsd: %(cpp_os_openbsd)     ; \
               : %(cpp_os_default)     }"

#define	CPP_OS_DEFAULT_SPEC ""

#undef	STARTFILE_SPEC
#define	STARTFILE_SPEC "\
%{mads         : %(startfile_ads)         ; \
  myellowknife : %(startfile_yellowknife) ; \
  mmvme        : %(startfile_mvme)        ; \
  msim         : %(startfile_sim)         ; \
  mcall-freebsd: %(startfile_freebsd)     ; \
  mcall-linux  : %(startfile_linux)       ; \
  mcall-netbsd : %(startfile_netbsd)      ; \
  mcall-openbsd: %(startfile_openbsd)     ; \
               : %(startfile_default)     }"

#define	STARTFILE_DEFAULT_SPEC "ecrti.o%s crtbegin.o%s"

#undef	LIB_SPEC
#define	LIB_SPEC "\
%{mads         : %(lib_ads)         ; \
  myellowknife : %(lib_yellowknife) ; \
  mmvme        : %(lib_mvme)        ; \
  msim         : %(lib_sim)         ; \
  mcall-freebsd: %(lib_freebsd)     ; \
  mcall-linux  : %(lib_linux)       ; \
  mcall-netbsd : %(lib_netbsd)      ; \
  mcall-openbsd: %(lib_openbsd)     ; \
               : %(lib_default)     }"

#define LIB_DEFAULT_SPEC "-lc"

#undef	ENDFILE_SPEC
#define	ENDFILE_SPEC "\
%{mads         : %(endfile_ads)         ; \
  myellowknife : %(endfile_yellowknife) ; \
  mmvme        : %(endfile_mvme)        ; \
  msim         : %(endfile_sim)         ; \
  mcall-freebsd: %(endfile_freebsd)     ; \
  mcall-linux  : %(endfile_linux)       ; \
  mcall-netbsd : %(endfile_netbsd)      ; \
  mcall-openbsd: %(endfile_openbsd)     ; \
               : %(crtsavres_default) %(endfile_default)     }"

#define CRTSAVRES_DEFAULT_SPEC ""

#define	ENDFILE_DEFAULT_SPEC "crtend.o%s ecrtn.o%s"

/* Motorola ADS support.  */
#define LIB_ADS_SPEC "--start-group -lads -lc --end-group"

#define	STARTFILE_ADS_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_ADS_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_ADS_SPEC "-T ads.ld%s"

#define LINK_OS_ADS_SPEC ""

#define CPP_OS_ADS_SPEC ""

/* Motorola Yellowknife support.  */
#define LIB_YELLOWKNIFE_SPEC "--start-group -lyk -lc --end-group"

#define	STARTFILE_YELLOWKNIFE_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_YELLOWKNIFE_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_YELLOWKNIFE_SPEC "-T yellowknife.ld%s"

#define LINK_OS_YELLOWKNIFE_SPEC ""

#define CPP_OS_YELLOWKNIFE_SPEC ""

/* Motorola MVME support.  */
#define LIB_MVME_SPEC "--start-group -lmvme -lc --end-group"

#define	STARTFILE_MVME_SPEC "ecrti.o%s crt0.o%s crtbegin.o%s"

#define	ENDFILE_MVME_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_MVME_SPEC "-Ttext 0x40000"

#define LINK_OS_MVME_SPEC ""

#define CPP_OS_MVME_SPEC ""

/* PowerPC simulator based on netbsd system calls support.  */
#define LIB_SIM_SPEC "--start-group -lsim -lc --end-group"

#define	STARTFILE_SIM_SPEC "ecrti.o%s sim-crt0.o%s crtbegin.o%s"

#define	ENDFILE_SIM_SPEC "crtend.o%s ecrtn.o%s"

#define LINK_START_SIM_SPEC ""

#define LINK_OS_SIM_SPEC "-m elf32ppcsim"

#define CPP_OS_SIM_SPEC ""

/* FreeBSD support.  */

#define CPP_OS_FREEBSD_SPEC	"\
  -D__PPC__ -D__ppc__ -D__PowerPC__ -D__powerpc__ \
  -Acpu=powerpc -Amachine=powerpc"

#define	STARTFILE_FREEBSD_SPEC	FBSD_STARTFILE_SPEC
#define ENDFILE_FREEBSD_SPEC	FBSD_ENDFILE_SPEC
#define LIB_FREEBSD_SPEC	FBSD_LIB_SPEC
#define LINK_START_FREEBSD_SPEC	""

#define LINK_OS_FREEBSD_SPEC "\
  %{p:%nconsider using '-pg' instead of '-p' with gprof(1)} \
  %{v:-V} \
  %{assert*} %{R*} %{rpath*} %{defsym*} \
  %{shared:-Bshareable %{h*} %{soname*}} \
  %{!shared: \
    %{!static: \
      %{rdynamic: -export-dynamic} \
      -dynamic-linker %(fbsd_dynamic_linker) } \
    %{static:-Bstatic}} \
  %{symbolic:-Bsymbolic}"

/* GNU/Linux support.  */
#define LIB_LINUX_SPEC "%{mnewlib: --start-group -llinux -lc --end-group } \
%{!mnewlib: %{pthread:-lpthread} %{shared:-lc} \
%{!shared: %{profile:-lc_p} %{!profile:-lc}}}"

#if ENABLE_OFFLOADING == 1
#define CRTOFFLOADBEGIN "%{fopenacc|fopenmp:crtoffloadbegin%O%s}"
#define CRTOFFLOADEND "%{fopenacc|fopenmp:crtoffloadend%O%s}"
#else
#define CRTOFFLOADBEGIN ""
#define CRTOFFLOADEND ""
#endif

/* STARTFILE_LINUX_SPEC should be the same as GNU_USER_TARGET_STARTFILE_SPEC
   but with the mnewlib ecrti.o%s selection substituted for crti.o%s.  */
#define	STARTFILE_LINUX_SPEC \
  "%{shared:; \
     pg|p|profile:gcrt1.o%s; \
     static:crt1.o%s; \
     static-pie|" PIE_SPEC ":Scrt1.o%s; \
     :crt1.o%s} \
   %{mnewlib:ecrti.o%s;:crti.o%s} \
   %{static:crtbeginT.o%s; \
     shared|static-pie|" PIE_SPEC ":crtbeginS.o%s; \
     :crtbegin.o%s} \
   %{fvtable-verify=none:%s; \
     fvtable-verify=preinit:vtv_start_preinit.o%s; \
     fvtable-verify=std:vtv_start.o%s} \
   " CRTOFFLOADBEGIN

/* ENDFILE_LINUX_SPEC should be the same as GNU_USER_TARGET_ENDFILE_SPEC
   but with the mnewlib ecrtn.o%s selection substituted for crtn.o%s.  */
#define ENDFILE_LINUX_SPEC \
  "%{fvtable-verify=none:%s; \
     fvtable-verify=preinit:vtv_end_preinit.o%s; \
     fvtable-verify=std:vtv_end.o%s} \
   %{static:crtend.o%s; \
     shared|static-pie|" PIE_SPEC ":crtendS.o%s; \
     :crtend.o%s} \
   %{mnewlib:ecrtn.o%s;:crtn.o%s} \
   " CRTOFFLOADEND

#define LINK_START_LINUX_SPEC ""

#define MUSL_DYNAMIC_LINKER_E ENDIAN_SELECT("","le","")

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"
#define UCLIBC_DYNAMIC_LINKER "/lib/ld-uClibc.so.0"
#define MUSL_DYNAMIC_LINKER \
  "/lib/ld-musl-powerpc" MUSL_DYNAMIC_LINKER_E "%{msoft-float:-sf}.so.1"
#if DEFAULT_LIBC == LIBC_UCLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U, M) \
  "%{mglibc:" G ";:%{mmusl:" M ";:" U "}}"
#elif DEFAULT_LIBC == LIBC_MUSL
#define CHOOSE_DYNAMIC_LINKER(G, U, M) \
  "%{mglibc:" G ";:%{muclibc:" U ";:" M "}}"
#elif !defined (DEFAULT_LIBC) || DEFAULT_LIBC == LIBC_GLIBC
#define CHOOSE_DYNAMIC_LINKER(G, U, M) \
  "%{muclibc:" U ";:%{mmusl:" M ";:" G "}}"
#else
#error "Unsupported DEFAULT_LIBC"
#endif
#define GNU_USER_DYNAMIC_LINKER \
  CHOOSE_DYNAMIC_LINKER (GLIBC_DYNAMIC_LINKER, UCLIBC_DYNAMIC_LINKER, \
			 MUSL_DYNAMIC_LINKER)

#define LINK_OS_LINUX_SPEC "-m elf32ppclinux %{!shared: %{!static: \
  %{rdynamic:-export-dynamic} \
  -dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}"

#if defined(HAVE_LD_EH_FRAME_HDR)
# define LINK_EH_SPEC "%{!static|static-pie:--eh-frame-hdr} "
#endif

#define CPP_OS_LINUX_SPEC "-D__unix__ -D__gnu_linux__ -D__linux__ \
%{!undef:							  \
  %{!ansi:							  \
    %{!std=*:-Dunix -D__unix -Dlinux -D__linux}			  \
    %{std=gnu*:-Dunix -D__unix -Dlinux -D__linux}}}		  \
-Asystem=linux -Asystem=unix -Asystem=posix %{pthread:-D_REENTRANT}"

/* NetBSD support.  */
#define LIB_NETBSD_SPEC "\
-lc"

#define	STARTFILE_NETBSD_SPEC "\
ncrti.o%s crt0.o%s \
%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

#define ENDFILE_NETBSD_SPEC "\
%{!shared:crtend.o%s} %{shared:crtendS.o%s} \
ncrtn.o%s"

#define LINK_START_NETBSD_SPEC "\
"

#define LINK_OS_NETBSD_SPEC "\
%{!shared: %{!static: \
  %{rdynamic:-export-dynamic} \
  -dynamic-linker /usr/libexec/ld.elf_so}}"

#define CPP_OS_NETBSD_SPEC "\
-D__powerpc__ -D__NetBSD__ -D__KPRINTF_ATTRIBUTE__"

/* OpenBSD support.  */
#ifndef	LIB_OPENBSD_SPEC
#define LIB_OPENBSD_SPEC "%{!shared:%{pthread:-lpthread%{p:_p}%{!p:%{pg:_p}}}} %{!shared:-lc%{p:_p}%{!p:%{pg:_p}}}"
#endif

#ifndef	STARTFILE_OPENBSD_SPEC
#define	STARTFILE_OPENBSD_SPEC "\
%{!shared: %{pg:gcrt0.o%s} %{!pg:%{p:gcrt0.o%s} %{!p:crt0.o%s}}} \
%{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"
#endif

#ifndef	ENDFILE_OPENBSD_SPEC
#define	ENDFILE_OPENBSD_SPEC "\
%{!shared:crtend.o%s} %{shared:crtendS.o%s}"
#endif

#ifndef LINK_START_OPENBSD_SPEC
#define LINK_START_OPENBSD_SPEC "-Ttext 0x400074"
#endif

#ifndef LINK_OS_OPENBSD_SPEC
#define LINK_OS_OPENBSD_SPEC ""
#endif

#ifndef CPP_OS_OPENBSD_SPEC
#define CPP_OS_OPENBSD_SPEC "%{posix:-D_POSIX_SOURCE} %{pthread:-D_POSIX_THREADS}"
#endif

/* Define any extra SPECS that the compiler needs to generate.  */
/* Override rs6000.h definition.  */
#undef	SUBTARGET_EXTRA_SPECS
#define	SUBTARGET_EXTRA_SPECS						\
  { "crtsavres_default",	CRTSAVRES_DEFAULT_SPEC },		\
  { "lib_ads",			LIB_ADS_SPEC },				\
  { "lib_yellowknife",		LIB_YELLOWKNIFE_SPEC },			\
  { "lib_mvme",			LIB_MVME_SPEC },			\
  { "lib_sim",			LIB_SIM_SPEC },				\
  { "lib_freebsd",		LIB_FREEBSD_SPEC },			\
  { "lib_linux",		LIB_LINUX_SPEC },			\
  { "lib_netbsd",		LIB_NETBSD_SPEC },			\
  { "lib_openbsd",		LIB_OPENBSD_SPEC },			\
  { "lib_default",		LIB_DEFAULT_SPEC },			\
  { "startfile_ads",		STARTFILE_ADS_SPEC },			\
  { "startfile_yellowknife",	STARTFILE_YELLOWKNIFE_SPEC },		\
  { "startfile_mvme",		STARTFILE_MVME_SPEC },			\
  { "startfile_sim",		STARTFILE_SIM_SPEC },			\
  { "startfile_freebsd",	STARTFILE_FREEBSD_SPEC },		\
  { "startfile_linux",		STARTFILE_LINUX_SPEC },			\
  { "startfile_netbsd",		STARTFILE_NETBSD_SPEC },		\
  { "startfile_openbsd",	STARTFILE_OPENBSD_SPEC },		\
  { "startfile_default",	STARTFILE_DEFAULT_SPEC },		\
  { "endfile_ads",		ENDFILE_ADS_SPEC },			\
  { "endfile_yellowknife",	ENDFILE_YELLOWKNIFE_SPEC },		\
  { "endfile_mvme",		ENDFILE_MVME_SPEC },			\
  { "endfile_sim",		ENDFILE_SIM_SPEC },			\
  { "endfile_freebsd",		ENDFILE_FREEBSD_SPEC },			\
  { "endfile_linux",		ENDFILE_LINUX_SPEC },			\
  { "endfile_netbsd",		ENDFILE_NETBSD_SPEC },			\
  { "endfile_openbsd",		ENDFILE_OPENBSD_SPEC },			\
  { "endfile_default",		ENDFILE_DEFAULT_SPEC },			\
  { "link_shlib",		LINK_SHLIB_SPEC },			\
  { "link_start",		LINK_START_SPEC },			\
  { "link_start_ads",		LINK_START_ADS_SPEC },			\
  { "link_start_yellowknife",	LINK_START_YELLOWKNIFE_SPEC },		\
  { "link_start_mvme",		LINK_START_MVME_SPEC },			\
  { "link_start_sim",		LINK_START_SIM_SPEC },			\
  { "link_start_freebsd",	LINK_START_FREEBSD_SPEC },		\
  { "link_start_linux",		LINK_START_LINUX_SPEC },		\
  { "link_start_netbsd",	LINK_START_NETBSD_SPEC },		\
  { "link_start_openbsd",	LINK_START_OPENBSD_SPEC },		\
  { "link_start_default",	LINK_START_DEFAULT_SPEC },		\
  { "link_os",			LINK_OS_SPEC },				\
  { "link_os_ads",		LINK_OS_ADS_SPEC },			\
  { "link_os_yellowknife",	LINK_OS_YELLOWKNIFE_SPEC },		\
  { "link_os_mvme",		LINK_OS_MVME_SPEC },			\
  { "link_os_sim",		LINK_OS_SIM_SPEC },			\
  { "link_os_freebsd",		LINK_OS_FREEBSD_SPEC },			\
  { "link_os_linux",		LINK_OS_LINUX_SPEC },			\
  { "link_os_netbsd",		LINK_OS_NETBSD_SPEC },			\
  { "link_os_openbsd",		LINK_OS_OPENBSD_SPEC },			\
  { "link_os_default",		LINK_OS_DEFAULT_SPEC },			\
  { "cc1_secure_plt_default",	CC1_SECURE_PLT_DEFAULT_SPEC },		\
  { "link_secure_plt",		LINK_SECURE_PLT_SPEC },			\
  { "cpp_os_ads",		CPP_OS_ADS_SPEC },			\
  { "cpp_os_yellowknife",	CPP_OS_YELLOWKNIFE_SPEC },		\
  { "cpp_os_mvme",		CPP_OS_MVME_SPEC },			\
  { "cpp_os_sim",		CPP_OS_SIM_SPEC },			\
  { "cpp_os_freebsd",		CPP_OS_FREEBSD_SPEC },			\
  { "cpp_os_linux",		CPP_OS_LINUX_SPEC },			\
  { "cpp_os_netbsd",		CPP_OS_NETBSD_SPEC },			\
  { "cpp_os_openbsd",		CPP_OS_OPENBSD_SPEC },			\
  { "cpp_os_default",		CPP_OS_DEFAULT_SPEC },			\
  { "fbsd_dynamic_linker",	FBSD_DYNAMIC_LINKER },			\
  SUBSUBTARGET_EXTRA_SPECS

#define	SUBSUBTARGET_EXTRA_SPECS

/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using `MULTILIB_OPTIONS'.

   Do not define this macro if `MULTILIB_OPTIONS' is not defined in
   the target makefile fragment or if none of the options listed in
   `MULTILIB_OPTIONS' are set by default.  *Note Target Fragment::.  */

#define	MULTILIB_DEFAULTS { "mbig", "mcall-sysv" }

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */
#define PROFILE_BEFORE_PROLOGUE 1

/* Function name to call to do profiling.  */
#define RS6000_MCOUNT "_mcount"

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)			\
  (flag_pic								\
   ? (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel		\
      | DW_EH_PE_sdata4)						\
   : DW_EH_PE_absptr)

#define DOUBLE_INT_ASM_OP "\t.quad\t"

/* Generate entries in .fixup for relocatable addresses.  */
#define RELOCATABLE_NEEDS_FIXUP 1

#define TARGET_ASM_FILE_END rs6000_elf_file_end

#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET rs6000_asan_shadow_offset

/* This target uses the sysv4.opt file.  */
#define TARGET_USES_SYSV4_OPT 1

/* Include order changes for musl, same as in generic linux.h.  */
#if DEFAULT_LIBC == LIBC_MUSL
#define INCLUDE_DEFAULTS_MUSL_GPP			\
    { GPLUSPLUS_INCLUDE_DIR, "G++", 1, 1,		\
      GPLUSPLUS_INCLUDE_DIR_ADD_SYSROOT, 0 },		\
    { GPLUSPLUS_TOOL_INCLUDE_DIR, "G++", 1, 1,		\
      GPLUSPLUS_INCLUDE_DIR_ADD_SYSROOT, 1 },		\
    { GPLUSPLUS_BACKWARD_INCLUDE_DIR, "G++", 1, 1,	\
      GPLUSPLUS_INCLUDE_DIR_ADD_SYSROOT, 0 },

#ifdef LOCAL_INCLUDE_DIR
#define INCLUDE_DEFAULTS_MUSL_LOCAL			\
    { LOCAL_INCLUDE_DIR, 0, 0, 1, 1, 2 },		\
    { LOCAL_INCLUDE_DIR, 0, 0, 1, 1, 0 },
#else
#define INCLUDE_DEFAULTS_MUSL_LOCAL
#endif

#ifdef PREFIX_INCLUDE_DIR
#define INCLUDE_DEFAULTS_MUSL_PREFIX			\
    { PREFIX_INCLUDE_DIR, 0, 0, 1, 0, 0},
#else
#define INCLUDE_DEFAULTS_MUSL_PREFIX
#endif

#ifdef CROSS_INCLUDE_DIR
#define INCLUDE_DEFAULTS_MUSL_CROSS			\
    { CROSS_INCLUDE_DIR, "GCC", 0, 0, 0, 0},
#else
#define INCLUDE_DEFAULTS_MUSL_CROSS
#endif

#ifdef TOOL_INCLUDE_DIR
#define INCLUDE_DEFAULTS_MUSL_TOOL			\
    { TOOL_INCLUDE_DIR, "BINUTILS", 0, 1, 0, 0},
#else
#define INCLUDE_DEFAULTS_MUSL_TOOL
#endif

#ifdef NATIVE_SYSTEM_HEADER_DIR
#define INCLUDE_DEFAULTS_MUSL_NATIVE			\
    { NATIVE_SYSTEM_HEADER_DIR, 0, 0, 0, 1, 2 },	\
    { NATIVE_SYSTEM_HEADER_DIR, 0, 0, 0, 1, 0 },
#else
#define INCLUDE_DEFAULTS_MUSL_NATIVE
#endif

#if defined (CROSS_DIRECTORY_STRUCTURE) && !defined (TARGET_SYSTEM_ROOT)
# undef INCLUDE_DEFAULTS_MUSL_LOCAL
# define INCLUDE_DEFAULTS_MUSL_LOCAL
# undef INCLUDE_DEFAULTS_MUSL_NATIVE
# define INCLUDE_DEFAULTS_MUSL_NATIVE
#else
# undef INCLUDE_DEFAULTS_MUSL_CROSS
# define INCLUDE_DEFAULTS_MUSL_CROSS
#endif

#undef INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS				\
  {							\
    INCLUDE_DEFAULTS_MUSL_GPP				\
    INCLUDE_DEFAULTS_MUSL_LOCAL				\
    INCLUDE_DEFAULTS_MUSL_PREFIX			\
    INCLUDE_DEFAULTS_MUSL_CROSS				\
    INCLUDE_DEFAULTS_MUSL_TOOL				\
    INCLUDE_DEFAULTS_MUSL_NATIVE			\
    { GCC_INCLUDE_DIR, "GCC", 0, 1, 0, 0 },		\
    { 0, 0, 0, 0, 0, 0 }				\
  }
#endif
