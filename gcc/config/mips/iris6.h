/* Definitions of target machine for GNU compiler.  Iris version 6.
   Copyright (C) 1994, 1995, 1996, 1997, 1998, 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.

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

/* Let mips.c know we need the Irix6 functions.  */
#define TARGET_IRIX6 1

/* Default to -mabi=n32 and -mips3.  */
#define MIPS_ISA_DEFAULT 3
#define MIPS_ABI_DEFAULT ABI_N32
#define MULTILIB_DEFAULTS { "mabi=n32" }

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_ABICALLS|MASK_FLOAT64|MASK_64BIT)
#endif

#include "mips/iris5.h"

/* Irix6 assembler does handle DWARF2 directives.  Override setting in
   irix5.h file.  */
#undef DWARF2_UNWIND_INFO

/* The Irix6 assembler will sometimes assign labels to the wrong
   section unless the labels are within .ent/.end blocks.  Therefore,
   we avoid creating such labels.  */
#define DWARF2_GENERATE_TEXT_SECTION_LABEL 0

/* wchar_t is defined differently with and without -mabi=64.  */

#undef WCHAR_TYPE
#define WCHAR_TYPE (Pmode == DImode ? "int" : "long int")

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Same for wint_t.  */

#undef WINT_TYPE
#define WINT_TYPE (Pmode == DImode ? "int" : "long int")

#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE 32

/* For Irix 6, -mabi=64 implies TARGET_LONG64.  */
/* This is handled in override_options.  */

#undef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC ""

/* We must pass -D_LONGLONG always, even when -ansi is used, because irix6
   system header files require it.  This is OK, because gcc never warns
   when long long is used in system header files.  Alternatively, we can
   add support for the SGI builtin type __long_long.  */

/* The GNU C++ standard library requires that __EXTENSIONS__ and
   _SGI_SOURCE be defined on at least irix6.2 and probably all IRIX 6
   prior to 6.5.  They normally get defined if !ansi, for g++ we want
   them regardless.  We don't need this on IRIX 6.5 itself, but it
   shouldn't hurt other than the namespace pollution.  */

/* Undefine because this includes iris5.h.  */
#undef  TARGET_OS_CPP_BUILTINS
#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("host_mips");		\
	builtin_define ("_LONGLONG");			\
	builtin_define ("_MODERN_C");			\
	builtin_define ("_SVR4_SOURCE");		\
	builtin_define_std ("SYSTYPE_SVR4");		\
	builtin_define ("__DSO__");			\
	builtin_define_std ("unix");			\
	builtin_define_std ("sgi");			\
	builtin_assert ("system=svr4");			\
	builtin_assert ("system=unix");			\
	builtin_assert ("machine=sgi");			\
							\
     if (mips_abi == ABI_32)				\
      {							\
	builtin_define ("_MIPS_SIM=_MIPS_SIM_ABI32");	\
	builtin_define ("_MIPS_SZLONG=32");		\
	builtin_define ("_MIPS_SZPTR=32");		\
      }							\
     else if (mips_abi == ABI_64)			\
      {							\
	builtin_define ("_ABI64=3");			\
	builtin_define ("_MIPS_SIM=_ABI64");		\
	builtin_define ("_MIPS_SZLONG=64");		\
	builtin_define ("_MIPS_SZPTR=64");		\
      }							\
     else						\
      {							\
	builtin_define ("_ABIN32=2");			\
	builtin_define ("_MIPS_SIM=_ABIN32");		\
	builtin_define ("_MIPS_SZLONG=32");		\
	builtin_define ("_MIPS_SZPTR=32");		\
      }							\
							\
     if (!TARGET_FLOAT64)				\
	builtin_define ("_MIPS_FPSET=16");		\
     else						\
	builtin_define ("_MIPS_FPSET=32");		\
							\
     if (!TARGET_INT64)					\
	builtin_define ("_MIPS_SZINT=32");		\
     else						\
	builtin_define ("_MIPS_SZINT=64");		\
							\
     if (!ISA_MIPS1 && !ISA_MIPS2)			\
	builtin_define ("_COMPILER_VERSION=601");	\
							\
     /* IRIX 6.5.18 and above provide many ISO C99	\
	features protected by the __c99 macro.		\
	libstdc++ v3 needs them as well.  */		\
     if ((c_language == clk_c && flag_isoc99)		\
	 || c_language == clk_cplusplus)		\
	builtin_define ("__c99");			\
							\
     if (c_language == clk_cplusplus)			\
      {							\
	builtin_define ("__EXTENSIONS__");		\
	builtin_define ("_SGI_SOURCE");			\
      }							\
							\
     if (!flag_iso)					\
       {						\
	 builtin_define ("__EXTENSIONS__");		\
	 builtin_define ("_SGI_SOURCE");		\
       }						\
} while (0)

/* Irix 6 uses DWARF-2.  */
#define DWARF2_DEBUGGING_INFO 1
#define MIPS_DEBUGGING_INFO 1
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Force the generation of dwarf .debug_frame sections even if not
   compiling -g.  This guarantees that we can unwind the stack.  */
#define DWARF2_FRAME_INFO 1

/* The size in bytes of a DWARF field indicating an offset or length
   relative to a debug info section, specified to be 4 bytes in the DWARF-2
   specification.  The SGI/MIPS ABI defines it to be the same as PTR_SIZE.  */
#define DWARF_OFFSET_SIZE PTR_SIZE

/* There is no GNU as port for Irix6 yet, so we set MD_EXEC_PREFIX so that
   gcc will automatically find SGI as instead of searching the user's path.
   The latter can fail when building a cross compiler if the user has . in
   the path before /usr/bin, since then gcc will find and try to use the link
   to the cross assembler which can't possibly work.  */

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/bin/"

/* We have no need for MD_STARTFILE_PREFIX.  */
#undef MD_STARTFILE_PREFIX

#undef MACHINE_TYPE
#define MACHINE_TYPE "SGI running IRIX 6.x"

/* Irix 5 stuff that we don't need for Irix 6.  */
/* ??? We do need this for the -mabi=32 switch though.  */
#undef ASM_OUTPUT_UNDEF_FUNCTION
#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#undef ASM_DECLARE_FUNCTION_SIZE

/* Stuff we need for Irix 6 that isn't in Irix 5.  */

/* The SGI assembler doesn't like labels before the .ent, so we must output
   the .ent and function name here, which is the normal place for it.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)			\
  do {									\
    fputs ("\t.ent\t", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs (":\n", STREAM);						\
  } while (0)

/* Likewise, the SGI assembler doesn't like labels after the .end, so we
   must output the .end here.  */
#define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)			\
  do {									\
    fputs ("\t.end\t", STREAM);						\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
  } while (0)

/* Tell function_prologue in mips.c that we have already output the .ent/.end
   pseudo-ops.  */
#define FUNCTION_NAME_ALREADY_DECLARED

#undef SET_ASM_OP	/* Has no equivalent.  See ASM_OUTPUT_DEF below.  */

#if 0
/* This is *NOT* how to equate one symbol to another symbol.  The assembler
   '=' syntax just equates a name to a constant expression.
   See ASM_OUTPUT_WEAK_ALIAS.  */

#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t");						\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, " = ");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif

/* Define the strings used for the special svr4 .type and .size directives.  */

#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"

/* Irix assembler does not support the init_priority C++ attribute.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0

/* A linker error can empirically be avoided by removing duplicate
   library search directories.  */
#define LINK_ELIMINATE_DUPLICATE_LDIRECTORIES 1

#define POPSECTION_ASM_OP	"\t.popsection"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
/* If no mips[3,4] option given, give the appropriate default for mabi=X */
#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC "%{!mabi*:-n32} %{!mips*: %{!mabi*:-mips3} %{mabi=n32|mabi=64:-mips3}}"

/* Must pass -g0 to the assembler, otherwise it may overwrite our
   debug info with its own debug info.  */
/* Must pass -show instead of -v.  */
/* Must pass -G 0 to the assembler, otherwise we may get warnings about
   GOT overflow.  */
/* ??? We pass -w to disable all assembler warnings.  The `label should be
   inside .ent/.end block' warning that we get for DWARF II debug info labels
   is particularly annoying.  */
#undef SUBTARGET_MIPS_AS_ASM_SPEC
#define SUBTARGET_MIPS_AS_ASM_SPEC "%{v:-show} -G 0 -w"

#undef SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "-g0"

/* The MIPS assembler occasionally misoptimizes.  Since GCC should be
   doing scheduling anyhow, just turn off optimization in the assembler.  */
#undef SUBTARGET_ASM_OPTIMIZING_SPEC
#define SUBTARGET_ASM_OPTIMIZING_SPEC "-O0"

/* The assembler now accepts .section pseudo-ops, but it does not allow
   one to change the section in the middle of a function, so we can't use
   the INIT_SECTION_ASM_OP code in crtstuff.  But we can build up the ctor
   and dtor lists this way, so we use -init and -fini to invoke the
   do_global_* functions instead of running collect2.  */

#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP_32	"\t.rdata"
#define READONLY_DATA_SECTION_ASM_OP_64	"\t.section\t.rodata"
#define READONLY_DATA_SECTION_ASM_OP		\
  (mips_abi != ABI_32 && mips_abi != ABI_O64	\
   ? READONLY_DATA_SECTION_ASM_OP_64		\
   : READONLY_DATA_SECTION_ASM_OP_32)

/* A default list of other sections which we might be "in" at any given
   time.  For targets that use additional sections (e.g. .tdesc) you
   should override this definition in the target-specific file which
   includes this file.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_sdata

/* A default list of extra section function definitions.  For targets
   that use additional sections (e.g. .tdesc) you should override this
   definition in the target-specific file which includes this file.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
const char *								\
current_section_name ()							\
{									\
  switch (in_section)							\
    {									\
    case no_section:	return NULL;					\
    case in_text:	return ".text";					\
    case in_data:	return ".data";					\
    case in_sdata:	return ".sdata";				\
    case in_bss:	return ".bss";					\
    case in_readonly_data:						\
      if (mips_abi != ABI_32 && mips_abi != ABI_O64)			\
	return ".rodata";						\
      else								\
	return ".rdata";						\
    case in_named:							\
      return in_named_name;						\
    }									\
  abort ();								\
}									\
									\
unsigned int								\
current_section_flags ()						\
{									\
  switch (in_section)							\
    {									\
    case no_section:	return 0;					\
    case in_text:	return SECTION_CODE;				\
    case in_data:	return SECTION_WRITE;				\
    case in_sdata:	return SECTION_WRITE | SECTION_SMALL;		\
    case in_bss:	return SECTION_WRITE | SECTION_BSS;		\
    case in_readonly_data: return 0;					\
    case in_named:	return get_named_section_flags (in_named_name);	\
    }									\
  abort ();								\
}

/* Switch into a generic section.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  iris6_asm_named_section

/* SGI assembler needs all sorts of extra help to do alignment properly.  */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN iris6_asm_output_align
#undef ASM_FILE_START
#define ASM_FILE_START  iris6_asm_file_start
#undef ASM_FILE_END
#define ASM_FILE_END	iris6_asm_file_end

#undef MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (32768*8)

/* ??? SGI assembler may core dump when compiling with -g.
   Sometimes as succeeds, but then we get a linker error. (cmds.c in 072.sc)
   Getting rid of .file solves both problems.  */
#undef ASM_OUTPUT_FILENAME
#define ASM_OUTPUT_FILENAME(STREAM, NUM_SOURCE_FILENAMES, NAME) \
do								\
  {								\
    fprintf (STREAM, "\t#.file\t%d ", NUM_SOURCE_FILENAMES);	\
    output_quoted_string (STREAM, NAME);			\
    fputs ("\n", STREAM);					\
  }								\
while (0)

/* ??? SGI assembler gives warning whenever .lcomm is used.  */
#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)		   \
do									   \
  {									   \
    if (mips_abi != ABI_32 && mips_abi != ABI_O64)			   \
      {									   \
	bss_section ();							   \
	mips_declare_object (STREAM, NAME, "", ":\n", 0);		   \
	ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	   \
	ASM_OUTPUT_SKIP (STREAM, SIZE);					   \
      }									   \
    else								   \
      mips_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (SIZE)); \
  }									   \
while (0)

/* A C statement (sans semicolon) to output to the stdio stream
   FILE the assembler definition of uninitialized global DECL named
   NAME whose size is SIZE bytes and alignment is ALIGN bytes.
   Try to use asm_output_aligned_bss to implement this macro.  */

#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (FILE, DECL, NAME, SIZE, ALIGN)

/* Write the extra assembler code needed to declare an object properly.  */

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			\
do									\
 {									\
   HOST_WIDE_INT size;							\
   size_directive_output = 0;						\
   if (!flag_inhibit_size_directive && DECL_SIZE (DECL))		\
     {									\
       size_directive_output = 1;					\
       size = int_size_in_bytes (TREE_TYPE (DECL));			\
       ASM_OUTPUT_SIZE_DIRECTIVE (STREAM, NAME, size);			\
     }									\
   mips_declare_object (STREAM, NAME, "", ":\n", 0);			\
 }									\
while (0)

/* Define the `__builtin_va_list' type for the ABI.  On Irix6, this
   type is `char *'.  */
#undef BUILD_VA_LIST_TYPE
#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = build_pointer_type (char_type_node)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	 \
do {									 \
     const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		 \
     HOST_WIDE_INT size;						 \
     if (!flag_inhibit_size_directive && DECL_SIZE (DECL)		 \
         && ! AT_END && TOP_LEVEL					 \
	 && DECL_INITIAL (DECL) == error_mark_node			 \
	 && !size_directive_output)					 \
       {								 \
	 size_directive_output = 1;					 \
	 size = int_size_in_bytes (TREE_TYPE (DECL));			 \
	 ASM_OUTPUT_SIZE_DIRECTIVE (FILE, name, size);			 \
       }								 \
   } while (0)

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX ((mips_abi == ABI_32 || mips_abi == ABI_O64) \
			    ? "$" : ".")

/* Profiling is supported via libprof1.a not -lc_p as in Irix 3.  */
/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{mabi=32:%{pg:gcrt1.o%s} \
       %{!pg:%{p:mcrt1.o%s libprof1.a%s}%{!p:crt1.o%s}}} \
     %{mabi=n32: \
       %{mips4:%{pg:/usr/lib32/mips4/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/mcrt1.o%s /usr/lib32/mips4/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/mcrt1.o%s /usr/lib32/mips3/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/crt1.o%s}}}} \
     %{mabi=64: \
       %{mips4:%{pg:/usr/lib64/mips4/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips4/mcrt1.o /usr/lib64/mips4/libprof1.a} \
           %{!p:/usr/lib64/mips4/crt1.o}}} \
       %{!mips4:%{pg:/usr/lib64/mips3/gcrt1.o} \
         %{!pg:%{p:/usr/lib64/mips3/mcrt1.o /usr/lib64/mips3/libprof1.a} \
           %{!p:/usr/lib64/mips3/crt1.o}}}} \
     %{!mabi*: \
       %{mips4:%{pg:/usr/lib32/mips4/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips4/mcrt1.o%s /usr/lib32/mips4/libprof1.a%s} \
           %{!p:/usr/lib32/mips4/crt1.o%s}}} \
       %{!mips4:%{pg:/usr/lib32/mips3/gcrt1.o%s} \
         %{!pg:%{p:/usr/lib32/mips3/mcrt1.o%s /usr/lib32/mips3/libprof1.a%s} \
           %{!p:/usr/lib32/mips3/crt1.o%s}}}}} \
   crtbegin.o%s"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{mabi=n32: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{mabi=64: %{mips4:-L/usr/lib64/mips4} %{!mips4:-L/usr/lib64/mips3} \
     -L/usr/lib64} \
   %{!mabi*: %{mips4:-L/usr/lib32/mips4} %{!mips4:-L/usr/lib32/mips3} \
     -L/usr/lib32} \
   %{!shared: \
     -dont_warn_unused %{p:libprof1.a%s}%{pg:libprof1.a%s} -lc -warn_unused}"

/* Avoid getting two warnings for libgcc.a everytime we link.  */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC "-dont_warn_unused -lgcc -warn_unused"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "crtend.o%s \
   %{!shared: \
     %{mabi=32:crtn.o%s}\
     %{mabi=n32:%{mips4:/usr/lib32/mips4/crtn.o%s}\
       %{!mips4:/usr/lib32/mips3/crtn.o%s}}\
     %{mabi=64:%{mips4:/usr/lib64/mips4/crtn.o%s}\
       %{!mips4:/usr/lib64/mips3/crtn.o%s}}\
     %{!mabi*:%{mips4:/usr/lib32/mips4/crtn.o%s}\
       %{!mips4:/usr/lib32/mips3/crtn.o%s}}}"

/* ??? If no mabi=X option give, but a mipsX option is, then should depend
   on the mipsX option.  */
#undef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%{call_shared} %{no_archive} %{exact_version} %{w} \
%{!shared: %{!non_shared: %{!call_shared: -call_shared -no_unresolved}}} \
%{rpath} -init __do_global_ctors -fini __do_global_dtors \
%{shared:-hidden_symbol __do_global_ctors,__do_global_ctors_1,__do_global_dtors} \
-_SYSTYPE_SVR4 -woff 131 \
%{mabi=32: -32}%{mabi=n32: -n32}%{mabi=64: -64}%{!mabi*: -n32}"

#define MIPS_TFMODE_FORMAT ibm_extended_format
