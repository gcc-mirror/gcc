/* OSF/1 1.3 now is compitable with SVR4, so include sysv4.h, and
   put difference here.  */

#include <stdio.h>
#include "i386/sysv4.h"	/* Base i386 target machine definitions */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 OSF/1)");

/* WORD_SWITCH_TAKES_ARG defined in svr4 is not correct. We also
 need an extra -soname */
#undef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)                    \
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)                 \
  || !strcmp (STR, "Tdata") || !strcmp (STR, "Ttext") \
  || !strcmp (STR, "Tbss") || !strcmp (STR, "soname"))

/* Note, -fpic and -fPIC are equivalent */
#undef  CPP_SPEC
#define CPP_SPEC "\
%{fpic: -D__SHARED__} %{fPIC: %{!fpic: -D__SHARED__}} \
%{.S:	%{!ansi:%{!traditional:%{!traditional-cpp:%{!ftraditional: -traditional}}}}} \
%{.S:	-D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C} \
%{!.S:	-D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"

/* -mmcount or -mno-mcount should be used with -pg or -p */
#undef  CC1_SPEC
#define CC1_SPEC "%{p: %{!mmcount: %{!mno-mcount: -mno-mcount }}} \
%{!p: %{pg: %{!mmcount: %{!mno-mcount: -mno-mcount }}}}"

/* Note, -D__NO_UNDERSCORES__ -D__ELF__ are provided in the older version of
   OSF/1 gcc. We keep them here, so that old /usr/include/i386/asm.h works.
   */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-D__NO_UNDERSCORES__ -D__ELF__ -DOSF -DOSF1 -Di386 -Dunix -Asystem(xpg4) -Asystem(osf1) -Acpu(i386) -Amachine(i386)"

/* current OSF/1 doesn't provide separate crti.o and gcrti.o (and also, crtn.o
   and gcrtn.o) for profile.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
                         %{!symbolic: \
                          %{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}}\
			crti.o%s \
			crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef	ASM_SPEC
#define ASM_SPEC       "%{v*: -v}"

#undef  LINK_SPEC
#define LINK_SPEC      "%{v*: -v} \
		%{h*} %{z*} \
		%{dy:-call_shared} %{dn:-static} \
		%{static:-static} \
		%{shared:-shared} \
		%{call_shared:-call_shared} \
		%{symbolic:-Bsymbolic -shared -call_shared} \
		%{!dy: %{!dn: %{!static: %{!shared: %{!symbolic: \
			%{noshrlib: -static } \
			%{!noshrlib: -call_shared}}}}}}"

#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX	"/usr/ccs/gcc/"

#undef  MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"

/* Define this macro meaning that gcc should find the library 'libgcc.a'
   by hand, rather than passing the argument '-lgcc' to tell the linker
   to do the search */
#define LINK_LIBGCC_SPECIAL

/* This goes with LINK_LIBGCC_SPECIAL, we need tell libgcc.a differently */
#undef  LIBGCC_SPEC
#define LIBGCC_SPEC "%{!shared:%{!symbolic:libgcc.a%s}}"

/* A C statement to output assembler commands which will identify the object
  file as having been compile with GNU CC. We don't need or want this for
  OSF1. */
#undef ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(FILE)

/* Identify the front-end which produced this file.  To keep symbol
   space down, and not confuse kdb, only do this if the language is
   not C.  */
#define ASM_IDENTIFY_LANGUAGE(STREAM)                                   \
{                                                                       \
  if (strcmp (lang_identify (), "c") != 0)                              \
    output_lang_identify (STREAM);                                      \
}

/* Specify size_t, ptrdiff_t, and wchar_t types.  */
#undef  SIZE_TYPE
#undef  PTRDIFF_TYPE
#undef  WCHAR_TYPE
#undef  WCHAR_TYPE_SIZE

#define SIZE_TYPE       "long unsigned int"
#define PTRDIFF_TYPE    "int"
#define WCHAR_TYPE      "unsigned int"
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Turn off long double being 96 bits.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64

/* Work with OSF/1 profile */
#define MASK_NO_MCOUNT		000200000000	/* profiling uses mcount_ptr */

#define TARGET_MCOUNT		((target_flags & MASK_NO_MCOUNT) == 0)

#undef	SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES						\
     { "mcount",		-MASK_NO_MCOUNT, "Profiling uses mcount" },			\
     { "no-mcount",		 MASK_NO_MCOUNT, "" },

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.

   We override it here to allow for the new profiling code to go before
   the prologue and the old mcount code to go after the prologue (and
   after %ebx has been set up for ELF shared library support).  */
#if 0
#define OSF_PROFILE_BEFORE_PROLOGUE					\
  (!TARGET_MCOUNT							\
   && !current_function_needs_context					\
   && (!flag_pic							\
       || !frame_pointer_needed						\
       || (!current_function_uses_pic_offset_table			\
	   && !current_function_uses_const_pool)))
#else
#define OSF_PROFILE_BEFORE_PROLOGUE 0
#endif
#undef	FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE, SIZE)					\
do									\
  {									\
    char *prefix = "";			\
    char *lprefix = LPREFIX;						\
    int labelno = profile_label_no;					\
									\
    if (profile_flag && OSF_PROFILE_BEFORE_PROLOGUE)			\
      {									\
	if (!flag_pic)				\
	  {								\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall *%s_mcount_ptr\n", prefix);		\
	  }								\
									\
	else								\
	  {								\
	    static int call_no = 0;					\
									\
	    fprintf (FILE, "\tcall %sPc%d\n", lprefix, call_no);	\
	    fprintf (FILE, "%sPc%d:\tpopl %%eax\n", lprefix, call_no);	\
	    fprintf (FILE, "\taddl $_GLOBAL_OFFSET_TABLE_+[.-%sPc%d],%%eax\n", \
		     lprefix, call_no++);				\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%eax),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tmovl %s_mcount_ptr@GOT(%%eax),%%eax\n",	\
		     prefix);						\
	    fprintf (FILE, "\tcall *(%%eax)\n");			\
	  }								\
      }									\
									\
    function_prologue (FILE, SIZE);					\
  }									\
while (0)

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GNU CC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results. */

#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)				\
do									\
  {									\
    if (!OSF_PROFILE_BEFORE_PROLOGUE)					\
      {									\
	char *prefix = "";			\
	char *lprefix = LPREFIX;					\
	int labelno = LABELNO;					\
									\
	/* Note that OSF/rose blew it in terms of calling mcount,	\
	   since OSF/rose prepends a leading underscore, but mcount's	\
	   doesn't.  At present, we keep this kludge for ELF as well	\
	   to allow old kernels to build profiling.  */			\
									\
	if (flag_pic							\
	    && !current_function_uses_pic_offset_table			\
	    && !current_function_uses_const_pool)			\
	  abort ();							\
									\
	if (TARGET_MCOUNT && flag_pic)					\
	  {								\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tcall *%smcount@GOT(%%ebx)\n", prefix);	\
	  }								\
									\
	else if (TARGET_MCOUNT)						\
	  {								\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall %smcount\n", prefix);		\
	  }								\
									\
	else if (flag_pic && frame_pointer_needed)			\
	  {								\
	    fprintf (FILE, "\tmovl 4(%%ebp),%%ecx\n");			\
	    fprintf (FILE, "\tpushl %%ecx\n");				\
	    fprintf (FILE, "\tleal %sP%d@GOTOFF(%%ebx),%%edx\n",	\
		     lprefix, labelno);					\
	    fprintf (FILE, "\tmovl _mcount_ptr@GOT(%%ebx),%%eax\n");	\
	    fprintf (FILE, "\tcall *(%%eax)\n");			\
	    fprintf (FILE, "\tpopl %%eax\n");				\
	  }								\
									\
	else if (frame_pointer_needed)					\
	  {								\
	    fprintf (FILE, "\tmovl 4(%%ebp),%%ecx\n");			\
	    fprintf (FILE, "\tpushl %%ecx\n");				\
	    fprintf (FILE, "\tmovl $%sP%d,%%edx\n", lprefix, labelno);	\
	    fprintf (FILE, "\tcall *_mcount_ptr\n");			\
	    fprintf (FILE, "\tpopl %%eax\n");				\
	  }								\
									\
	else								\
	  abort ();							\
      }									\
  }									\
while (0)

#if defined (CROSS_COMPILE) && defined (HOST_BITS_PER_INT) && defined (HOST_BITS_PER_LONG) && defined (HOST_BITS_PER_LONGLONG)
#if (HOST_BITS_PER_INT==32) && (HOST_BITS_PER_LONG==64) && (HOST_BITS_PER_LONGLONG==64)
#define REAL_ARITHMETIC
#endif
#endif
