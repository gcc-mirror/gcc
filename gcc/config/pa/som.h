/* Definitions for SOM assembler support.
   Copyright (C) 1999, 2001, 2002 Free Software Foundation, Inc.

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

/* So we can conditionalize small amounts of code in pa.c or pa.md.  */
#undef TARGET_SOM
#define TARGET_SOM 1

/* We do not use BINCL stabs in SOM.
   ??? If it does not hurt, we probably should to avoid useless divergence
   from other embedded stabs implementations.  */
#undef DBX_USE_BINCL

/* We make the first line stab special to avoid adding several
   gross hacks to GAS.  */
#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    static tree last_function_decl = NULL;		\
    if (current_function_decl == last_function_decl)	\
      fprintf (file, "\t.stabn 68,0,%d,L$M%d-%s\nL$M%d:\n",	\
	       line, sym_lineno,			\
	       XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0) + 1, \
	       sym_lineno);				\
    else						\
      fprintf (file, "\t.stabn 68,0,%d,0\n", line);	\
    last_function_decl = current_function_decl;		\
    sym_lineno += 1; }

/* gdb needs a null N_SO at the end of each file for scattered loading.  */

#undef	DBX_OUTPUT_MAIN_SOURCE_FILE_END
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME) \
  text_section (); \
  fputs ("\t.SPACE $TEXT$\n\t.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n", FILE); \
  fprintf (FILE,							\
	   "\t.stabs \"\",%d,0,0,L$text_end0000\nL$text_end0000:\n", N_SO)

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.  Because
   the HP assembler does auto alignment, it is necessary to use
   DW_EH_PE_aligned instead of the default DW_EH_PE_absptr.  */

#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (TARGET_GAS ? DW_EH_PE_absptr : DW_EH_PE_aligned)

/* HPUX has a program 'chatr' to list the dependencies of dynamically
   linked executables and shared libraries.  */
#define LDD_SUFFIX "chatr"
/* Look for lines like "dynamic   /usr/lib/X11R5/libX11.sl"
   or "static    /usr/lib/X11R5/libX11.sl". 

   HPUX 10.20 also has lines like "static branch prediction ..."
   so we filter that out explicitly.

   We also try to bound our search for libraries with marker
   lines.  What a pain.  */
#define PARSE_LDD_OUTPUT(PTR)					\
do {								\
  static int in_shlib_list = 0;					\
  while (*PTR == ' ') PTR++;					\
  if (strncmp (PTR, "shared library list:",			\
	       sizeof ("shared library list:") - 1) == 0)	\
    {								\
      PTR = 0;							\
      in_shlib_list = 1;					\
    }								\
  else if (strncmp (PTR, "shared library binding:",		\
		    sizeof ("shared library binding:") - 1) == 0)\
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (strncmp (PTR, "static branch prediction disabled",	\
		    sizeof ("static branch prediction disabled") - 1) == 0)\
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (in_shlib_list					\
	   &&  strncmp (PTR, "dynamic", sizeof ("dynamic") - 1) == 0) \
    {								\
      PTR += sizeof ("dynamic") - 1;				\
      while (*p == ' ') PTR++;					\
    }								\
  else if (in_shlib_list					\
	   && strncmp (PTR, "static", sizeof ("static") - 1) == 0) \
    {								\
      PTR += sizeof ("static") - 1;				\
      while (*p == ' ') PTR++;					\
    }								\
  else								\
    PTR = 0;							\
} while (0)

/* Output the label for a function definition.  */
#ifndef HP_FP_ARG_DESCRIPTOR_REVERSED
#define ASM_DOUBLE_ARG_DESCRIPTORS(FILE, ARG0, ARG1)	\
  do { fprintf (FILE, ",ARGW%d=FR", (ARG0));		\
       fprintf (FILE, ",ARGW%d=FU", (ARG1));} while (0)
#define DFMODE_RETURN_STRING ",RTNVAL=FU"
#define SFMODE_RETURN_STRING ",RTNVAL=FR"
#else
#define ASM_DOUBLE_ARG_DESCRIPTORS(FILE, ARG0, ARG1)	\
  do { fprintf (FILE, ",ARGW%d=FU", (ARG0));		\
       fprintf (FILE, ",ARGW%d=FR", (ARG1));} while (0)
#define DFMODE_RETURN_STRING ",RTNVAL=FR"
#define SFMODE_RETURN_STRING ",RTNVAL=FU"
#endif


/* NAME refers to the function's name.  If we are placing each function into
   its own section, we need to switch to the section for this function.  Note
   that the section name will have a "." prefix.  */
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE, NAME) \
  {									\
    const char *name = (*targetm.strip_name_encoding) (NAME);		\
    if (TARGET_GAS && in_section == in_text) 				\
      fputs ("\t.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n", FILE); \
    else if (TARGET_GAS)						\
      fprintf (FILE,							\
	       "\t.SUBSPA .%s\n", name);				\
  }
    
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
    do { tree fntype = TREE_TYPE (TREE_TYPE (DECL));			\
	 tree tree_type = TREE_TYPE (DECL);				\
	 tree parm;							\
	 int i;								\
	 if (TREE_PUBLIC (DECL) || TARGET_GAS)				\
	   { 								\
	     if (TREE_PUBLIC (DECL))					\
	       {							\
		 fputs ("\t.EXPORT ", FILE);				\
		 assemble_name (FILE, NAME);				\
		 fputs (",ENTRY,PRIV_LEV=3", FILE);			\
	       }							\
	     else							\
	       {							\
		 fputs ("\t.PARAM ", FILE);				\
		 assemble_name (FILE, NAME);				\
		 fputs (",PRIV_LEV=3", FILE);				\
	       }							\
	     for (parm = DECL_ARGUMENTS (DECL), i = 0; parm && i < 4;	\
		  parm = TREE_CHAIN (parm))				\
	       {							\
		 if (TYPE_MODE (DECL_ARG_TYPE (parm)) == SFmode		\
		     && ! TARGET_SOFT_FLOAT)				\
		   fprintf (FILE, ",ARGW%d=FR", i++);			\
		 else if (TYPE_MODE (DECL_ARG_TYPE (parm)) == DFmode	\
			  && ! TARGET_SOFT_FLOAT)			\
		   {							\
		     if (i <= 2)					\
		       {						\
			 if (i == 1) i++;				\
			 ASM_DOUBLE_ARG_DESCRIPTORS (FILE, i++, i++);	\
		       }						\
		     else						\
		       break;						\
		   }							\
		 else							\
		   {							\
		     int arg_size =					\
		       FUNCTION_ARG_SIZE (TYPE_MODE (DECL_ARG_TYPE (parm)),\
					  DECL_ARG_TYPE (parm));	\
		     /* Passing structs by invisible reference uses	\
			one general register.  */			\
		     if (arg_size > 2					\
			 || TREE_ADDRESSABLE (DECL_ARG_TYPE (parm)))	\
		       arg_size = 1;					\
		     if (arg_size == 2 && i <= 2)			\
		       {						\
			 if (i == 1) i++;				\
			 fprintf (FILE, ",ARGW%d=GR", i++);		\
			 fprintf (FILE, ",ARGW%d=GR", i++);		\
		       }						\
		     else if (arg_size == 1)				\
		       fprintf (FILE, ",ARGW%d=GR", i++);		\
		     else						\
		       i += arg_size;					\
		   }							\
	       }							\
	     /* anonymous args */					\
	     if (TYPE_ARG_TYPES (tree_type) != 0			\
		 && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (tree_type)))\
		     != void_type_node))				\
	       {							\
		 for (; i < 4; i++)					\
		   fprintf (FILE, ",ARGW%d=GR", i);			\
	       }							\
	     if (TYPE_MODE (fntype) == DFmode && ! TARGET_SOFT_FLOAT)	\
	       fputs (DFMODE_RETURN_STRING, FILE);			\
	     else if (TYPE_MODE (fntype) == SFmode && ! TARGET_SOFT_FLOAT) \
	       fputs (SFMODE_RETURN_STRING, FILE);			\
	     else if (fntype != void_type_node)				\
	       fputs (",RTNVAL=GR", FILE);				\
	     fputs ("\n", FILE);					\
	   }} while (0)

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) \
do {  \
     if (TARGET_PA_20) \
       fputs("\t.LEVEL 2.0\n", FILE); \
     else if (TARGET_PA_11) \
       fputs("\t.LEVEL 1.1\n", FILE); \
     else \
       fputs("\t.LEVEL 1.0\n", FILE); \
     fputs ("\t.SPACE $PRIVATE$\n\
\t.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31\n\
\t.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82\n\
\t.SPACE $TEXT$\n\
\t.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44\n\
\t.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n\
\t.IMPORT $global$,DATA\n\
\t.IMPORT $$dyncall,MILLICODE\n", FILE);\
     if (profile_flag)\
       fprintf (FILE, "\t.IMPORT _mcount, CODE\n");\
     if (write_symbols != NO_DEBUG) \
       output_file_directive ((FILE), main_input_filename); \
   } while (0)

/* Output before code.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define TEXT_SECTION_ASM_OP "\t.SPACE $TEXT$\n\t.SUBSPA $CODE$\n"

/* Output before read-only data.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define READONLY_DATA_ASM_OP "\t.SPACE $TEXT$\n\t.SUBSPA $LIT$\n"

#define EXTRA_SECTIONS in_readonly_data

#define EXTRA_SECTION_FUNCTIONS						\
extern void readonly_data PARAMS ((void));				\
void									\
readonly_data ()							\
{									\
  if (in_section != in_readonly_data)					\
    {									\
      in_section = in_readonly_data;					\
      fprintf (asm_out_file, "%s\n", READONLY_DATA_ASM_OP);		\
    }									\
}

/* FIXME: HPUX ld generates incorrect GOT entries for "T" fixups
   which reference data within the $TEXT$ space (for example constant
   strings in the $LIT$ subspace).

   The assemblers (GAS and HP as) both have problems with handling
   the difference of two symbols which is the other correct way to
   reference constant data during PIC code generation.

   So, there's no way to reference constant data which is in the
   $TEXT$ space during PIC generation.  Instead place all constant
   data into the $PRIVATE$ subspace (this reduces sharing, but it
   works correctly).  */

#define READONLY_DATA_SECTION (flag_pic ? data_section : readonly_data)

/* Output before writable data.  */

/* Supposedly the assembler rejects the command if there is no tab!  */
#define DATA_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $DATA$\n"

/* Output before uninitialized data.  */

#define BSS_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $BSS$\n"

/* We must not have a reference to an external symbol defined in a
   shared library in a readonly section, else the SOM linker will
   complain.

   So, we force exception information into the data section.  */
#define TARGET_ASM_EXCEPTION_SECTION data_section

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.

   We call assemble_name, which in turn sets TREE_SYMBOL_REFERENCED.  This
   macro will restore the original value of TREE_SYMBOL_REFERENCED to avoid
   placing useless function definitions in the output file.

   Also note that the SOM based tools need the symbol imported as a CODE
   symbol, while the ELF based tools require the symbol to be imported as
   an ENTRY symbol.  What a crock.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
  do { int save_referenced;					\
       save_referenced = TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)); \
       fputs ("\t.IMPORT ", FILE);				\
       assemble_name (FILE, NAME);				\
       if (FUNCTION_NAME_P (NAME))     				\
	 fputs (",CODE\n", FILE);				\
       else							\
	 fputs (",DATA\n", FILE);				\
       TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (DECL)) = save_referenced; \
     } while (0)

/* The bogus HP assembler requires ALL external references to be
   "imported", even library calls. They look a bit different, so
   here's this macro.

   Also note not all libcall names are passed to pa_encode_section_info
   (__main for example).  To make sure all libcall names have section
   info recorded in them, we do it here.  We must also ensure that
   we don't import a libcall that has been previously exported since
   the HP assembler may change an ENTRY symbol to a CODE symbol.  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, RTL) \
  do { const char *name;						\
       tree id;								\
									\
       if (!function_label_operand (RTL, VOIDmode))			\
	 hppa_encode_label (RTL);					\
									\
       name = (*targetm.strip_name_encoding) (XSTR ((RTL), 0));		\
       id = maybe_get_identifier (name);				\
       if (! id || ! TREE_SYMBOL_REFERENCED (id))			\
	 {								\
	   fputs ("\t.IMPORT ", FILE);					\
	   assemble_name (FILE, XSTR ((RTL), 0));		       	\
	   fputs (",CODE\n", FILE);					\
	 }								\
     } while (0)

/* We want __gcc_plt_call to appear in every program built by
   gcc, so we make a reference to it out of __main.
   We use the asm statement to fool the optimizer into not
   removing the dead (but important) initialization of
   REFERENCE.  */

#define DO_GLOBAL_DTORS_BODY			\
do {						\
  extern void __gcc_plt_call ();		\
  void (*reference)() = &__gcc_plt_call;	\
  func_ptr *p;					\
  __asm__ ("" : : "r" (reference));		\
  for (p = __DTOR_LIST__ + 1; *p; )		\
    (*p++) ();					\
} while (0)

/* The .align directive in the HP assembler allows up to a 32 alignment.  */
#define MAX_OFILE_ALIGNMENT 32768

/* The SOM linker hardcodes paths into binaries.  As a result, dotdots
   must be removed from library prefixes to prevent binaries from depending
   on the location of the GCC tool directory.  The downside is GCC
   cannot be moved after installation using a symlink.  */
#define ALWAYS_STRIP_DOTDOT 1

/* Aggregates with a single float or double field should be passed and
   returned in the general registers.  */
#define MEMBER_TYPE_FORCES_BLK(FIELD, MODE) (MODE==SFmode || MODE==DFmode)

/* If GAS supports weak, we can support weak when we have working linker
   support for secondary definitions and are generating code for GAS.  */
#ifdef HAVE_GAS_WEAK
#define SUPPORTS_WEAK (TARGET_SOM_SDEF && TARGET_GAS)
#else
#define SUPPORTS_WEAK 0
#endif

/* We can support one only if we support weak.  */
#define SUPPORTS_ONE_ONLY SUPPORTS_WEAK

/* Use weak (secondary definitions) to make one only declarations.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

/* This is how we tell the assembler that a symbol is weak.  The SOM
   weak implementation uses the secondary definition (sdef) flag.

   The behavior of sdef symbols is similar to ELF weak symbols in that
   multiple definitions can occur without incurring a link error.
   However, they differ in the following ways:
     1) Undefined sdef symbols are not allowed.
     2) The linker searches for undefined sdef symbols and will load an
	archive library member to resolve an undefined sdef symbol.
     3) The exported symbol from a shared library is a primary symbol
        rather than a sdef symbol.  Thus, more care is needed in the
	ordering of libraries.

   It appears that the linker discards extra copies of "weak" functions
   when linking shared libraries, independent of whether or not they
   are in their own section.  In linking final executables, -Wl,-O can
   be used to remove dead procedures.  Thus, support for named sections
   is not needed and in previous testing caused problems with various
   HP tools.  */
#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE);				\
       assemble_name (FILE, NAME);				\
       fputc ('\n', FILE);					\
       if (! FUNCTION_NAME_P (NAME))				\
	 {							\
	   fputs ("\t.EXPORT ", FILE);				\
	   assemble_name (FILE, NAME);				\
	   fputs (",DATA\n", FILE);				\
	 }							\
  } while (0)

/* We can't handle weak aliases, and therefore can't support pragma weak.
   Suppress the use of pragma weak in gthr-dce.h and gthr-posix.h.  */
#define GTHREAD_USE_WEAK 0
