/* Definitions for SOM assembler support.
   Copyright (C) 1999-2024 Free Software Foundation, Inc.

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

/* So we can conditionalize small amounts of code in pa.cc or pa.md.  */
#undef TARGET_SOM
#define TARGET_SOM 1

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
  if (startswith (PTR, "shared library list:"))			\
    {								\
      PTR = 0;							\
      in_shlib_list = 1;					\
    }								\
  else if (startswith (PTR, "shared library binding:"))		\
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (startswith (PTR, "static branch prediction disabled")) \
    {								\
      PTR = 0;							\
      in_shlib_list = 0;					\
    }								\
  else if (in_shlib_list					\
	   && startswith (PTR, "dynamic"))			\
    {								\
      PTR += sizeof ("dynamic") - 1;				\
      while (*p == ' ') PTR++;					\
    }								\
  else if (in_shlib_list					\
	   && startswith (PTR, "static"))			\
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


#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
    do { tree tree_type = TREE_TYPE (DECL);				\
	 tree fntype = TREE_TYPE (tree_type);				\
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
		  parm = DECL_CHAIN (parm))				\
	       {							\
		 tree type = DECL_ARG_TYPE (parm);			\
		 machine_mode mode = TYPE_MODE (type);			\
		 if (!AGGREGATE_TYPE_P (type)				\
		     && mode == SFmode && ! TARGET_SOFT_FLOAT)		\
		   fprintf (FILE, ",ARGW%d=FR", i++);			\
		 else if (!AGGREGATE_TYPE_P (type)			\
			  && mode == DFmode && ! TARGET_SOFT_FLOAT)	\
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
		     int arg_size = pa_function_arg_size (mode, type);	\
		     /* Passing structs by invisible reference uses	\
			one general register.  */			\
		     if (arg_size > 2 || TREE_ADDRESSABLE (type))	\
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
	     if (stdarg_p (tree_type))					\
	       {							\
		 for (; i < 4; i++)					\
		   fprintf (FILE, ",ARGW%d=GR", i);			\
	       }							\
	     if (!AGGREGATE_TYPE_P (fntype)				\
		 && TYPE_MODE (fntype) == DFmode			\
		 && ! TARGET_SOFT_FLOAT)				\
	       fputs (DFMODE_RETURN_STRING, FILE);			\
	     else if (!AGGREGATE_TYPE_P (fntype)			\
		      && TYPE_MODE (fntype) == SFmode			\
		      && ! TARGET_SOFT_FLOAT)				\
	       fputs (SFMODE_RETURN_STRING, FILE);			\
	     else if (fntype != void_type_node)				\
	       fputs (",RTNVAL=GR", FILE);				\
	     fputs ("\n", FILE);					\
	   }} while (0)

#define TARGET_ASM_FILE_START pa_som_file_start
#define TARGET_ASM_INIT_SECTIONS pa_som_asm_init_sections

/* String to output before writable data.  */
#define DATA_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $DATA$\n"

/* String to output before uninitialized data.  */
#define BSS_SECTION_ASM_OP "\t.SPACE $PRIVATE$\n\t.SUBSPA $BSS$\n"

/* This is how to output a command to make the user-level label
   named NAME defined for reference from other files.  We use
   assemble_name_raw instead of assemble_name since a symbol in
   a .IMPORT directive that isn't otherwise referenced is not
   placed in the symbol table of the assembled object.

   Failure to import a function reference can cause the HP linker
   to segmentation fault!

   Note that the SOM based tools need the symbol imported as a
   CODE symbol, while the ELF based tools require the symbol to
   be imported as an ENTRY symbol.  */

#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME) \
  pa_hpux_asm_output_external ((FILE), (DECL), (NAME))
#define ASM_OUTPUT_EXTERNAL_REAL(FILE, DECL, NAME) \
  do { fputs ("\t.IMPORT ", FILE);					\
       assemble_name_raw (FILE, NAME);					\
       if (FUNCTION_NAME_P (NAME))					\
	 fputs (",CODE\n", FILE);					\
       else								\
	 fputs (",DATA\n", FILE);					\
     } while (0)

/* The bogus HP assembler requires ALL external references to be
   "imported", even library calls.  They look a bit different, so
   here's this macro.

   Also note not all libcall names are passed to pa_encode_section_info
   (__main for example).  To make sure all libcall names have section
   info recorded in them, we do it here.

   We must also ensure that a libcall that has been previously
   exported is not subsequently imported since the HP assembler may
   change the type from an ENTRY to a CODE symbol.  This would make
   the symbol local.  We are forced to use the identifier node
   associated with the real assembler name for this check as the
   symbol_ref available in ASM_DECLARE_FUNCTION_NAME is not the
   same as the one used here.  As a result, we can't use flags
   in the symbol_ref for this check.  The identifier check assumes
   assemble_external_libcall is called before the symbol is used.  */

#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, RTL) \
  do { const char *name;						\
       tree id;								\
									\
       if (!function_label_operand (RTL, VOIDmode))			\
	 pa_encode_label (RTL);						\
									\
       name = targetm.strip_name_encoding (XSTR ((RTL), 0));		\
       id = maybe_get_identifier (name);				\
       if (!id || !TREE_SYMBOL_REFERENCED (id))				\
	 {								\
	   fputs ("\t.IMPORT ", FILE);					\
	   assemble_name_raw (FILE, XSTR ((RTL), 0));		       	\
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
  extern void __gcc_plt_call (void);		\
  void (*reference)(void) = &__gcc_plt_call;	\
  func_ptr *p;					\
  __asm__ ("" : : "r" (reference));		\
  for (p = __DTOR_LIST__ + 1; *p; )		\
    (*p++) ();					\
} while (0)

/* This macro specifies the biggest alignment supported by the object
   file format of this machine.

   The .align directive in the HP assembler allows alignments up to 4096
   bytes.  However, the maximum alignment of a global common symbol is 8
   bytes for objects smaller than the page size (4096 bytes).  For larger
   objects, the linker provides an alignment of 32 bytes.  Unfortunately,
   this macro doesn't provide a mechanism to test for common symbols.  */
#define MAX_OFILE_ALIGNMENT 32768

/* The SOM linker hardcodes paths into binaries.  As a result, dotdots
   must be removed from library prefixes to prevent binaries from depending
   on the location of the GCC tool directory.  The downside is GCC
   cannot be moved after installation using a symlink.  */
#undef TARGET_ALWAYS_STRIP_DOTDOT
#define TARGET_ALWAYS_STRIP_DOTDOT true

/* If GAS supports weak, we can support weak when we have working linker
   support for secondary definitions and are generating code for GAS.
   This is primarily for one-only support as SOM doesn't allow undefined
   weak symbols or weak aliases.  */
#ifdef HAVE_GAS_WEAK
#define TARGET_SUPPORTS_WEAK (TARGET_SOM_SDEF && TARGET_GAS)
#else
#define TARGET_SUPPORTS_WEAK 0
#endif

/* CVS GAS as of 4/28/04 supports a comdat parameter for the .nsubspa
   directive.  This provides one-only linkage semantics even though we
   don't have weak support.  */
#ifdef HAVE_GAS_NSUBSPA_COMDAT
#define SUPPORTS_SOM_COMDAT (TARGET_GAS)
#else
#define SUPPORTS_SOM_COMDAT 0
#endif

/* We can support one only if we support weak or comdat.  */
#define SUPPORTS_ONE_ONLY (TARGET_SUPPORTS_WEAK || SUPPORTS_SOM_COMDAT)

/* We use DECL_COMMON for uninitialized one-only variables as we don't
   have linkonce .bss.  We use SOM secondary definitions or comdat for
   initialized variables and functions.  */
#define MAKE_DECL_ONE_ONLY(DECL) \
  do {									\
    if (VAR_P (DECL)					\
        && (DECL_INITIAL (DECL) == 0					\
            || DECL_INITIAL (DECL) == error_mark_node))			\
      DECL_COMMON (DECL) = 1;						\
    else if (TARGET_SUPPORTS_WEAK)					\
      DECL_WEAK (DECL) = 1;						\
  } while (0)

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
#if defined HAVE_GAS_WEAK
#define ASM_WEAKEN_DECL(FILE,DECL,NAME,VALUE) \
  do									\
    {									\
      if ((VALUE) != NULL)						\
	error_at (DECL_SOURCE_LOCATION (DECL),				\
		  "weak aliases are not supported");			\
      fputs ("\t.weak\t", FILE);					\
      assemble_name (FILE, NAME);					\
      fputc ('\n', FILE);						\
									\
      /* Import external objects.  */					\
      if (DECL_EXTERNAL (DECL))						\
	{								\
	  fputs ("\t.IMPORT ", FILE);					\
	  assemble_name (FILE, NAME);					\
	  if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	    fputs (",CODE\n", FILE);					\
	  else								\
	    fputs (",DATA\n", FILE);					\
	}								\
      /* Functions are globalized by ASM_DECLARE_FUNCTION_NAME.  */	\
      else if (TREE_CODE (DECL) != FUNCTION_DECL)			\
	{								\
	  fputs ("\t.EXPORT ", FILE);					\
	  assemble_name (FILE, NAME);					\
	  fputs (",DATA\n", FILE);					\
	}								\
    }									\
  while (0)
#endif

/* Although gas accepts .weakref, it doesn't provide the correct symbol
   type for function references.  For now, we use ASM_WEAKEN_DECL instead.
   We have to undefine HAVE_GAS_WEAKREF to prevent default.h from defining
   ASM_OUTPUT_WEAKREF.  */
#undef HAVE_GAS_WEAKREF

/* We can't handle weak aliases, and therefore can't support pragma weak.
   Suppress the use of pragma weak in gthr-dce.h and gthr-posix.h.  */
#define GTHREAD_USE_WEAK 0

/* Shared library suffix.  Collect2 strips the version string after
   this suffix when generating constructor/destructor names.  */
#define SHLIB_SUFFIX ".sl"

/* We don't have named sections.  */
#define TARGET_HAVE_NAMED_SECTIONS false

#define TARGET_ASM_TM_CLONE_TABLE_SECTION pa_som_tm_clone_table_section

/* Generate specially named labels to identify DWARF 2 frame unwind
   information.  */
#define EH_FRAME_THROUGH_COLLECT2
