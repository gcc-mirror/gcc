/* nextstep.h -- operating system specific defines to be used when
   targeting GCC for NeXTSTEP.
   Copyright (C) 1989, 1990, 1991, 1992, 1993 Free Software Foundation, Inc.

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

/* Use new NeXT include file search path.
   In a cross compiler with NeXT as target, don't expect
   the host to use Next's directory scheme.  */

#ifndef CROSS_COMPILE
#undef	INCLUDE_DEFAULTS
#define INCLUDE_DEFAULTS				\
  {							\
    { GPLUSPLUS_INCLUDE_DIR, 1, 1 },			\
    { LOCAL_INCLUDE_DIR, 0, 1 },			\
    { TOOL_INCLUDE_DIR, 0, 1 },				\
    { GCC_INCLUDE_DIR, 0, 0 },				\
    /* These are for fixincludes-fixed ansi/bsd headers	\
       which wouldn't be found otherwise.		\
       (The use of string catenation here is OK since	\
	NeXT's native compiler is derived from GCC.) */	\
    { GCC_INCLUDE_DIR "/ansi", 0, 0 },			\
    { GCC_INCLUDE_DIR "/bsd", 0, 0 },			\
    { "/NextDeveloper/Headers", 0, 0 },			\
    { "/NextDeveloper/Headers/ansi", 0, 0 },		\
    { "/NextDeveloper/Headers/bsd", 0, 0 },		\
    { "/LocalDeveloper/Headers", 0, 0 },		\
    { "/LocalDeveloper/Headers/ansi", 0, 0 },		\
    { "/LocalDeveloper/Headers/bsd", 0, 0 },		\
    { "/NextDeveloper/2.0CompatibleHeaders", 0, 0 },	\
    { STANDARD_INCLUDE_DIR, 0, 0 },			\
    { "/usr/include/bsd", 0, 0 },			\
    { 0, 0, 0 }						\
  }
#endif /* CROSS_COMPILE */

#undef	EXTRA_FORMAT_FUNCTIONS
#define EXTRA_FORMAT_FUNCTIONS \
      "NXPrintf",	FALSE,	2,	FALSE,	\
      "NXScanf",	TRUE,	2,	FALSE,	\
      "NXVPrintf",	FALSE,	2,	TRUE,	\
      "NXVScanf",	TRUE,	2,	TRUE,	\
      "DPSPrintf",	FALSE,	2,	FALSE,	\
      "bsd_sprintf",	FALSE,	2,	FALSE,	\
      "bsd_vsprintf",	FALSE,	2,	TRUE,

/* Make -fnext-runtime the default.  */

#define NEXT_OBJC_RUNTIME

/* We have atexit.  */

#define HAVE_ATEXIT

/* Enable recent gcc to compile under the old gcc in Next release 1.0.  */

#define __inline inline

/* wchar_t is unsigned short */

#undef	WCHAR_TYPE
#define WCHAR_TYPE "short unsigned int"
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE (BITS_PER_WORD / 2)

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */

#undef	DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* These compiler options take n arguments.  */

#undef	WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR)	 	\
  (DEFAULT_WORD_SWITCH_TAKES_ARG (STR) ? 1 :	\
   !strcmp (STR, "segalign") ? 1 :		\
   !strcmp (STR, "seg1addr") ? 1 :		\
   !strcmp (STR, "segaddr") ? 2 :		\
   !strcmp (STR, "sectobjectsymbols") ? 2 :	\
   !strcmp (STR, "segprot") ? 3 :		\
   !strcmp (STR, "sectcreate") ? 3 :		\
   !strcmp (STR, "sectalign") ? 3 :		\
   !strcmp (STR, "segcreate") ? 3 :		\
   !strcmp (STR, "sectorder") ? 3 :		\
   !strcmp (STR, "siff-mask") ? 1 :		\
   !strcmp (STR, "siff-filter") ? 1 :		\
   !strcmp (STR, "siff-warning") ? 1 :		\
   !strcmp (STR, "arch") ? 1 :			\
   !strcmp (STR, "pagezero_size") ? 1 :		\
   0)

#undef	WORD_SWITCH
#define WORD_SWITCH(STR)			\
  (WORD_SWITCH_TAKES_ARG (STR)			\
   || !strcmp (STR, "bsd")			\
   || !strcmp (STR, "object")			\
   || !strcmp (STR, "ObjC")			\
   || !strcmp (STR, "all_load"))

/* Machine dependent ccp options.  */

#undef	CPP_SPEC
#define CPP_SPEC "%{!traditional: -D__STDC__}            \
                  %{posixstrict:-D_POSIX_SOURCE}         \
                  %{!posixstrict:%{bsd:-D__STRICT_BSD__} \
                  %{posix:-D_POSIX_SOURCE}               \
                  %{!ansi:-D_NEXT_SOURCE}}               \
                  %{MD:-MD %M} %{MMD:-MMD %M}"

/* Machine dependent ld options.  */

#undef	LINK_SPEC
#define LINK_SPEC "%{Z} %{M} \
%{execute*} %{preload*} %{fvmlib*} \
%{segalign*} %{seg1addr*} %{segaddr*} %{segprot*} \
%{pagezero_size*} \
%{seglinkedit*} %{noseglinkedit*} \
%{sectcreate*} %{sectalign*} %{sectobjectsymbols}\
%{segcreate*} %{Mach*} %{whyload} %{w} \
%{sectorder*} %{whatsloaded} %{ObjC} %{all_load} %{object}"

/* Machine dependent libraries.  */

#undef	LIB_SPEC
#define LIB_SPEC "%{!posix*:-lsys_s} %{posix*:-lposix}"

/* We specify crt0.o as -lcrt0.o so that ld will search the library path. */

#undef	STARTFILE_SPEC
#define STARTFILE_SPEC  \
    "%{!posix*:%{pg:-lgcrt0.o}%{!pg: \
     %{p:%e-p profiling is no longer supported.  Use -pg instead.} \
     %{!p:-lcrt0.o}}}\
     %{posix*:%{pg:-lgposixcrt0.o}%{!pg: \
     %{p:%e-p profiling is no longer supported.  Use -pg instead.} \
     %{!p:-lposixcrt0.o}}}"

/* Why not? */

#undef	DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 2

/* Allow #sscs (but don't do anything). */

#define SCCS_DIRECTIVE

/* We use Dbx symbol format.  */

#undef	SDB_DEBUGGING_INFO
#undef	XCOFF_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO

/* This saves a fair amount of space. */

#undef	DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 0

/* These screw up NeXT's gdb at the moment, so don't use them. */

#undef	DBX_OUTPUT_MAIN_SOURCE_DIRECTORY
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(FILE, FILENAME)

/* These come from bsd386.h, but are specific to sequent, so make sure
   they don't bite us.  */

#undef	DBX_NO_XREFS
#undef	DBX_CONTIN_LENGTH

/* gdb needs a null N_SO at the end of each file for scattered loading. */

#undef	DBX_OUTPUT_MAIN_SOURCE_FILE_END
#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  fprintf (FILE,							\
	   "\t.text\n\t.stabs \"%s\",%d,0,0,Letext\nLetext:\n",		\
	   "" , N_SO)

/* Don't use .gcc_compiled symbols to communicate with GDB;
   They interfere with numerically sorted symbol lists. */

#undef	ASM_IDENTIFY_GCC
#define ASM_IDENTIFY_GCC(asm_out_file)
#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP
#undef	INVOKE__main

#undef	ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)                       \
  do { constructor_section ();                                  \
       ASM_OUTPUT_ALIGN (FILE, 1);                              \
       fprintf (FILE, "\t.long ");                              \
       assemble_name (FILE, NAME);                              \
       fprintf (FILE, "\n");                                    \
       fprintf (FILE, ".reference .constructors_used\n");       \
      } while (0)

#undef	ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)                        \
  do { destructor_section ();                                   \
       ASM_OUTPUT_ALIGN (FILE, 1);                              \
       fprintf (FILE, "\t.long ");                              \
       assemble_name (FILE, NAME);                              \
       fprintf (FILE, "\n");                                    \
       fprintf (FILE, ".reference .destructors_used\n");        \
      } while (0)

/* Don't output a .file directive.  That is only used by the assembler for
   error reporting.  */
#undef	ASM_FILE_START
#define ASM_FILE_START(FILE)

#undef	ASM_FILE_END
#define ASM_FILE_END(FILE)					\
  do {								\
    extern char *language_string;				\
    if (strcmp (language_string, "GNU C++") == 0)		\
      {								\
	constructor_section ();					\
	destructor_section ();					\
	ASM_OUTPUT_ALIGN (FILE, 1);				\
      }								\
  } while (0)

/* How to parse #pragma's */

#undef	HANDLE_PRAGMA
#define HANDLE_PRAGMA(finput) handle_pragma (finput, &get_directive_line)

/* Give methods pretty symbol names on NeXT. */

#undef	OBJC_GEN_METHOD_LABEL
#define OBJC_GEN_METHOD_LABEL(BUF,IS_INST,CLASS_NAME,CAT_NAME,SEL_NAME,NUM) \
  do { if (CAT_NAME)							\
	 sprintf (BUF, "%c[%s(%s) %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (CAT_NAME), (SEL_NAME));		\
       else								\
	 sprintf (BUF, "%c[%s %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (SEL_NAME));				\
     } while (0)

/* Wrap new method names in quotes so the assembler doesn't gag.
   Make Objective-C internal symbols local.  */

#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  do { if (NAME[0] == '+' || NAME[0] == '-') fprintf (FILE, "\"%s\"", NAME); \
       else if (!strncmp (NAME, "_OBJC_", 6)) fprintf (FILE, "L%s", NAME);   \
       else if (!strncmp (NAME, ".objc_class_name_", 17))		\
	 fprintf (FILE, "%s", NAME);					\
       else fprintf (FILE, "_%s", NAME); } while (0)

#undef	ALIGN_ASM_OP
#define ALIGN_ASM_OP		".align"

#undef	ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t%s %d\n", ALIGN_ASM_OP, (LOG))

/* Ensure correct alignment of bss data.  */

#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN) \
( fputs (".lcomm ", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u,%u\n", (SIZE), floor_log2 ((ALIGN) / BITS_PER_UNIT)))

/* Output #ident as a .ident.  */

#undef	ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* The maximum alignment which the object file format can support.
   For NeXT's Mach-O format, this is 2^15.  */

#undef	MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT 0x8000

/* Create new Mach-O sections. */

#undef	SECTION_FUNCTION
#define SECTION_FUNCTION(FUNCTION, SECTION, DIRECTIVE, WAS_TEXT, OBJC)	\
void									\
FUNCTION ()								\
{									\
  extern void text_section ();					 	\
  extern void objc_section_init ();					\
  extern int flag_no_mach_text_sections;				\
  									\
  if (WAS_TEXT && flag_no_mach_text_sections)       			\
    text_section ();							\
  else if (in_section != SECTION)					\
    {									\
      if (OBJC)								\
	objc_section_init ();						\
      fprintf (asm_out_file, "%s\n", DIRECTIVE);			\
      in_section = SECTION;						\
    }									\
}									\

#undef	EXTRA_SECTIONS
#define EXTRA_SECTIONS					\
  in_const, in_cstring, in_literal4, in_literal8,	\
  in_constructor, in_destructor,			\
  in_objc_class, in_objc_meta_class, in_objc_category,	\
  in_objc_class_vars, in_objc_instance_vars,		\
  in_objc_cls_meth, in_objc_inst_meth,			\
  in_objc_cat_cls_meth, in_objc_cat_inst_meth,		\
  in_objc_selector_refs,				\
  in_objc_symbols, in_objc_module_info,			\
  in_objc_protocol, in_objc_string_object,		\
  in_objc_class_names, in_objc_meth_var_names,		\
  in_objc_meth_var_types, in_objc_cls_refs

#undef	EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS			\
SECTION_FUNCTION (const_section,		\
		  in_const,			\
		  ".const", 1, 0)		\
SECTION_FUNCTION (cstring_section,		\
		  in_cstring,			\
		  ".cstring", 1, 0)		\
SECTION_FUNCTION (literal4_section,		\
		  in_literal4,			\
		  ".literal4", 1, 0)		\
SECTION_FUNCTION (literal8_section,		\
		  in_literal8,			\
		  ".literal8", 1, 0)		\
SECTION_FUNCTION (constructor_section,		\
		  in_constructor,		\
		  ".constructor", 0, 0)		\
SECTION_FUNCTION (destructor_section,		\
		  in_destructor,		\
		  ".destructor", 0, 0)		\
SECTION_FUNCTION (objc_class_section,		\
		  in_objc_class,		\
		  ".objc_class", 0, 1)		\
SECTION_FUNCTION (objc_meta_class_section,	\
		  in_objc_meta_class,		\
		  ".objc_meta_class", 0, 1)	\
SECTION_FUNCTION (objc_category_section,	\
		  in_objc_category,		\
		".objc_category", 0, 1)		\
SECTION_FUNCTION (objc_class_vars_section,	\
		  in_objc_class_vars,		\
		  ".objc_class_vars", 0, 1)	\
SECTION_FUNCTION (objc_instance_vars_section,	\
		  in_objc_instance_vars,	\
		  ".objc_instance_vars", 0, 1)	\
SECTION_FUNCTION (objc_cls_meth_section,	\
		  in_objc_cls_meth,		\
		  ".objc_cls_meth", 0, 1)	\
SECTION_FUNCTION (objc_inst_meth_section,	\
		  in_objc_inst_meth,		\
		  ".objc_inst_meth", 0, 1)	\
SECTION_FUNCTION (objc_cat_cls_meth_section,	\
		  in_objc_cat_cls_meth,		\
		  ".objc_cat_cls_meth", 0, 1)	\
SECTION_FUNCTION (objc_cat_inst_meth_section,	\
		  in_objc_cat_inst_meth,	\
		  ".objc_cat_inst_meth", 0, 1)	\
SECTION_FUNCTION (objc_selector_refs_section,	\
		  in_objc_selector_refs,	\
		  ".objc_message_refs", 0, 1)	\
SECTION_FUNCTION (objc_symbols_section,		\
		  in_objc_symbols,		\
		  ".objc_symbols", 0, 1)	\
SECTION_FUNCTION (objc_module_info_section,	\
		  in_objc_module_info,		\
		  ".objc_module_info", 0, 1)	\
SECTION_FUNCTION (objc_protocol_section,	\
		  in_objc_protocol,		\
		  ".objc_protocol", 0, 1)	\
SECTION_FUNCTION (objc_string_object_section,	\
		  in_objc_string_object,	\
		  ".objc_string_object", 0, 1)	\
SECTION_FUNCTION (objc_class_names_section,	\
		in_objc_class_names,		\
		".objc_class_names", 0, 1)	\
SECTION_FUNCTION (objc_meth_var_names_section,	\
		in_objc_meth_var_names,		\
		".objc_meth_var_names", 0, 1)	\
SECTION_FUNCTION (objc_meth_var_types_section,	\
		in_objc_meth_var_types,		\
		".objc_meth_var_types", 0, 1)	\
SECTION_FUNCTION (objc_cls_refs_section,	\
		in_objc_cls_refs,		\
		".objc_cls_refs", 0, 1)		\
						\
void						\
objc_section_init ()				\
{						\
  static int been_here = 0;			\
						\
  if (been_here == 0)				\
    {						\
      been_here = 1;				\
      objc_class_section ();			\
      objc_meta_class_section ();		\
      objc_cat_cls_meth_section ();		\
      objc_cat_inst_meth_section ();		\
      objc_cls_meth_section ();			\
      objc_inst_meth_section ();		\
      objc_selector_refs_section ();		\
      objc_symbols_section ();			\
      objc_category_section ();			\
      objc_protocol_section ();			\
      objc_class_vars_section ();		\
      objc_instance_vars_section ();		\
      objc_module_info_section ();		\
      objc_string_object_section ();		\
      objc_class_names_section ();		\
      objc_meth_var_names_section ();		\
      objc_meth_var_types_section ();		\
      objc_cls_refs_section ();			\
    }						\
}

#undef	READONLY_DATA_SECTION
#define READONLY_DATA_SECTION const_section

#undef	SELECT_SECTION
#define SELECT_SECTION(exp,reloc)				\
  do								\
    {								\
      if (TREE_CODE (exp) == STRING_CST)			\
	{							\
	  if (flag_writable_strings)				\
	    data_section ();					\
	  else if (TREE_STRING_LENGTH (exp) !=			\
		   strlen (TREE_STRING_POINTER (exp)) + 1)	\
	    readonly_data_section ();				\
	  else							\
	    cstring_section ();					\
	}							\
      else if (TREE_CODE (exp) == INTEGER_CST			\
	       || TREE_CODE (exp) == REAL_CST)			\
        {							\
	  tree size = TYPE_SIZE (TREE_TYPE (exp));		\
	  							\
	  if (TREE_CODE (size) == INTEGER_CST &&		\
	      TREE_INT_CST_LOW (size) == 4 &&			\
	      TREE_INT_CST_HIGH (size) == 0)			\
	    literal4_section ();				\
	  else if (TREE_CODE (size) == INTEGER_CST &&		\
	      TREE_INT_CST_LOW (size) == 8 &&			\
	      TREE_INT_CST_HIGH (size) == 0)			\
	    literal8_section ();				\
	  else							\
	    readonly_data_section ();				\
	}							\
      else if (TREE_CODE (exp) == CONSTRUCTOR				\
	       && TREE_TYPE (exp)					\
	       && TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE		\
	       && TYPE_NAME (TREE_TYPE (exp))				\
	       && TREE_CODE (TYPE_NAME (TREE_TYPE (exp))) == IDENTIFIER_NODE \
	       && IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (exp))))	\
	{								\
	  if (!strcmp (IDENTIFIER_POINTER (TYPE_NAME (TREE_TYPE (exp))), \
			"NXConstantString"))				\
	  objc_string_object_section ();				\
	else if ((TREE_READONLY (exp) || TREE_CONSTANT (exp))		\
		&& !TREE_SIDE_EFFECTS (exp))				\
	  readonly_data_section ();					\
	else								\
	  data_section ();						\
      }									\
      else if (TREE_CODE (exp) == VAR_DECL &&				\
	       DECL_NAME (exp) &&					\
	       TREE_CODE (DECL_NAME (exp)) == IDENTIFIER_NODE &&	\
	       IDENTIFIER_POINTER (DECL_NAME (exp)) &&			\
	       !strncmp (IDENTIFIER_POINTER (DECL_NAME (exp)), "_OBJC_", 6)) \
	{								\
	  const char *name = IDENTIFIER_POINTER (DECL_NAME (exp));	\
	  								\
	  if (!strncmp (name, "_OBJC_CLASS_METHODS_", 20))		\
	    objc_cls_meth_section ();					\
	  else if (!strncmp (name, "_OBJC_INSTANCE_METHODS_", 23))	\
	    objc_inst_meth_section ();					\
	  else if (!strncmp (name, "_OBJC_CATEGORY_CLASS_METHODS_", 20)) \
	    objc_cat_cls_meth_section ();				\
	  else if (!strncmp (name, "_OBJC_CATEGORY_INSTANCE_METHODS_", 23)) \
	    objc_cat_inst_meth_section ();				\
	  else if (!strncmp (name, "_OBJC_CLASS_VARIABLES_", 22))	\
	    objc_class_vars_section ();					\
	  else if (!strncmp (name, "_OBJC_INSTANCE_VARIABLES_", 25))	\
	    objc_instance_vars_section ();				\
	  else if (!strncmp (name, "_OBJC_CLASS_PROTOCOLS_", 22))	\
	    objc_cat_cls_meth_section ();				\
	  else if (!strncmp (name, "_OBJC_CLASS_NAME_", 17))		\
	    objc_class_names_section ();				\
	  else if (!strncmp (name, "_OBJC_METH_VAR_NAME_", 20))		\
	    objc_meth_var_names_section ();				\
	  else if (!strncmp (name, "_OBJC_METH_VAR_TYPE_", 20))		\
	    objc_meth_var_types_section ();				\
	  else if (!strncmp (name, "_OBJC_CLASS_REFERENCES", 22))	\
	    objc_cls_refs_section ();					\
	  else if (!strncmp (name, "_OBJC_CLASS_", 12))			\
	    objc_class_section ();					\
	  else if (!strncmp (name, "_OBJC_METACLASS_", 16))		\
	    objc_meta_class_section ();					\
	  else if (!strncmp (name, "_OBJC_CATEGORY_", 15))		\
	    objc_category_section ();					\
	  else if (!strncmp (name, "_OBJC_SELECTOR_REFERENCES", 25))	\
	    objc_selector_refs_section ();				\
	  else if (!strncmp (name, "_OBJC_SYMBOLS", 13))		\
	    objc_symbols_section ();					\
	  else if (!strncmp (name, "_OBJC_MODULES", 13))		\
	    objc_module_info_section ();				\
	  else if (!strncmp (name, "_OBJC_PROTOCOL_INSTANCE_METHODS_", 32)) \
	    objc_cat_inst_meth_section ();                              \
	  else if (!strncmp (name, "_OBJC_PROTOCOL_CLASS_METHODS_", 29)) \
	    objc_cat_cls_meth_section ();                               \
	  else if (!strncmp (name, "_OBJC_PROTOCOL_REFS_", 20))         \
	    objc_cat_cls_meth_section ();                               \
	  else if (!strncmp (name, "_OBJC_PROTOCOL_", 15))              \
	    objc_protocol_section ();                                   \
	  else if ((TREE_READONLY (exp) || TREE_CONSTANT (exp))		\
		&& !TREE_SIDE_EFFECTS (exp))     			\
	    readonly_data_section ();                                   \
	  else								\
	    data_section ();						\
	}								\
      else if (TREE_CODE (exp) == VAR_DECL)				\
	{								\
	  if ((flag_pic && reloc)					\
	      || !TREE_READONLY (exp) || TREE_SIDE_EFFECTS (exp)	\
	      || !DECL_INITIAL (exp)					\
	      || (DECL_INITIAL (exp) != error_mark_node			\
		  && !TREE_CONSTANT (DECL_INITIAL (exp))))		\
	    data_section ();						\
	  else								\
	    readonly_data_section ();					\
	}								\
      else								\
	readonly_data_section ();					\
    }									\
  while (0)

#undef	SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(mode, rtx)					\
  do									\
    {									\
      if (GET_MODE_SIZE(mode) == 8)					\
	literal8_section();						\
      else if (GET_MODE_SIZE(mode) == 4)				\
	literal4_section();						\
      else								\
	const_section ();						\
    }									\
  while (0)
