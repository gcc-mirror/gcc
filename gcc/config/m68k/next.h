/* next.h:  definitions for NeXT.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m68k.h"

/* Use new NeXT include file search path.  */

#ifndef CROSS_COMPILE /* In a cross compiler with NeXT as target, don't expect
			 the host to use Next's directory scheme.  */
#define INCLUDE_DEFAULTS				\
  {							\
    { GPLUSPLUS_INCLUDE_DIR, 1},			\
    { GCC_INCLUDE_DIR, 0},				\
    { LOCAL_INCLUDE_DIR, 0},				\
    { "/NextDeveloper/Headers", 0},			\
    { "/NextDeveloper/Headers/ansi", 0},		\
    { "/NextDeveloper/Headers/bsd", 0},			\
    { "/LocalDeveloper/Headers", 0},			\
    { "/LocalDeveloper/Headers/ansi", 0},		\
    { "/LocalDeveloper/Headers/bsd", 0},		\
    { "/NextDeveloper/2.0CompatibleHeaders", 0},	\
    { STANDARD_INCLUDE_DIR, 0},				\
    { 0, 0}						\
  }
#endif /* CROSS_COMPILE */

#define EXTRA_FORMAT_FUNCTIONS \
      "NXPrintf",	FALSE,	2,	FALSE,	\
      "NXScanf",	TRUE,	2,	FALSE,	\
      "NXVPrintf",	FALSE,	2,	TRUE,	\
      "NXVScanf",	TRUE,	2,	TRUE,	\
      "DPSPrintf",	FALSE,	2,	FALSE,	\
      "bsd_sprintf",	FALSE,	2,	FALSE,	\
      "bsd_vsprintf",	FALSE,	2,	TRUE,

/* Use NeXT's special calling convention for sending an Objc message.  */
#define NEXT_OBJC_RUNTIME

/* We have atexit.  */
#define HAVE_ATEXIT

/* Enable recent gcc to compile under the old gcc in Next release 1.0.  */
#define __inline inline

/* See m68k.h.  0407 means 68040 (or 68030 or 68020, with 68881/2).  */

#define TARGET_DEFAULT 0407

/* wchar_t is unsigned short */

#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE "short unsigned int"
#define WCHAR_TYPE_SIZE (BITS_PER_WORD / 2)

/* Give methods pretty symbol names on NeXT. */

#define OBJC_GEN_METHOD_LABEL(BUF,IS_INST,CLASS_NAME,CAT_NAME,SEL_NAME)	\
  do { if (CAT_NAME)							\
	 sprintf (BUF, "%c[%s(%s) %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (CAT_NAME), (SEL_NAME));		\
       else								\
	 sprintf (BUF, "%c[%s %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (SEL_NAME));				\
     } while (0)

/* Wrap new method names in quotes so the assembler doesn't gag.
   Make Objective-C internal symbols local.  */

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  do { if (NAME[0] == '+' || NAME[0] == '-') fprintf (FILE, "\"%s\"", NAME); \
       else if (!strncmp (NAME, "_OBJC_", 6)) fprintf (FILE, "L%s", NAME);   \
       else if (!strncmp (NAME, ".objc_class_name_", 17))		\
	 fprintf (FILE, "%s", NAME);					\
       else fprintf (FILE, "_%s", NAME); } while (0)

#undef STACK_BOUNDARY
/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 32

/* These compiler options take n arguments.  */

#define WORD_SWITCH_TAKES_ARG(STR)	 	\
  (!strcmp (STR, "Ttext") ? 1 :			\
   !strcmp (STR, "Tdata") ? 1 :			\
   !strcmp (STR, "Tbss") ? 1 :			\
   !strcmp (STR, "include") ? 1 :		\
   !strcmp (STR, "imacros") ? 1 :		\
   !strcmp (STR, "segalign") ? 1 :		\
   !strcmp (STR, "seg1addr") ? 1 :		\
   !strcmp (STR, "segaddr") ? 2 :		\
   !strcmp (STR, "sectobjectsymbols") ? 2 :	\
   !strcmp (STR, "segprot") ? 3 :		\
   !strcmp (STR, "sectcreate") ? 3 :		\
   !strcmp (STR, "sectalign") ? 3 :		\
   !strcmp (STR, "segcreate") ? 3 :		\
   !strcmp (STR, "sectorder") ? 3 :		\
   !strcmp (STR, "aux-info") ? 1 :		\
   0)

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dmc68000 -Dm68k -DNeXT -Dunix -D__MACH__ -D__ARCHITECTURE__=\"m68k\""

/* Machine dependent ccp options.  */

/* This option used to be called -bsd, but that conflicts with the
   general -b option.  */
#define CPP_SPEC "%{strict-bsd:-D__STRICT_BSD__}"

/* Machine dependent ld options.  */

#define LINK_SPEC "%{Z} %{M} \
%{execute*} %{object*} %{preload*} %{fvmlib*} \
%{segalign*} %{seg1addr*} %{segaddr*} %{segprot*} \
%{seglinkedit*} %{noseglinkedit*} \
%{sectcreate*} %{sectalign*} %{sectobjectsymbols}\
%{segcreate*} %{Mach*} %{whyload} %{w} \
%{sectorder*} %{whatsloaded}"

/* Machine dependent libraries.  */

#define LIB_SPEC "%{!p:%{!pg:-lsys_s}} %{pg:-lsys_p}"
 
/* We specify crt0.o as -lcrt0.o so that ld will search the library path. */
#define STARTFILE_SPEC  \
  "%{pg:-lgcrt0.o}%{!pg: \
     %{p:%e-p profiling is no longer supported.  Use -pg instead.} \
     %{!p:-lcrt0.o}}"

/* Every structure or union's size must be a multiple of 2 bytes.
   (Why isn't this in m68k.h?)  */

#define STRUCTURE_SIZE_BOUNDARY 16

/* Why not? */

#define DOLLARS_IN_IDENTIFIERS 2

/* Allow #sscs (but don't do anything). */

#define SCCS_DIRECTIVE

/* We use Dbx symbol format.  */

#define DBX_DEBUGGING_INFO

/* This saves a fair amount of space. */

#define DBX_CONTIN_LENGTH 0

/* These screw up NeXT's gdb at the moment, so don't use them. */

#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(FILE, FILENAME)

/* gdb needs a null N_SO at the end of each file for scattered loading. */

#define DBX_OUTPUT_MAIN_SOURCE_FILE_END(FILE, FILENAME)			\
  fprintf (FILE,							\
	   "\t.text\n\t.stabs \"%s\",%d,0,0,Letext\nLetext:\n",		\
	   "" , N_SO)

/* Don't use .gcc_compiled symbols to communicate with GDB;
   They interfere with numerically sorted symbol lists. */

#define ASM_IDENTIFY_GCC(asm_out_file)

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "\t.double 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.double 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "\t.single 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.single 0r%.20e\n", (VALUE)))

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "#0r%.9g", (VALUE)))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (REAL_VALUE_ISINF ((VALUE))						\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "#0r%.20g", (VALUE)))

#if 0 /* This is for system verson 3.0, which isn't out yet.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do { constructor_section ();			\
       ASM_OUTPUT_ALIGN (FILE, 1);		\
       fprintf (FILE, "\t.long ");		\
       assemble_name (FILE, NAME);		\
       fprintf (FILE, "\n"); } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)	\
  do { destructor_section ();			\
       ASM_OUTPUT_ALIGN (FILE, 1);		\
       fprintf (FILE, "\t.long ");		\
       assemble_name (FILE, NAME);		\
       fprintf (FILE, "\n"); } while (0)
#endif

/* How to parse #pragma's */

#define HANDLE_PRAGMA(finput) handle_pragma (finput)

/* Create new Mach-O sections. */

#define SECTION_FUNCTION(FUNCTION, SECTION, DIRECTIVE, WAS_TEXT)	\
void									\
FUNCTION ()								\
{									\
  extern void text_section ();					 	\
  extern int flag_no_mach_text_sections;		 	      	\
  									\
  if (WAS_TEXT && flag_no_mach_text_sections)       			\
    text_section ();							\
  else if (in_section != SECTION)					\
    {									\
      fprintf (asm_out_file, "%s\n", DIRECTIVE);			\
      in_section = SECTION;						\
    }									\
}									\

#define EXTRA_SECTIONS					\
  in_const, in_cstring, in_literal4, in_literal8,	\
  in_constructor, in_destructor,			\
  in_objc_class, in_objc_meta_class, in_objc_category,	\
  in_objc_class_vars, in_objc_instance_vars,		\
  in_objc_cls_meth, in_objc_inst_meth,			\
  in_objc_cat_cls_meth, in_objc_cat_inst_meth,		\
  in_objc_selector_strs, in_objc_selector_refs,		\
  in_objc_symbols, in_objc_module_info

#define EXTRA_SECTION_FUNCTIONS			\
SECTION_FUNCTION (const_section,		\
		  in_const,			\
		  ".const", 1)			\
SECTION_FUNCTION (cstring_section,		\
		  in_cstring,			\
		  ".cstring", 1)		\
SECTION_FUNCTION (literal4_section,		\
		  in_literal4,			\
		  ".literal4", 1)		\
SECTION_FUNCTION (literal8_section,		\
		  in_literal8,			\
		  ".literal8", 1)		\
SECTION_FUNCTION (constructor_section,		\
		  in_constructor,		\
		  ".constructor", 0)		\
SECTION_FUNCTION (destructor_section,		\
		  in_destructor,		\
		  ".destructor", 0)		\
SECTION_FUNCTION (objc_class_section,		\
		  in_objc_class,		\
		  ".objc_class", 0)		\
SECTION_FUNCTION (objc_meta_class_section,	\
		  in_objc_meta_class,		\
		  ".objc_meta_class", 0)	\
SECTION_FUNCTION (objc_category_section,	\
		  in_objc_category,		\
		".objc_category", 0)		\
SECTION_FUNCTION (objc_class_vars_section,	\
		  in_objc_class_vars,		\
		  ".objc_class_vars", 0)	\
SECTION_FUNCTION (objc_instance_vars_section,	\
		  in_objc_instance_vars,	\
		  ".objc_instance_vars", 0)	\
SECTION_FUNCTION (objc_cls_meth_section,	\
		  in_objc_cls_meth,		\
		  ".objc_cls_meth", 0)		\
SECTION_FUNCTION (objc_inst_meth_section,	\
		  in_objc_inst_meth,		\
		  ".objc_inst_meth", 0)		\
SECTION_FUNCTION (objc_cat_cls_meth_section,	\
		  in_objc_cat_cls_meth,		\
		  ".objc_cat_cls_meth", 0)	\
SECTION_FUNCTION (objc_cat_inst_meth_section,	\
		  in_objc_cat_inst_meth,	\
		  ".objc_cat_inst_meth", 0)	\
SECTION_FUNCTION (objc_selector_strs_section,	\
		  in_objc_selector_strs,	\
		  ".objc_selector_strs", 0)	\
SECTION_FUNCTION (objc_selector_refs_section,	\
		  in_objc_selector_refs,	\
		  ".objc_selector_refs", 0)	\
SECTION_FUNCTION (objc_symbols_section,		\
		  in_objc_symbols,		\
		  ".objc_symbols", 0)		\
SECTION_FUNCTION (objc_module_info_section,	\
		  in_objc_module_info,		\
		  ".objc_module_info", 0)

#define READONLY_DATA_SECTION const_section

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
      else if ((TREE_READONLY (exp) || TREE_CONSTANT (exp))	\
	       && !TREE_SIDE_EFFECTS (exp))				\
	readonly_data_section ();					\
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
	  else if (!strncmp (name, "_OBJC_CLASS_", 12))			\
	    objc_class_section ();					\
	  else if (!strncmp (name, "_OBJC_METACLASS_", 16))		\
	    objc_meta_class_section ();					\
	  else if (!strncmp (name, "_OBJC_CATEGORY_", 15))		\
	    objc_category_section ();					\
	  else if (!strncmp (name, "_OBJC_STRINGS", 13))		\
	    objc_selector_strs_section ();				\
	  else if (!strncmp (name, "_OBJC_SELECTOR_REFERENCES", 25))	\
	    objc_selector_refs_section ();				\
	  else if (!strncmp (name, "_OBJC_SYMBOLS", 13))		\
	    objc_symbols_section ();					\
	  else if (!strncmp (name, "_OBJC_MODULES", 13))		\
	    objc_module_info_section ();				\
	  else								\
	    data_section ();						\
	}								\
      else								\
        data_section ();						\
    }									\
  while (0)

/* Force the assembler to create all the Objective-C sections,
    so that their order is guaranteed. */
  
#define OBJC_PROLOGUE					\
  do {							\
	extern void objc_class_section ();		\
	extern void objc_meta_class_section ();		\
	extern void objc_cat_cls_meth_section ();	\
	extern void objc_cat_inst_meth_section ();	\
	extern void objc_cls_meth_section ();		\
	extern void objc_inst_meth_section ();		\
	extern void objc_selector_refs_section ();	\
	extern void objc_symbols_section ();		\
	extern void objc_category_section ();		\
	extern void objc_class_vars_section ();		\
	extern void objc_instance_vars_section ();	\
	extern void objc_module_info_section ();	\
	extern void objc_selector_strs_section ();	\
							\
	objc_class_section ();		\
	objc_meta_class_section ();	\
	objc_cat_cls_meth_section ();	\
	objc_cat_inst_meth_section ();	\
	objc_cls_meth_section ();	\
	objc_inst_meth_section ();	\
	objc_selector_refs_section ();	\
	objc_symbols_section ();	\
	objc_category_section ();	\
	objc_class_vars_section ();	\
	objc_instance_vars_section ();	\
	objc_module_info_section ();	\
	objc_selector_strs_section ();	\
     } while (0)

/* We do not define JUMP_TABLES_IN_TEXT_SECTION, since we wish to keep
   the text section pure.  There is no point in addressing the jump
   tables using pc relative addressing, since they are not in the text
   section, so we undefine CASE_VECTOR_PC_RELATIVE.  This also
   causes the compiler to use absolute addresses in the jump table,
   so we redefine CASE_VECTOR_MODE to be SImode. */

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE SImode

#undef CASE_VECTOR_PC_RELATIVE

/* Don't treat addresses involving labels differently from symbol names.
   Previously, references to labels generated pc-relative addressing modes
   while references to symbol names generated absolute addressing modes.  */

#undef GO_IF_INDEXABLE_BASE(X, ADDR)
#define GO_IF_INDEXABLE_BASE(X, ADDR)	\
{ if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X)) goto ADDR; }

#define ALIGN_ASM_OP		".align"

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t%s %d\n", ALIGN_ASM_OP, (LOG))

/* The maximum alignment which the object file format can support.
   For NeXT's Mach-O format, this is 2^15.  */

#define MAX_OFILE_ALIGNMENT 0x8000
