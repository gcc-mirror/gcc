/* Definitions of target machine for GNU compiler,
   for some generic XCOFF file format
   Copyright (C) 2001, 2002 Free Software Foundation, Inc.

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


#define TARGET_OBJECT_FORMAT OBJECT_XCOFF

/* The RS/6000 uses the XCOFF format.  */
#define XCOFF_DEBUGGING_INFO

/* Define if the object format being used is COFF or a superset.  */
#define OBJECT_FORMAT_COFF

/* Define the magic numbers that we recognize as COFF.
 
    AIX 4.3 adds U803XTOCMAGIC (0757) for 64-bit objects and AIX V5 adds
    U64_TOCMAGIC (0767), but collect2.c does not include files in the
    correct order to conditionally define the symbolic name in this macro.
 
    The AIX linker accepts import/export files as object files,
    so accept "#!" (0x2321) magic number.  */
#define MY_ISCOFF(magic) \
  ((magic) == U802WRMAGIC || (magic) == U802ROMAGIC \
   || (magic) == U802TOCMAGIC || (magic) == 0757 || (magic) == 0767 \
   || (magic) == 0x2321)

/* We don't have GAS for the RS/6000 yet, so don't write out special
    .stabs in cc1plus.  */

#define FASCIST_ASSEMBLER

/* We define this to prevent the name mangler from putting dollar signs into
   function names.  */

#define NO_DOLLAR_IN_LABEL

/* We define this to 0 so that gcc will never accept a dollar sign in a
   variable name.  This is needed because the AIX assembler will not accept
   dollar signs.  */

#define DOLLARS_IN_IDENTIFIERS 0

/* Define the extra sections we need.  We define three: one is the read-only
   data section which is used for constants.  This is a csect whose name is
   derived from the name of the input file.  The second is for initialized
   global variables.  This is a csect whose name is that of the variable.
   The third is the TOC.  */

#define EXTRA_SECTIONS \
   read_only_data, private_data, read_only_private_data, toc, bss

/* Define the routines to implement these extra sections.
   BIGGEST_ALIGNMENT is 64, so align the sections that much.  */

#define EXTRA_SECTION_FUNCTIONS				\
							\
void							\
read_only_data_section ()				\
{							\
  if (in_section != read_only_data)			\
    {							\
      fprintf (asm_out_file, "\t.csect %s[RO],3\n",	\
	       xcoff_read_only_section_name);		\
      in_section = read_only_data;			\
    }							\
}							\
							\
void							\
private_data_section ()					\
{							\
  if (in_section != private_data)			\
    {							\
      fprintf (asm_out_file, "\t.csect %s[RW],3\n",	\
	       xcoff_private_data_section_name);	\
      in_section = private_data;			\
    }							\
}							\
							\
void							\
read_only_private_data_section ()			\
{							\
  if (in_section != read_only_private_data)		\
    {							\
      fprintf (asm_out_file, "\t.csect %s[RO],3\n",	\
	       xcoff_private_data_section_name);	\
      in_section = read_only_private_data;		\
    }							\
}							\
							\
void							\
toc_section ()						\
{							\
  if (TARGET_MINIMAL_TOC)				\
    {							\
      /* toc_section is always called at least once from ASM_FILE_START, \
	 so this is guaranteed to always be defined once and only once   \
	 in each file.  */						 \
      if (! toc_initialized)				\
	{						\
	  fputs ("\t.toc\nLCTOC..1:\n", asm_out_file);	\
	  fputs ("\t.tc toc_table[TC],toc_table[RW]\n", asm_out_file); \
	  toc_initialized = 1;				\
	}						\
							\
      if (in_section != toc)				\
	fprintf (asm_out_file, "\t.csect toc_table[RW]%s\n",	\
		 (TARGET_32BIT ? "" : ",3"));		\
    }							\
  else							\
    {							\
      if (in_section != toc)				\
        fputs ("\t.toc\n", asm_out_file);		\
    }							\
  in_section = toc;					\
}

/* Define the name of our readonly data section.  */

#define READONLY_DATA_SECTION read_only_data_section

/* Select the section for an initialized data object.

   On the RS/6000, we have a special section for all variables except those
   that are static.  */

#define SELECT_SECTION(EXP,RELOC,ALIGN)			\
{							\
  if ((TREE_CODE (EXP) == STRING_CST			\
       && ! flag_writable_strings)			\
      || (TREE_CODE_CLASS (TREE_CODE (EXP)) == 'd'	\
	  && TREE_READONLY (EXP) && ! TREE_THIS_VOLATILE (EXP) \
	  && DECL_INITIAL (EXP)				\
	  && (DECL_INITIAL (EXP) == error_mark_node	\
	      || TREE_CONSTANT (DECL_INITIAL (EXP)))	\
	  && ! (RELOC)))				\
    {							\
      if (TREE_PUBLIC (EXP))				\
        read_only_data_section ();			\
      else						\
        read_only_private_data_section ();		\
    }							\
  else							\
    {							\
      if (TREE_PUBLIC (EXP))				\
        data_section ();				\
      else						\
        private_data_section ();			\
    }							\
}

/* Return non-zero if this entry is to be written into the constant
   pool in a special way.  We do so if this is a SYMBOL_REF, LABEL_REF
   or a CONST containing one of them.  If -mfp-in-toc (the default),
   we also do this for floating-point constants.  We actually can only
   do this if the FP formats of the target and host machines are the
   same, but we can't check that since not every file that uses
   GO_IF_LEGITIMATE_ADDRESS_P includes real.h.  We also do this when
   we can write the entry into the TOC and the entry is not larger
   than a TOC entry.  */

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY_P(X, MODE)			\
  (TARGET_TOC								\
   && (GET_CODE (X) == SYMBOL_REF					\
       || (GET_CODE (X) == CONST && GET_CODE (XEXP (X, 0)) == PLUS	\
	   && GET_CODE (XEXP (XEXP (X, 0), 0)) == SYMBOL_REF)		\
       || GET_CODE (X) == LABEL_REF					\
       || (GET_CODE (X) == CONST_INT 					\
	   && GET_MODE_BITSIZE (MODE) <= GET_MODE_BITSIZE (Pmode))	\
       || (GET_CODE (X) == CONST_DOUBLE					\
	   && (TARGET_POWERPC64						\
	       || TARGET_MINIMAL_TOC					\
	       || (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT		\
		   && ! TARGET_NO_FP_IN_TOC)))))

/* Select section for constant in constant pool.

   On RS/6000, all constants are in the private read-only data area.
   However, if this is being placed in the TOC it must be output as a
   toc entry.  */

#define SELECT_RTX_SECTION(MODE, X, ALIGN)		\
{ if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (X, MODE))	\
    toc_section ();					\
  else							\
    read_only_private_data_section ();			\
}

/* If we are referencing a function that is static or is known to be
   in this file, make the SYMBOL_REF special.  We can use this to indicate
   that we can branch to this function without emitting a no-op after the
   call.  Do not set this flag if the function is weakly defined.  */

#define ENCODE_SECTION_INFO(DECL)			\
  if (TREE_CODE (DECL) == FUNCTION_DECL			\
      && !TREE_PUBLIC (DECL)				\
      && !DECL_WEAK (DECL))				\
    SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;

/* FP save and restore routines.  */
#define	SAVE_FP_PREFIX "._savef"
#define SAVE_FP_SUFFIX ""
#define	RESTORE_FP_PREFIX "._restf"
#define RESTORE_FP_SUFFIX ""

/* Function name to call to do profiling.  */
#undef RS6000_MCOUNT
#define RS6000_MCOUNT ".__mcount"

/* Function names to call to do floating point truncation.  */

#undef RS6000_ITRUNC
#define RS6000_ITRUNC "__itrunc"
#undef RS6000_UITRUNC
#define RS6000_UITRUNC "__uitrunc"

/* This outputs NAME to FILE up to the first null or '['.  */

#define RS6000_OUTPUT_BASENAME(FILE, NAME)	\
  {						\
    const char *_p;				\
						\
    STRIP_NAME_ENCODING (_p, (NAME));		\
    assemble_name ((FILE), _p);			\
  }

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { RS6000_OUTPUT_BASENAME (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\t.globl ", FILE);	\
       RS6000_OUTPUT_BASENAME (FILE, NAME); putc ('\n', FILE);} while (0)

/* Remove any trailing [DS] or the like from the symbol name.  */

#define STRIP_NAME_ENCODING(VAR,NAME)			\
  do							\
    {							\
      const char *_name = (NAME);			\
      size_t _len;					\
      if (*_name == '*')				\
        _name++;					\
      _len = strlen (_name);				\
      if (_name[_len - 1] != ']')			\
	(VAR) = _name;					\
      else						\
	{						\
	  char *_new_name = (char *) alloca (_len + 1);	\
	  strcpy (_new_name, _name);			\
	  _new_name[_len - 4] = '\0';			\
	  (VAR) = _new_name;				\
	}						\
    }							\
  while (0)

/* Output at beginning of assembler file.

   Initialize the section names for the RS/6000 at this point.

   Specify filename, including full path, to assembler.

   We want to go into the TOC section so at least one .toc will be emitted.
   Also, in order to output proper .bs/.es pairs, we need at least one static
   [RW] section emitted.

   Finally, declare mcount when profiling to make the assembler happy.  */

#define ASM_FILE_START(FILE)					\
{								\
  rs6000_gen_section_name (&xcoff_bss_section_name,		\
			   main_input_filename, ".bss_");	\
  rs6000_gen_section_name (&xcoff_private_data_section_name,	\
			   main_input_filename, ".rw_");	\
  rs6000_gen_section_name (&xcoff_read_only_section_name,	\
			   main_input_filename, ".ro_");	\
								\
  fprintf (FILE, "\t.file\t\"%s\"\n", main_input_filename);	\
  if (TARGET_64BIT)						\
    fputs ("\t.machine\t\"ppc64\"\n", FILE);			\
  toc_section ();						\
  if (write_symbols != NO_DEBUG)				\
    private_data_section ();					\
  text_section ();						\
  if (profile_flag)						\
    fprintf (FILE, "\t.extern %s\n", RS6000_MCOUNT);		\
  rs6000_file_start (FILE, TARGET_CPU_DEFAULT);			\
}

/* Output at end of assembler file.

   On the RS/6000, referencing data should automatically pull in text.  */

#define ASM_FILE_END(FILE)					\
{								\
  text_section ();						\
  fputs ("_section_.text:\n", FILE);				\
  data_section ();						\
  fputs (TARGET_32BIT						\
	 ? "\t.long _section_.text\n" : "\t.llong _section_.text\n", FILE); \
}

/* This macro produces the initial definition of a function name.
   On the RS/6000, we need to place an extra '.' in the function name and
   output the function descriptor.

   The csect for the function will have already been created by the
   `text_section' call previously done.  We do have to go back to that
   csect, however.

   We also record that the function exists in the current compilation
   unit, reachable by short branch, by setting SYMBOL_REF_FLAG.

   The third and fourth parameters to the .function pseudo-op (16 and 044)
   are placeholders which no longer have any use.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL)		\
{ rtx sym_ref = XEXP (DECL_RTL (DECL), 0);			\
  if (!DECL_WEAK (DECL))					\
    SYMBOL_REF_FLAG (sym_ref) = 1;				\
  if (TREE_PUBLIC (DECL))					\
    {								\
      if (!RS6000_WEAK || !DECL_WEAK (decl))			\
	{							\
	  fputs ("\t.globl .", FILE);				\
	  RS6000_OUTPUT_BASENAME (FILE, NAME);			\
	  putc ('\n', FILE);					\
	}							\
    }								\
  else								\
    {								\
      fputs ("\t.lglobl .", FILE);				\
      RS6000_OUTPUT_BASENAME (FILE, NAME);			\
      putc ('\n', FILE);					\
    }								\
  fputs ("\t.csect ", FILE);					\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fputs (TARGET_32BIT ? "[DS]\n" : "[DS],3\n", FILE);		\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fputs (":\n", FILE);						\
  fputs (TARGET_32BIT ? "\t.long ." : "\t.llong .", FILE);	\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fputs (", TOC[tc0], 0\n", FILE);				\
  in_section = no_section;					\
  function_section(DECL);					\
  putc ('.', FILE);						\
  RS6000_OUTPUT_BASENAME (FILE, NAME);				\
  fputs (":\n", FILE);						\
  if (write_symbols == XCOFF_DEBUG)				\
    xcoffout_declare_function (FILE, DECL, NAME);		\
}

/* Output a reference to SYM on FILE.  */

#define ASM_OUTPUT_SYMBOL_REF(FILE, SYM) \
  rs6000_output_symbol_ref (FILE, SYM)

/* This says how to output an external.  */

#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(FILE, DECL, NAME)	\
{ rtx _symref = XEXP (DECL_RTL (DECL), 0);	\
  if ((TREE_CODE (DECL) == VAR_DECL		\
       || TREE_CODE (DECL) == FUNCTION_DECL)	\
      && (NAME)[strlen (NAME) - 1] != ']')	\
    {						\
      char *_name = (char *) permalloc (strlen (XSTR (_symref, 0)) + 5); \
      strcpy (_name, XSTR (_symref, 0));	\
      strcat (_name, TREE_CODE (DECL) == FUNCTION_DECL ? "[DS]" : "[RW]"); \
      XSTR (_symref, 0) = _name;		\
    }						\
}

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s..%u:\n", (PREFIX), (unsigned) (NUM))

/* This is how to output an internal label prefix.  rs6000.c uses this
   when generating traceback tables.  */

#define ASM_OUTPUT_INTERNAL_LABEL_PREFIX(FILE,PREFIX)   \
  fprintf (FILE, "%s..", PREFIX)

/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed.  */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
{ ASM_OUTPUT_ALIGN (FILE, 2); ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM); }

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s..%u", (PREFIX), (unsigned) (NUM))

/* This is how to output an assembler line to define N characters starting
   at P to FILE.  */

#define ASM_OUTPUT_ASCII(FILE, P, N)  output_ascii ((FILE), (P), (N))

/* This is how to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNMENT)	\
  do { fputs ("\t.comm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       if ( (SIZE) > 4)					\
         fprintf ((FILE), ",%d,3\n", (SIZE));		\
       else						\
	 fprintf ((FILE), ",%d\n", (SIZE));		\
  } while (0)

/* This says how to output an assembler line
   to define a local common symbol.
   Alignment cannot be specified, but we can try to maintain
   alignment after preceding TOC section if it was aligned
   for 64-bit mode.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do { fputs ("\t.lcomm ", (FILE));			\
       RS6000_OUTPUT_BASENAME ((FILE), (NAME));		\
       fprintf ((FILE), ",%d,%s\n", (TARGET_32BIT ? (SIZE) : (ROUNDED)), \
		xcoff_bss_section_name);		\
     } while (0)

/* This is how we tell the assembler that two symbols have the same value.  */
#define SET_ASM_OP "\t.set "

/* This is how we tell the assembler to equate two values.  */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "%s", SET_ASM_OP);				\
	RS6000_OUTPUT_BASENAME (FILE, LABEL1);				\
	fprintf (FILE, ",");						\
	RS6000_OUTPUT_BASENAME (FILE, LABEL2);				\
	fprintf (FILE, "\n");						\
  } while (0)

/* Used by rs6000_assemble_integer, among others.  */
#define DOUBLE_INT_ASM_OP "\t.llong\t"

/* Output before instructions.  */
#define TEXT_SECTION_ASM_OP "\t.csect .text[PR]"

/* Output before writable data.
   Align entire section to BIGGEST_ALIGNMENT.  */
#define DATA_SECTION_ASM_OP "\t.csect .data[RW],3"

/* Define unique section name -- functions only.  */
#define UNIQUE_SECTION(DECL,RELOC)			\
  do {							\
    int len;						\
    const char *name;					\
    char *string;					\
							\
    if (TREE_CODE (DECL) == FUNCTION_DECL) {		\
      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL)); \
      len = strlen (name) + 5;				\
      string = alloca (len + 1);			\
      sprintf (string, ".%s[PR]", name);		\
      DECL_SECTION_NAME (DECL) = build_string (len, string); \
    }							\
  } while (0)

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION  xcoff_asm_named_section

/* Define the name of the section to use for the EH language specific
   data areas (.gcc_except_table on most other systems).  */
#define TARGET_ASM_EXCEPTION_SECTION data_section

/* Define to prevent DWARF2 unwind info in the data section rather
   than in the .eh_frame section.  We do this because the AIX linker
   would otherwise garbage collect these sections.  */
#define EH_FRAME_IN_DATA_SECTION 1
