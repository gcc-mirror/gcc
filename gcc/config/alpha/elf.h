/* Definitions of target machine for GNU compiler, for DEC Alpha w/ELF.
   Copyright (C) 1996, 1997, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Richard Henderson (rth@tamu.edu).

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
Boston, MA 02111-1307, USA.    */

#undef OBJECT_FORMAT_COFF
#undef EXTENDED_COFF
#define OBJECT_FORMAT_ELF

#define DBX_DEBUGGING_INFO
#define DWARF2_DEBUGGING_INFO

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#undef ASM_FINAL_SPEC

#undef  CC1_SPEC
#define CC1_SPEC  "%{G*}"

#undef  ASM_SPEC
#define ASM_SPEC  "%{G*} %{relax:-relax} %{!gstabs*:-no-mdebug}%{gstabs*:-mdebug}"

#undef  LINK_SPEC
#define LINK_SPEC "-m elf64alpha %{G*} %{relax:-relax}		\
  %{O*:-O3} %{!O*:-O1}						\
  %{shared:-shared}						\
  %{!shared:							\
    %{!static:							\
      %{rdynamic:-export-dynamic}				\
      %{!dynamic-linker:-dynamic-linker %(elf_dynamic_linker)}}	\
    %{static:-static}}"

/* Output at beginning of assembler file.  */
#undef  ASM_FILE_START
#define ASM_FILE_START(FILE)					\
do {								\
  if (write_symbols == DBX_DEBUG)				\
    {								\
      alpha_write_verstamp (FILE);				\
      output_file_directive (FILE, main_input_filename);	\
    }								\
  fprintf (FILE, "\t.set noat\n");				\
  fprintf (FILE, "\t.set noreorder\n");				\
  if (TARGET_BWX | TARGET_MAX | TARGET_FIX | TARGET_CIX)	\
    {								\
      fprintf (FILE, "\t.arch %s\n",				\
               (TARGET_CPU_EV6 ? "ev6"				\
                : TARGET_MAX ? "pca56" : "ev56"));		\
    }								\
} while (0)

#undef  IDENT_ASM_OP
#define IDENT_ASM_OP "\t.ident\t"

/* Allow #sccs in preprocessor.  */
#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */
#undef  ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(FILE, NAME) \
  fprintf (FILE, "%s\"%s\"\n", IDENT_ASM_OP, NAME);

/* This is how to allocate empty space in some section.  The .zero
   pseudo-op is used for this on most svr4 assemblers.  */

#undef  SKIP_ASM_OP
#define SKIP_ASM_OP	"\t.zero\t"

#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE, SIZE) \
  fprintf (FILE, "%s%u\n", SKIP_ASM_OP, (SIZE))

/* Output the label which precedes a jumptable.  Note that for all svr4
   systems where we actually generate jumptables (which is to say every
   svr4 target except i386, where we use casesi instead) we put the jump-
   tables into the .rodata section and since other stuff could have been
   put into the .rodata section prior to any given jumptable, we have to
   make sure that the location counter for the .rodata section gets pro-
   perly re-aligned prior to the actual beginning of the jump table.  */

#undef  ALIGN_ASM_OP
#define ALIGN_ASM_OP "\t.align\t"

#ifndef ASM_OUTPUT_BEFORE_CASE_LABEL
#define ASM_OUTPUT_BEFORE_CASE_LABEL(FILE, PREFIX, NUM, TABLE) \
  ASM_OUTPUT_ALIGN ((FILE), 2);
#endif

#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE)		\
  do {									\
    ASM_OUTPUT_BEFORE_CASE_LABEL (FILE, PREFIX, NUM, JUMPTABLE)		\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM);			\
  } while (0)

/* The standard SVR4 assembler seems to require that certain builtin
   library routines (e.g. .udiv) be explicitly declared as .globl
   in each assembly file where they are referenced.  */

#undef  ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(FILE, FUN)				\
  ASM_GLOBALIZE_LABEL (FILE, XSTR (FUN, 0))

/* This says how to output assembler code to declare an
   uninitialized external linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#undef  COMMON_ASM_OP
#define COMMON_ASM_OP	"\t.comm\t"

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
do {									\
  fprintf ((FILE), "%s", COMMON_ASM_OP);				\
  assemble_name ((FILE), (NAME));					\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ALIGN) / BITS_PER_UNIT);	\
} while (0)

/* This says how to output assembler code to declare an
   uninitialized internal linkage data object.  Under SVR4,
   the linker seems to want the alignment of data objects
   to depend on their types.  We do exactly that here.  */

#undef  ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN)		\
do {									\
  if ((SIZE) <= g_switch_value)						\
    sbss_section();							\
  else									\
    bss_section();							\
  fprintf (FILE, "%s", TYPE_ASM_OP);					\
  assemble_name (FILE, NAME);						\
  putc (',', FILE);							\
  fprintf (FILE, TYPE_OPERAND_FMT, "object");				\
  putc ('\n', FILE);							\
  if (!flag_inhibit_size_directive)					\
    {									\
      fprintf (FILE, "%s", SIZE_ASM_OP);				\
      assemble_name (FILE, NAME);					\
      fprintf (FILE, ",%d\n", (SIZE));					\
    }									\
  ASM_OUTPUT_ALIGN ((FILE), exact_log2((ALIGN) / BITS_PER_UNIT));	\
  ASM_OUTPUT_LABEL(FILE, NAME);						\
  ASM_OUTPUT_SKIP((FILE), (SIZE));					\
} while (0)

/* This is the pseudo-op used to generate a 64-bit word of data with a
   specific value in some section.  */

#undef  INT_ASM_OP
#define INT_ASM_OP		"\t.quad\t"

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'. 

   This value is really 2^63.  Since gcc figures the alignment in bits,
   we could only potentially get to 2^60 on suitible hosts.  Due to other
   considerations in varasm, we must restrict this to what fits in an int.  */

#undef  MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT \
  (1 << (HOST_BITS_PER_INT < 64 ? HOST_BITS_PER_INT - 2 : 62))

/* This is the pseudo-op used to generate a contiguous sequence of byte
   values from a double-quoted string WITHOUT HAVING A TERMINATING NUL
   AUTOMATICALLY APPENDED.  This is the same for most svr4 assemblers.  */

#undef  ASCII_DATA_ASM_OP
#define ASCII_DATA_ASM_OP	"\t.ascii\t"

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#undef USE_CONST_SECTION
#define USE_CONST_SECTION	1

#undef  CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP	"\t.section\t.rodata"

/* Define the pseudo-ops used to switch to the .ctors and .dtors sections.

   Note that we want to give these sections the SHF_WRITE attribute
   because these sections will actually contain data (i.e. tables of
   addresses of functions in the current root executable or shared library
   file) and, in the case of a shared library, the relocatable addresses
   will have to be properly resolved/relocated (and then written into) by
   the dynamic linker when it actually attaches the given shared library
   to the executing process.  (Note that on SVR4, you may wish to use the
   `-z text' option to the ELF linker, when building a shared library, as
   an additional check that you are doing everything right.  But if you do
   use the `-z text' option when building a shared library, you will get
   errors unless the .ctors and .dtors sections are marked as writable
   via the SHF_WRITE attribute.)  */

#undef  CTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"aw\""
#undef  DTORS_SECTION_ASM_OP
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"aw\""

/* Handle the small data sections.  */
#undef  BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"
#undef  SBSS_SECTION_ASM_OP
#define SBSS_SECTION_ASM_OP	"\t.section\t.sbss,\"aw\""
#undef  SDATA_SECTION_ASM_OP
#define SDATA_SECTION_ASM_OP	"\t.section\t.sdata,\"aw\""

/* On svr4, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known svr4 assemblers.  */

#undef  INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"\t.section\t.init"
#undef  FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	"\t.section\t.fini"

/* A default list of other sections which we might be "in" at any given
   time.  For targets that use additional sections (e.g. .tdesc) you
   should override this definition in the target-specific file which
   includes this file.  */

#undef  EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors, in_sbss, in_sdata

/* A default list of extra section function definitions.  For targets
   that use additional sections (e.g. .tdesc) you should override this
   definition in the target-specific file which includes this file.  */

#undef  EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  SECTION_FUNCTION_TEMPLATE(ctors_section, in_ctors, CTORS_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(dtors_section, in_dtors, DTORS_SECTION_ASM_OP) \
  SECTION_FUNCTION_TEMPLATE(sbss_section, in_sbss, SBSS_SECTION_ASM_OP)	\
  SECTION_FUNCTION_TEMPLATE(sdata_section, in_sdata, SDATA_SECTION_ASM_OP)

extern void ctors_section		PARAMS ((void));
extern void dtors_section		PARAMS ((void));
extern void sbss_section		PARAMS ((void));
extern void sdata_section		PARAMS ((void));

#undef  READONLY_DATA_SECTION
#define READONLY_DATA_SECTION() const_section ()

#undef  CONST_SECTION_FUNCTION
#define CONST_SECTION_FUNCTION					\
void								\
const_section ()						\
{								\
  if (!USE_CONST_SECTION)					\
    text_section();						\
  else if (in_section != in_const)				\
    {								\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);	\
      in_section = in_const;					\
    }								\
}

#undef  SECTION_FUNCTION_TEMPLATE
#define SECTION_FUNCTION_TEMPLATE(FN, ENUM, OP)	\
void FN ()					\
{						\
  if (in_section != ENUM)			\
    {						\
      fprintf (asm_out_file, "%s\n", OP);	\
      in_section = ENUM;			\
    }						\
}


/* Switch into a generic section.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.

   If the section has already been defined, we must not emit the
   attributes here. The SVR4 assembler does not recognize section
   redefinitions.  If DECL is NULL, no attributes are emitted.  */

#undef  ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME, RELOC)		\
  do									\
    {									\
      static htab_t htab;                                               \
                                                                        \
      struct section_info                                               \
      {									\
	enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;		\
      };                                                                \
                                                                        \
      struct section_info *s;						\
      const char *mode;							\
      enum sect_enum type;                                              \
      PTR* slot;                                                        \
                                                                        \
      /* The names we put in the hashtable will always be the unique    \
	 versions gived to us by the stringtable, so we can just use    \
	 their addresses as the keys.  */                               \
      if (!htab)                                                        \
	htab = htab_create (31,                                         \
			    htab_hash_pointer,                          \
			    htab_eq_pointer,                            \
			    NULL);                                      \
                                                                        \
      if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)			\
	type = SECT_EXEC, mode = "ax";					\
      else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))		\
	type = SECT_RO, mode = "a";					\
      else								\
	type = SECT_RW, mode = "aw";					\
      									\
      /* See if we already have an entry for this section.  */          \
      slot = htab_find_slot (htab, NAME, INSERT);                       \
      if (!*slot)                                                       \
	{                                                               \
	  s = (struct section_info *) xmalloc (sizeof (* s));		\
	  s->type = type;						\
	  *slot = s;							\
	  fprintf (FILE, "\t.section\t%s,\"%s\",@progbits\n",		\
		   NAME, mode);						\
	}								\
      else								\
	{								\
	  s = (struct section_info *) *slot;                            \
	  if (DECL && s->type != type)					\
	    error_with_decl (DECL,                                      \
			     "%s causes a section type conflict");      \
	  								\
	  fprintf (FILE, "\t.section\t%s\n", NAME);			\
	}								\
    }									\
  while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#undef  ASM_OUTPUT_CONSTRUCTOR
#define ASM_OUTPUT_CONSTRUCTOR(FILE, NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "%s", INT_ASM_OP);					\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#undef  ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE, NAME)      				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "%s", INT_ASM_OP);					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement or statements to switch to the appropriate
   section for output of DECL.  DECL is either a `VAR_DECL' node
   or a constant of some sort.  RELOC indicates whether forming
   the initial value of DECL requires link-time relocations.

   Set SECNUM to:
	0	.text
	1	.rodata
	2	.data
	3	.sdata
	4	.bss
	5	.sbss
*/

#define DO_SELECT_SECTION(SECNUM, DECL, RELOC)			\
  do								\
     {								\
       SECNUM = 1;						\
       if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	 SECNUM = 0;						\
       else if (TREE_CODE (DECL) == STRING_CST)			\
	 {							\
	   if (flag_writable_strings)				\
	     SECNUM = 2;					\
	 }							\
       else if (TREE_CODE (DECL) == VAR_DECL)			\
	 {							\
	   if (DECL_INITIAL (DECL) == NULL			\
	       || DECL_INITIAL (DECL) == error_mark_node)	\
	     SECNUM = 4;					\
	   else if ((flag_pic && RELOC)				\
		    || ! TREE_READONLY (DECL)			\
		    || TREE_SIDE_EFFECTS (DECL)			\
		    || ! TREE_CONSTANT (DECL_INITIAL (DECL)))	\
	     SECNUM = 2;					\
	 }							\
       else if (TREE_CODE (DECL) == CONSTRUCTOR)		\
	 {							\
	   if ((flag_pic && RELOC)				\
	       || ! TREE_READONLY (DECL)			\
	       || TREE_SIDE_EFFECTS (DECL)			\
	       || ! TREE_CONSTANT (DECL))			\
	     SECNUM = 2;					\
	 }							\
								\
       /* Select small data sections based on size.  */		\
       if (SECNUM >= 2)						\
	 {							\
	   int size = int_size_in_bytes (TREE_TYPE (DECL));	\
	   if (size >= 0 && size <= g_switch_value)		\
	     SECNUM += 1;					\
	 }							\
     }								\
   while (0)

#undef  SELECT_SECTION
#define SELECT_SECTION(DECL, RELOC)		\
  do						\
    {						\
      typedef void (*sec_fn) PARAMS ((void));	\
      static sec_fn const sec_functions[6] =	\
      {						\
	text_section,				\
	const_section,				\
	data_section,				\
	sdata_section,				\
	bss_section,				\
	sbss_section				\
      };					\
						\
      int sec;					\
						\
      DO_SELECT_SECTION (sec, DECL, RELOC);	\
						\
      (*sec_functions[sec]) ();			\
    }						\
  while (0)

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

#define UNIQUE_SECTION_P(DECL)   (DECL_ONE_ONLY (DECL))

#undef  UNIQUE_SECTION
#define UNIQUE_SECTION(DECL, RELOC)					\
  do									\
    {									\
      static const char * const prefixes[6][2] =			\
      {									\
	{ ".text.",   ".gnu.linkonce.t." },				\
	{ ".rodata.", ".gnu.linkonce.r." },				\
	{ ".data.",   ".gnu.linkonce.d." },				\
	{ ".sdata.",  ".gnu.linkonce.s." },				\
	{ ".bss.",    ".gnu.linkonce.b." },				\
	{ ".sbss.",   ".gnu.linkonce.sb." }				\
      };								\
									\
      int nlen, plen, sec;						\
      const char *name, *prefix;					\
      char *string;							\
									\
      DO_SELECT_SECTION (sec, DECL, RELOC);				\
									\
      name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));		\
      STRIP_NAME_ENCODING (name, name);					\
      nlen = strlen (name);						\
									\
      prefix = prefixes[sec][DECL_ONE_ONLY(DECL)];			\
      plen = strlen (prefix);						\
									\
      string = alloca (nlen + plen + 1);				\
									\
      memcpy (string, prefix, plen);					\
      memcpy (string + plen, name, nlen + 1);				\
									\
      DECL_SECTION_NAME (DECL) = build_string (nlen + plen, string);	\
    }									\
  while (0)

/* A C statement or statements to switch to the appropriate
   section for output of RTX in mode MODE.  RTX is some kind
   of constant in RTL.  The argument MODE is redundant except
   in the case of a `const_int' rtx.  Currently, these always
   go into the const section.  */

#undef  SELECT_RTX_SECTION
#define SELECT_RTX_SECTION(MODE, RTX) \
   const_section()

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#undef  TYPE_ASM_OP
#define TYPE_ASM_OP	"\t.type\t"
#undef  SIZE_ASM_OP
#define SIZE_ASM_OP	"\t.size\t"

/* This is how we tell the assembler that a symbol is weak.  */

#undef  ASM_WEAKEN_LABEL
#define ASM_WEAKEN_LABEL(FILE, NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* This is how we tell the assembler that two symbols have the same value.  */

#undef  ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE, NAME1, NAME2) \
  do { assemble_name(FILE, NAME1); 	 \
       fputs(" = ", FILE);		 \
       assemble_name(FILE, NAME2);	 \
       fputc('\n', FILE); } while (0)

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"@%s"

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif

/* These macros generate the special .type and .size directives which
   are used to set the corresponding fields of the linker symbol table
   entries in an ELF object file under SVR4.  These macros also output
   the starting labels for the relevant functions/objects.  */

/* Write the extra assembler code needed to declare an object properly.  */

#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do {								\
    HOST_WIDE_INT size;						\
    fprintf (FILE, "%s", TYPE_ASM_OP);				\
    assemble_name (FILE, NAME);					\
    putc (',', FILE);						\
    fprintf (FILE, TYPE_OPERAND_FMT, "object");			\
    putc ('\n', FILE);						\
    size_directive_output = 0;					\
    if (!flag_inhibit_size_directive				\
	&& DECL_SIZE (DECL)					\
	&& (size = int_size_in_bytes (TREE_TYPE (DECL))) > 0)	\
      {								\
	size_directive_output = 1;				\
	fprintf (FILE, "%s", SIZE_ASM_OP);			\
	assemble_name (FILE, NAME);				\
	fputc (',', FILE);					\
	fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, size);		\
	fputc ('\n', FILE);					\
      }								\
    ASM_OUTPUT_LABEL(FILE, NAME);				\
  } while (0)

/* Output the size directive for a decl in rest_of_decl_compilation
   in the case where we did not do so before the initializer.
   Once we find the error_mark_node, we know that the value of
   size_directive_output was set
   by ASM_DECLARE_OBJECT_NAME when it was run for the same decl.  */

#undef  ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP_LEVEL, AT_END)	\
  do {									\
    const char *name = XSTR (XEXP (DECL_RTL (DECL), 0), 0);		\
    HOST_WIDE_INT size;							\
    if (!flag_inhibit_size_directive					\
	&& DECL_SIZE (DECL)						\
	&& ! AT_END && TOP_LEVEL					\
	&& DECL_INITIAL (DECL) == error_mark_node			\
	&& !size_directive_output					\
	&& (size = int_size_in_bytes (TREE_TYPE (DECL))) > 0)		\
      {									\
	size_directive_output = 1;					\
	fprintf (FILE, "%s", SIZE_ASM_OP);				\
	assemble_name (FILE, name);					\
	fputc (',', FILE);						\
	fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, size);			\
	fputc ('\n', FILE);						\
      }									\
  } while (0)

/* A table of bytes codes used by the ASM_OUTPUT_ASCII and
   ASM_OUTPUT_LIMITED_STRING macros.  Each byte in the table
   corresponds to a particular byte value [0..255].  For any
   given byte value, if the value in the corresponding table
   position is zero, the given character can be output directly.
   If the table value is 1, the byte must be output as a \ooo
   octal escape.  If the tables value is anything else, then the
   byte value should be output as a \ followed by the value
   in the table.  Note that we can use standard UN*X escape
   sequences for many control characters, but we don't use
   \a to represent BEL because some svr4 assemblers (e.g. on
   the i386) don't know about that.  Also, we don't use \v
   since some versions of gas, such as 2.2 did not accept it.  */

#undef  ESCAPES
#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"

/* Some svr4 assemblers have a limit on the number of characters which
   can appear in the operand of a .string directive.  If your assembler
   has such a limitation, you should define STRING_LIMIT to reflect that
   limit.  Note that at least some svr4 assemblers have a limit on the
   actual number of bytes in the double-quoted string, and that they
   count each character in an escape sequence as one byte.  Thus, an
   escape sequence like \377 would count as four bytes.

   If your target assembler doesn't support the .string directive, you
   should define this to zero.  */

#undef  STRING_LIMIT
#define STRING_LIMIT	((unsigned) 256)
#undef  STRING_ASM_OP
#define STRING_ASM_OP	"\t.string\t"

/* GAS is the only Alpha/ELF assembler.  */
#undef  TARGET_GAS
#define TARGET_GAS	(1)

/* Provide a STARTFILE_SPEC appropriate for ELF.  Here we add the
   (even more) magical crtbegin.o file which provides part of the
   support for getting C++ file-scope static object constructed
   before entering `main'. 

   Don't bother seeing crtstuff.c -- there is absolutely no hope
   of getting that file to understand multiple GPs.  We provide a
   hand-coded assembly version.  */
   
#undef	STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} %{!p:crt1.o%s}}}\
   crti.o%s %{shared:crtbeginS.o%s}%{!shared:crtbegin.o%s}"

/* Provide a ENDFILE_SPEC appropriate for ELF.  Here we tack on the
   magical crtend.o file which provides part of the support for
   getting C++ file-scope static object constructed before entering
   `main', followed by a normal ELF "finalizer" file, `crtn.o'.  */

#undef	ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{shared:crtendS.o%s}%{!shared:crtend.o%s} crtn.o%s"

/* We support #pragma.  */
#define HANDLE_SYSV_PRAGMA

/* Undo the auto-alignment stuff from alpha.h.  ELF has unaligned data
   pseudos natively.  */
#undef UNALIGNED_SHORT_ASM_OP
#undef UNALIGNED_INT_ASM_OP
#undef UNALIGNED_DOUBLE_INT_ASM_OP

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   Since application size is already constrained to <2GB by the form of
   the ldgp relocation, we can use a 32-bit pc-relative relocation to
   static data.  Dynamic data is accessed indirectly to allow for read
   only EH sections.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)       \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)
