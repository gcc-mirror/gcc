/* Definitions of target machine for GNU compiler, for HPs running
   HPUX using the 64bit runtime model.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

/* We can debug dynamically linked executables on hpux11; we also
   want dereferencing of a NULL pointer to cause a SEGV.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "-E %{mlinker-opt:-O} %{!shared:-u main} %{static:-a archive} %{shared:-shared}"

/* Like the default, except no -lg.  */
#undef LIB_SPEC
#define LIB_SPEC \
  "%{!shared:\
     %{!p:\
       %{!pg:\
         %{!threads:-lc}\
         %{threads:-lcma -lc_r}}\
       %{p: -L/lib/libp/ -lc}\
       %{pg: -L/lib/libp/ -lc}}} /usr/lib/pa20_64/milli.a"

/* Under hpux11, the normal location of the `ld' and `as' programs is the
   /usr/ccs/bin directory.  */

#ifndef CROSS_COMPILE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/opt/langtools/bin"
#endif

/* Under hpux11 the normal location of the various *crt*.o files is the
   /usr/ccs/lib directory.  */

#ifndef CROSS_COMPILE
#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/opt/langtools/lib/pa20_64/"
#endif

/* hpux11 has the new HP assembler.  It's still lousy, but it's a whole lot
   better than the assembler shipped with older versions of hpux.  */
#undef NEW_HP_ASSEMBLER
#define NEW_HP_ASSEMBLER 1

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
do {  \
     if (TARGET_64BIT) \
       fputs("\t.LEVEL 2.0w\n", FILE); \
     else if (TARGET_PA_20) \
       fputs("\t.LEVEL 2.0\n", FILE); \
     else if (TARGET_PA_11) \
       fputs("\t.LEVEL 1.1\n", FILE); \
     else \
       fputs("\t.LEVEL 1.0\n", FILE); \
     if (profile_flag)\
       fprintf (FILE, "\t.IMPORT _mcount, CODE\n");\
     if (write_symbols != NO_DEBUG) \
       output_file_directive ((FILE), main_input_filename); \
   } while (0)

/* It looks like DWARF2 will be the easiest debug format to handle on this
   platform.  */
#define OBJECT_FORMAT_ELF
#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
/* This isn't quite ready yet.  I'm seeing it mess up some line
   tables.  For example, we're getting lines starting/ending at
   impossible addresses.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1


/* The rest of this file is copied from the generic svr4.h.  One day we
   would like to simply include svr4.h instead of copying all these
   definitions.  */

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#define USE_CONST_SECTION	1

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

#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"aw\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"aw\""

/* On svr4, we *do* have support for the .init and .fini sections, and we
   can put stuff in there to be executed before and after `main'.  We let
   crtstuff.c and other files know this by defining the following symbols.
   The definitions say how to change sections to the .init and .fini
   sections.  This is the same for all known svr4 assemblers.  */

/* ??? For the time being, we aren't using init sections. */
#if 0
#define INIT_SECTION_ASM_OP	"\t.section\t.init"
#define FINI_SECTION_ASM_OP	"\t.section\t.fini"
#endif

/* A default list of other sections which we might be "in" at any given
   time.  For targets that use additional sections (e.g. .tdesc) you
   should override this definition in the target-specific file which
   includes this file.  */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_const, in_ctors, in_dtors

/* A default list of extra section function definitions.  For targets
   that use additional sections (e.g. .tdesc) you should override this
   definition in the target-specific file which includes this file.  */

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS						\
  CONST_SECTION_FUNCTION						\
  CTORS_SECTION_FUNCTION						\
  DTORS_SECTION_FUNCTION

#define READONLY_DATA_SECTION() const_section ()

#define CONST_SECTION_FUNCTION						\
void									\
const_section ()							\
{									\
  if (!USE_CONST_SECTION)						\
    text_section();							\
  else if (in_section != in_const)					\
    {									\
      fprintf (asm_out_file, "%s\n", CONST_SECTION_ASM_OP);		\
      in_section = in_const;						\
    }									\
}

#define CTORS_SECTION_FUNCTION						\
void									\
ctors_section ()							\
{									\
  if (in_section != in_ctors)						\
    {									\
      fprintf (asm_out_file, "%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctors;						\
    }									\
}

#define DTORS_SECTION_FUNCTION						\
void									\
dtors_section ()							\
{									\
  if (in_section != in_dtors)						\
    {									\
      fprintf (asm_out_file, "%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtors;						\
    }									\
}

/* Switch into a generic section.
 
   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.
 
   If the section has already been defined, we must not
   emit the attributes here. The SVR4 assembler does not
   recognize section redefinitions.
   If DECL is NULL, no attributes are emitted.  */

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

#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)
#define UNIQUE_SECTION_P(DECL) (DECL_ONE_ONLY (DECL))
#define UNIQUE_SECTION(DECL,RELOC)				\
do {								\
  int len;							\
  char *name, *string, *prefix;					\
								\
  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (DECL));	\
								\
  if (! DECL_ONE_ONLY (DECL))					\
    {								\
      prefix = ".";                                             \
      if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	prefix = ".text.";					\
      else if (DECL_READONLY_SECTION (DECL, RELOC))		\
	prefix = ".rodata.";					\
      else							\
	prefix = ".data.";					\
    }								\
  else if (TREE_CODE (DECL) == FUNCTION_DECL)			\
    prefix = ".gnu.linkonce.t.";				\
  else if (DECL_READONLY_SECTION (DECL, RELOC))			\
    prefix = ".gnu.linkonce.r.";				\
  else								\
    prefix = ".gnu.linkonce.d.";				\
								\
  len = strlen (name) + strlen (prefix);			\
  string = alloca (len + 1);					\
  sprintf (string, "%s%s", prefix, name);			\
								\
  DECL_SECTION_NAME (DECL) = build_string (len, string);	\
} while (0)

#define INT_ASM_OP "\t.dword\t"
/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "%sP%%", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "%sP%%", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* ??? For the time being, we aren't using .ctors/.dtors sections. */
#undef ASM_OUTPUT_DESTRUCTOR
#undef ASM_OUTPUT_CONSTRUCTOR

/* Define the strings used for the special svr4 .type and .size directives.
   These strings generally do not vary from one system running svr4 to
   another, but if a given system (e.g. m88k running svr) needs to use
   different pseudo-op names for these, they may be overridden in the
   file which includes this one.  */

#define TYPE_ASM_OP	"\t.type\t"
#define SIZE_ASM_OP	"\t.size\t"

/* This is how we tell the assembler that a symbol is weak.  */

#define ASM_WEAKEN_LABEL(FILE,NAME) \
  do { fputs ("\t.weak\t", FILE); assemble_name (FILE, NAME); \
       fputc ('\n', FILE); } while (0)

/* The following macro defines the format used to output the second
   operand of the .type assembler directive.  Different svr4 assemblers
   expect various different forms for this operand.  The one given here
   is just a default.  You may need to override it in your machine-
   specific tm.h file (depending upon the particulars of your assembler).  */

#define TYPE_OPERAND_FMT	"@%s"

/* Write the extra assembler code needed to declare a function's result.
   Most svr4 assemblers don't require any special declaration of the
   result value, but there are exceptions.  */

#ifndef ASM_DECLARE_RESULT
#define ASM_DECLARE_RESULT(FILE, RESULT)
#endif
