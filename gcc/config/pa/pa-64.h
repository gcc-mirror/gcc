/* Definitions of target machine for GNU compiler, for HPs using the
   64bit runtime model.
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
#define NEW_HP_ASSEMBLER

/* The default sizes for basic datatypes provided by GCC are not
   correct for the PA64 runtime architecture.

   In PA64, basic types have the following sizes

     char	1 byte
     short	2 bytes
     int	4 bytes
     long	8 bytes
     long long	8 bytes
     pointer	8 bytes
     float	4 bytes
     double	8 bytes
     long double 16 bytes
     size_t	8 bytes
     ptrdiff_t	8 bytes
     wchar	4 bytes
     
  Make GCC agree with types.h.  */
#undef SIZE_TYPE
#undef PTRDIFF_TYPE

#define SIZE_TYPE "long unsigned int"
#define PTRDIFF_TYPE "long int"

/* If it is not listed here, then the default selected by GCC is OK.  */
#define SHORT_TYPE_SIZE 16
#define INT_TYPE_SIZE 32
#define MAX_LONG_TYPE_SIZE 64
#define LONG_TYPE_SIZE 64
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
/* This should be 128, but until we work out the ABI for the 128bit
   FP codes supplied by HP we'll keep it at 64 bits.  */
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#define MAX_WCHAR_TYPE_SIZE 32

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
do {  \
     if (TARGET_64BIT) \
       fputs("\t.LEVEL 2.0w\n", FILE); \
     else if (TARGET_PA_11) \
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

/* Temporary until we figure out what to do with those *(&@$ 32bit
   relocs which appear in stabs.  */
#undef DBX_DEBUGGING_INFO

/* We want the compiler to select a suitable secondary memory location.
   ?!? This may not work reliably.  Keep an eye out for problems.  */
#undef SECONDARY_MEMORY_NEEDED_RTX


/* ?!? This needs to be made compile-time selectable.

   The PA64 runtime model has arguments that grow to higher addresses
   (like most other targets).  The older runtime model has arguments
   that grow to lower addresses.  What fun.  */
#undef ARGS_GROW_DOWNWARD
#undef ARG_POINTER_REGNUM
#define ARG_POINTER_REGNUM 29
#undef STATIC_CHAIN_REGNUM
#define STATIC_CHAIN_REGNUM 31

/* This is not needed for correct operation in 32bit mode, and since
   older versions of gas and the hpux assembler do not accept .dword
   we put this here instead of the more logical location, pa.h.  */
#define ASM_OUTPUT_DOUBLE_INT(FILE,VALUE)  \
{ fputs ("\t.dword ", FILE);                    \
  if (function_label_operand (VALUE, VOIDmode)) \
    fputs ("P%", FILE);                         \
  output_addr_const (FILE, (VALUE));            \
  fputs ("\n", FILE);}

/* It looks like DWARF2 will be the easiest debug format to handle on this
   platform.  */
#define OBJECT_FORMAT_ELF
#define DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_FORMAT DWARF2_DEBUG
/* This isn't quite ready yet.  I'm seeing it mess up some line
   tables.  For example, we're getting lines starting/ending at
   impossible addresses.  */
#define DWARF2_ASM_LINE_DEBUG_INFO 1


/* Nonzero if we do not know how to pass TYPE solely in registers. */
#define MUST_PASS_IN_STACK(MODE,TYPE)                   \
  ((TYPE) != 0                                          \
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST      \
       || TREE_ADDRESSABLE (TYPE)))

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

extern void text_section ();

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
do {									\
  static struct section_info						\
    {									\
      struct section_info *next;				        \
      char *name;						        \
      enum sect_enum {SECT_RW, SECT_RO, SECT_EXEC} type;		\
    } *sections;							\
  struct section_info *s;						\
  char *mode;								\
  enum sect_enum type;							\
									\
  for (s = sections; s; s = s->next)					\
    if (!strcmp (NAME, s->name))					\
      break;								\
									\
  if (DECL && TREE_CODE (DECL) == FUNCTION_DECL)			\
    type = SECT_EXEC, mode = "ax";					\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))			\
    type = SECT_RO, mode = "a";						\
  else									\
    type = SECT_RW, mode = "aw";					\
									\
  if (s == 0)								\
    {									\
      s = (struct section_info *) xmalloc (sizeof (struct section_info));  \
      s->name = xmalloc ((strlen (NAME) + 1) * sizeof (*NAME));		\
      strcpy (s->name, NAME);						\
      s->type = type;							\
      s->next = sections;						\
      sections = s;							\
      fprintf (FILE, "\t.section\t%s,\"%s\",@progbits\n", NAME, mode);	\
    }									\
  else									\
    {									\
      if (DECL && s->type != type)					\
	error_with_decl (DECL, "%s causes a section type conflict");	\
									\
      fprintf (FILE, "\t.section\t%s\n", NAME);				\
    }									\
} while (0)

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

#define INT_ASM_OP ".dword"
/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t P%%", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t P%%", INT_ASM_OP);				\
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

#define TYPE_ASM_OP	".type"
#define SIZE_ASM_OP	".size"

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
