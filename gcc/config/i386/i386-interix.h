/* Target definitions for GNU compiler for Intel 80386 running Interix
   Parts Copyright (C) 1991, 1999 Free Software Foundation, Inc.

   Parts:
     by Douglas B. Rupp (drupp@cs.washington.edu).
     by Ron Guilmette (rfg@netcom.com).
     by Donn Terry (donn@softway.com).
     by Mumit Khan (khan@xraylith.wisc.edu).

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

#define YES_UNDERSCORES

/* YES_UNDERSCORES must preceed gas.h */
#include <i386/gas.h>
/* The rest must follow. */

#define DBX_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#define HANDLE_SYSV_PRAGMA
#undef HANDLE_PRAGMA_WEAK  /* until the link format can handle it */

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387 and needs stack probes
   We also align doubles to 64-bits for MSVC default compatability */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT \
   (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_STACK_PROBE | \
    MASK_ALIGN_DOUBLE)

#undef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 2 /* 486 */

#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"

/* WinNT (and thus Interix) use unsigned int */
#define SIZE_TYPE "unsigned int"

#define ASM_LOAD_ADDR(loc, reg)   "     leal " #loc "," #reg "\n"

/* For the sake of libgcc2.c, indicate target supports atexit.  */
#define HAVE_ATEXIT

/* cpp handles __STDC__ */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES " \
  -D__INTERIX \
  -D__OPENNT \
  -D_M_IX86=300 -D_X86_=1 \
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  -Asystem(unix) -Asystem(interix) -Asystem(interix) -Acpu(i386) -Amachine(i386)"

#undef CPP_SPEC
/* Write out the correct language type definition for the header files.  
   Unless we have assembler language, write out the symbols for C.
   cpp_cpu is an Intel specific variant. See i386.h
   mieee is an Alpha specific variant.  Cross polination a bad idea.
   */
#define CPP_SPEC "\
%{!.S:	-D__LANGUAGE_C__ -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}  \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS__ -D__LANGUAGE_C_PLUS_PLUS -D__cplusplus} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C__ -D__LANGUAGE_OBJECTIVE_C} \
-remap \
%(cpp_cpu) \
%{posix:-D_POSIX_SOURCE} \
-idirafter %$INTERIX_ROOT/usr/include"

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i386 Interix)");

/* The global __fltused is necessary to cause the printf/scanf routines
   for outputting/inputting floating point numbers to be loaded.  Since this
   is kind of hard to detect, we just do it all the time. */

#ifdef ASM_FILE_START
#undef ASM_FILE_START
#endif
#define ASM_FILE_START(FILE) \
  do {  fprintf (FILE, "\t.file\t");                            \
        output_quoted_string (FILE, dump_base_name);            \
        fprintf (FILE, "\n");                                   \
        fprintf (FILE, ".global\t__fltused\n");                 \
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
   should define this to zero.
*/

#define STRING_LIMIT	((unsigned) 256)

#define STRING_ASM_OP	".string"

/* The routine used to output NUL terminated strings.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable, especially for targets like the i386
   (where the only alternative is to output character sequences as
   comma separated lists of numbers).   */

#define ASM_OUTPUT_LIMITED_STRING(FILE, STR)				\
  do									\
    {									\
      register unsigned char *_limited_str = (unsigned char *) (STR);	\
      register unsigned ch;						\
      fprintf ((FILE), "\t%s\t\"", STRING_ASM_OP);			\
      for (; (ch = *_limited_str); _limited_str++)			\
        {								\
	  register int escape = ESCAPES[ch];				\
	  switch (escape)						\
	    {								\
	    case 0:							\
	      putc (ch, (FILE));					\
	      break;							\
	    case 1:							\
	      fprintf ((FILE), "\\%03o", ch);				\
	      break;							\
	    default:							\
	      putc ('\\', (FILE));					\
	      putc (escape, (FILE));					\
	      break;							\
	    }								\
        }								\
      fprintf ((FILE), "\"\n");						\
    }									\
  while (0)

/* The routine used to output sequences of byte values.  We use a special
   version of this for most svr4 targets because doing so makes the
   generated assembly code more compact (and thus faster to assemble)
   as well as more readable.  Note that if we find subparts of the
   character sequence which end with NUL (and which are shorter than
   STRING_LIMIT) we output those using ASM_OUTPUT_LIMITED_STRING.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(FILE, STR, LENGTH)				\
  do									\
    {									\
      register unsigned char *_ascii_bytes = (unsigned char *) (STR);	\
      register unsigned char *limit = _ascii_bytes + (LENGTH);		\
      register unsigned bytes_in_chunk = 0;				\
      for (; _ascii_bytes < limit; _ascii_bytes++)			\
        {								\
	  register unsigned char *p;					\
	  if (bytes_in_chunk >= 64)					\
	    {								\
	      fputc ('\n', (FILE));					\
	      bytes_in_chunk = 0;					\
	    }								\
	  for (p = _ascii_bytes; p < limit && *p != '\0'; p++)		\
	    continue;							\
	  if (p < limit && (p - _ascii_bytes) <= STRING_LIMIT)		\
	    {								\
	      if (bytes_in_chunk > 0)					\
		{							\
		  fputc ('\n', (FILE));					\
		  bytes_in_chunk = 0;					\
		}							\
	      ASM_OUTPUT_LIMITED_STRING ((FILE), _ascii_bytes);		\
	      _ascii_bytes = p;						\
	    }								\
	  else								\
	    {								\
	      if (bytes_in_chunk == 0)					\
		fprintf ((FILE), "\t.byte\t");				\
	      else							\
		fputc (',', (FILE));					\
	      fprintf ((FILE), "0x%02x", *_ascii_bytes);		\
	      bytes_in_chunk += 5;					\
	    }								\
	}								\
      if (bytes_in_chunk > 0)						\
        fprintf ((FILE), "\n");						\
    }									\
  while (0)

/* This is how to output an element of a case-vector that is relative.
   This is only used for PIC code.  See comments by the `casesi' insn in
   i386.md for an explanation of the expression this outputs.
   PE format differs on what PC-relative offsets look like (see
   coff_i386_rtype_to_howto), and we need to compensate (by one word) here. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) \
  fprintf (FILE, "\t.long __GLOBAL_OFFSET_TABLE_+[.-%s%d+4]\n", LPREFIX, VALUE)

/* Indicate that jump tables go in the text section.  This is
   necessary when compiling PIC code.  */

#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Emit code to check the stack when allocating more that 4000
   bytes in one go. */

#define CHECK_STACK_LIMIT 0x1000

/* the following are OSF linker (not gld) specific... we don't want them */
#undef HAS_INIT_SECTION
#undef LD_INIT_SWITCH
#undef LD_FINI_SWITCH


/* The following are needed for C++, but also needed for profiling */

/* Support const sections and the ctors and dtors sections for g++.
   Note that there appears to be two different ways to support const
   sections at the moment.  You can either #define the symbol
   READONLY_DATA_SECTION (giving it some code which switches to the
   readonly data section) or else you can #define the symbols
   EXTRA_SECTIONS, EXTRA_SECTION_FUNCTIONS, SELECT_SECTION, and
   SELECT_RTX_SECTION.  We do both here just to be on the safe side.  */

#define USE_CONST_SECTION	1

#define CONST_SECTION_ASM_OP	".section\t.rdata,\"r\""

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

#define CTORS_SECTION_ASM_OP	".section\t.ctors,\"x\""
#define DTORS_SECTION_ASM_OP	".section\t.dtors,\"x\""

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

#undef READONLY_DATA_SECTION
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

#if 0
/* Currently gas chokes on this; that's not too hard to fix, but there's
   not a lot of impeteus to do it, either.  If it is done, gas will have
   to handle long section name escapes (which are defined in the COFF/PE
   document as /nnn where nnn is a string table index).  The benefit:
   section attributes and -ffunction-sections, neither of which seem to
   be critical. */
/* gas may have been fixed? bfd was. */

/* Switch into a generic section.
   This is currently only used to support section attributes.

   We make the section read-only and executable for a function decl,
   read-only for a const data decl, and writable for a non-const data decl.  */
#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME) \
  fprintf (FILE, ".section\t%s,\"%s\",@progbits\n", NAME, \
	   (DECL) && TREE_CODE (DECL) == FUNCTION_DECL ? "ax" : \
	   (DECL) && TREE_READONLY (DECL) ? "a" : "aw")
#endif

#define INT_ASM_OP		".long"

/* The MS compilers take alignment as a number of bytes, so we do as well */
#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  if ((LOG)!=0) fprintf ((FILE), "\t.balign %d\n", 1<<(LOG))

/* A C statement (sans semicolon) to output an element in the table of
   global constructors.  */
#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    ctors_section ();							\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);						\
    fprintf (FILE, "\n");						\
  } while (0)

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    dtors_section ();                   				\
    fprintf (FILE, "\t%s\t ", INT_ASM_OP);				\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, "\n");						\
  } while (0)

/* The linker will take care of this, and having them causes problems with
   ld -r (specifically -rU). */
#define CTOR_LISTS_DEFINED_EXTERNALLY 1

#define SET_ASM_OP	".set"
/* Output a definition (implements alias) */
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
do									\
{									\
    fprintf ((FILE), "\t%s\t", SET_ASM_OP);				\
    assemble_name (FILE, LABEL1);					\
    fprintf (FILE, ",");						\
    assemble_name (FILE, LABEL2);					\
    fprintf (FILE, "\n");						\
    }									\
while (0)

#define HOST_PTR_PRINTF "%p"
#define HOST_PTR_AS_INT unsigned long

#define PCC_BITFIELD_TYPE_MATTERS 1
#define PCC_BITFIELD_TYPE_TEST TYPE_NATIVE(rec)
#define GROUP_BITFIELDS_BY_ALIGN TYPE_NATIVE(rec)

/* The following two flags are usually "off" for i386, because some non-gnu
   tools (for the i386) don't handle them.  However, we don't have that
   problem, so.... */

/* Forward references to tags are allowed.  */
#define SDB_ALLOW_FORWARD_REFERENCES

/* Unknown tags are also allowed.  */
#define SDB_ALLOW_UNKNOWN_REFERENCES

/* The integer half of this list needs to be constant.  However, there's
   a lot of disagreement about what the floating point adjustments should
   be.  We pick one that works with gdb.  (The underlying problem is
   what to do about the segment registers.  Since we have access to them
   from /proc, we'll allow them to be accessed in gdb, even tho the
   gcc compiler can't generate them.  (There's some evidence that 
   MSVC does, but possibly only for certain special "canned" sequences.) */

#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(n) \
((n) == 0 ? 0 \
 : (n) == 1 ? 2 \
 : (n) == 2 ? 1 \
 : (n) == 3 ? 3 \
 : (n) == 4 ? 6 \
 : (n) == 5 ? 7 \
 : (n) == 6 ? 5 \
 : (n) == 7 ? 4 \
 : ((n) >= FIRST_STACK_REG && (n) <= LAST_STACK_REG) ? (n)+8 \
 : (-1))

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   Apply stddef, handle (as yet unimplemented) pic.

   stddef renaming does NOT apply to Alpha. */

char *gen_stdcall_suffix ();

#undef ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL) 					\
do 									\
  {									\
    if (flag_pic)							\
      {									\
	rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'		\
		   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));		\
	SYMBOL_REF_FLAG (XEXP (rtl, 0))					\
	  = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'			\
	     || ! TREE_PUBLIC (DECL));					\
      }									\
    if (TREE_CODE (DECL) == FUNCTION_DECL) 				\
      if (lookup_attribute ("stdcall",					\
			    TYPE_ATTRIBUTES (TREE_TYPE (DECL))))	\
        XEXP (DECL_RTL (DECL), 0) = 					\
          gen_rtx (SYMBOL_REF, Pmode, gen_stdcall_suffix (DECL)); 	\
  }									\
while (0)

/* This macro gets just the user-specified name
   out of the string in a SYMBOL_REF.  Discard
   trailing @[NUM] encoded by ENCODE_SECTION_INFO.  */
#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)				\
do {									\
  char *_p;								\
  char *_name = SYMBOL_NAME;						\
  for (_p = _name; *_p && *_p != '@'; ++_p)				\
    ;									\
  if (*_p == '@')							\
    {									\
      int _len = _p - _name;						\
      (VAR) = (char *) alloca (_len + 1);				\
      strncpy ((VAR), _name, _len);					\
      (VAR)[_len] = '\0';						\
    }									\
  else									\
    (VAR) = _name;							\
} while (0)
      
#if 0	
/* Turn this back on when the linker is updated to handle grouped
   .data$ sections correctly. See corresponding note in i386/interix.c. 
   MK. */

/* Define this macro if in some cases global symbols from one translation
   unit may not be bound to undefined symbols in another translation unit
   without user intervention.  For instance, under Microsoft Windows
   symbols must be explicitly imported from shared libraries (DLLs).  */
#define MULTIPLE_SYMBOL_SPACES

#define UNIQUE_SECTION_P(DECL) DECL_ONE_ONLY (DECL)
extern void i386_pe_unique_section ();
#define UNIQUE_SECTION(DECL,RELOC) i386_pe_unique_section (DECL, RELOC)

#define SUPPORTS_ONE_ONLY 1

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */
#undef ASM_OUTPUT_SECTION_NAME
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC)		\
do {									\
  static struct section_info						\
    {									\
      struct section_info *next;					\
      char *name;							\
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
    type = SECT_EXEC, mode = "x";					\
  else if (DECL && DECL_READONLY_SECTION (DECL, RELOC))			\
    type = SECT_RO, mode = "r";						\
  else									\
    type = SECT_RW, mode = "w";						\
									\
  if (s == 0)								\
    {									\
      s = (struct section_info *) xmalloc (sizeof (struct section_info)); \
      s->name = xmalloc ((strlen (NAME) + 1) * sizeof (*NAME));		\
      strcpy (s->name, NAME);						\
      s->type = type;							\
      s->next = sections;						\
      sections = s;							\
      fprintf (STREAM, ".section\t%s,\"%s\"\n", NAME, mode);		\
      /* Functions may have been compiled at various levels of		\
         optimization so we can't use `same_size' here.  Instead,	\
         have the linker pick one.  */					\
      if ((DECL) && DECL_ONE_ONLY (DECL))				\
        fprintf (STREAM, "\t.linkonce %s\n",				\
	         TREE_CODE (DECL) == FUNCTION_DECL			\
	         ? "discard" : "same_size");				\
    }									\
  else									\
    {									\
      fprintf (STREAM, ".section\t%s,\"%s\"\n", NAME, mode);		\
    }									\
} while (0)

#endif /* 0 */

/* DWARF2 Unwinding doesn't work with exception handling yet. */
#define DWARF2_UNWIND_INFO 0

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C

