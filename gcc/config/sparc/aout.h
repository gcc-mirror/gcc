/* Definitions of target machine for GNU compiler, for SPARC using a.out.
   Copyright (C) 1994, 1996 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com).

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

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (sparc)");

/* Names to predefine in the preprocessor for this target machine.
   ??? It would be nice to not include any subtarget specific values here,
   however there's no way to portably provide subtarget values to
   CPP_PREFINES.  Also, -D values in CPP_SUBTARGET_SPEC don't get turned into
   foo, __foo and __foo__.  */

#define CPP_PREDEFINES "-Dsparc -Acpu=sparc -Amachine=sparc"

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)				\
 (DEFAULT_WORD_SWITCH_TAKES_ARG (STR)				\
  || !strcmp (STR, "target") || !strcmp (STR, "assert"))

/* This is defined differently for v9 in a cover file.  */
#define SELECT_SECTION(T,RELOC,ALIGN)					\
{									\
  if (TREE_CODE (T) == VAR_DECL)					\
    {									\
      if (TREE_READONLY (T) && ! TREE_SIDE_EFFECTS (T)			\
	  && DECL_INITIAL (T)						\
	  && (DECL_INITIAL (T) == error_mark_node			\
	      || TREE_CONSTANT (DECL_INITIAL (T)))			\
	  && DECL_ALIGN (T) <= MAX_TEXT_ALIGN				\
	  && ! (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES)))	\
	text_section ();						\
      else								\
	data_section ();						\
    }									\
  else if (TREE_CODE (T) == CONSTRUCTOR)				\
    {									\
      if (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES))		\
	data_section ();						\
    }									\
  else if (TREE_CODE_CLASS (TREE_CODE (T)) == 'c')			\
    {									\
      if ((TREE_CODE (T) == STRING_CST && flag_writable_strings)	\
	  || TYPE_ALIGN (TREE_TYPE (T)) > MAX_TEXT_ALIGN		\
	  || (flag_pic && ((RELOC) || SUNOS4_SHARED_LIBRARIES)))	\
	data_section ();						\
      else								\
	text_section ();						\
    }									\
}

/* Output the label for a function definition.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
do {									\
  ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));			\
  ASM_OUTPUT_LABEL (FILE, NAME);					\
} while (0)

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\t.data"

/* How to renumber registers for dbx and gdb.  In the flat model, the frame
   pointer is really %i7.  */

#define DBX_REGISTER_NUMBER(REGNO) \
  (TARGET_FLAT && REGNO == FRAME_POINTER_REGNUM ? 31 : REGNO)

/* This is how to output a note to DBX telling it the line number
   to which the following sequence of instructions corresponds.

   This is needed for SunOS 4.0, and should not hurt for 3.2
   versions either.  */
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,LM%d\nLM%d:\n",	\
	     line, sym_lineno, sym_lineno);		\
    sym_lineno += 1; }
