/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows NT 3.x, using a unix style C library and tools.

   This is different to the winnt.h file, since that is used
   to build GCC for use with a windows style library and tool
   set, winnt.h uses the Microsoft tools to do that.

   Copyright (C) 1995 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA. */


#define YES_UNDERSCORES

#define DBX_DEBUGGING_INFO 
#define SDB_DEBUGGING_INFO 
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "i386/gas.h"
#include "dbxcoff.h"

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif

#define CPP_PREDEFINES "-Di386 -D_WIN32 \
  -DPOSIX -D__CYGWIN32__ -DWINNT  -D_X86_=1 -D__STDC__=1\
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  -Asystem(winnt) -Acpu(i386) -Amachine(i386)"

/* We have to dynamic link to get to the system dlls,
   and I've put all of libc and libm and the unix stuff into
   cygwin.dll, the import library is called 'libcygwin.a' */

#undef LIB_SPEC
#define LIB_SPEC "-lcygwin"

/* No need for libgcc, it's in the shared library. */
#undef LIBGCC_SPEC
#define LIBGCC_SPEC ""

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!:crt0%O%s}"

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"
#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE 64
#define HAVE_ATEXIT 1

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_ctor, in_dtor

#undef EXTRA_SECTION_FUNCTIONS
#define EXTRA_SECTION_FUNCTIONS					\
  CTOR_SECTION_FUNCTION						\
  DTOR_SECTION_FUNCTION

#define CTOR_SECTION_FUNCTION					\
void								\
ctor_section ()							\
{								\
  if (in_section != in_ctor)					\
    {								\
      fprintf (asm_out_file, "\t.section .ctor\n");		\
      in_section = in_ctor;					\
    }								\
}

#define DTOR_SECTION_FUNCTION					\
void								\
dtor_section ()							\
{								\
  if (in_section != in_dtor)					\
    {								\
      fprintf (asm_out_file, "\t.section .dtor\n");		\
      in_section = in_dtor;					\
    }								\
}

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    ctor_section ();				\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);			\
    fprintf (FILE, "\n");			\
  } while (0)

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       	\
  do {						\
    dtor_section ();                   		\
    fprintf (FILE, "%s\t", ASM_LONG);		\
    assemble_name (FILE, NAME);              	\
    fprintf (FILE, "\n");			\
  } while (0)

/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On i386, if using PIC, mark a SYMBOL_REF for a non-global symbol
   so that we may access it directly in the GOT.

   On i386 running Windows NT, modify the assembler name with a suffix 
   consisting of an atsign (@) followed by string of digits that represents
   the number of bytes of arguments passed to the function, if it has the 
   attribute STDCALL. */

#ifdef ENCODE_SECTION_INFO
#undef ENCODE_SECTION_INFO
#define ENCODE_SECTION_INFO(DECL) 					\
do									\
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
#endif

/* Emit code to check the stack when allocating more that 20
   bytes in one go. */

#define CHECK_STACK_LIMIT 20



/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387 and needs stack probes */
#undef TARGET_DEFAULT

#define TARGET_DEFAULT \
   (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_STACK_PROBE) 

/* A C statement to output something to the assembler file to switch to section
   NAME for object DECL which is either a FUNCTION_DECL, a VAR_DECL or
   NULL_TREE.  Some target formats do not support arbitrary sections.  Do not
   define this macro in such cases.  */

#define ASM_OUTPUT_SECTION_NAME(FILE, DECL, NAME) \
  fprintf (FILE, "\t.section %s\n", NAME)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))
