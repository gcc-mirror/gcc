/* Operating system specific defines to be used when targeting GCC for
   Windows NT 3.x on an i386.
   Copyright (C) 1994, 1995 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (drupp@cs.washington.edu).

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#define YES_UNDERSCORES

#include "i386/gas.h"

/* Value is the number of bytes of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   This only happens if the function declaration has the STDCALL attribute and
   the number of arguments is not variable */

#undef RETURN_POPS_ARGS
#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  ( \
   TREE_CODE (FUNTYPE) == IDENTIFIER_NODE \
   ? \
     0 \
   : \
     ( \
      ((FUNDECL && (TREE_CODE_CLASS (TREE_CODE (FUNDECL)) == 'd') \
	? \
          lookup_attribute ("stdcall", \
			    DECL_MACHINE_ATTRIBUTES (FUNDECL)) != NULL_TREE \
	: 0 \
       ) \
      ) \
      && \
         ( \
          TYPE_ARG_TYPES (FUNTYPE) == 0 \
          || \
            TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE))) \
            == void_type_node \
         ) \
     ) \
     ? \
       (SIZE) \
     : \
       (aggregate_value_p (TREE_TYPE (FUNTYPE))) \
       ? \
         GET_MODE_SIZE (Pmode) \
       : \
         0 \
  )

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dunix -Di386 -DWIN32 -D_WIN32 \
  -DWINNT -D_M_IX86=300 -D_X86_=1 -D__STDC__=0 -DALMOST_STDC -D_MSC_VER=800 \
  -D__stdcall=__attribute__((__stdcall__)) \
  -D__cdecl=__attribute__((__cdecl__)) \
  -Asystem(unix) -Asystem(winnt) -Acpu(i386) -Amachine(i386)"

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
			    DECL_MACHINE_ATTRIBUTES (DECL)))		\
        XEXP (DECL_RTL (DECL), 0) = 					\
          gen_rtx (SYMBOL_REF, Pmode, gen_stdcall_suffix (DECL)); 	\
  }									\
while (0)
#endif

/* Value is 1 if the declaration has either of the attributes: CDECL or
   STDCALL and 0 otherwise */

#define VALID_MACHINE_DECL_ATTRIBUTE(DECL, ATTR, NAME, ARGS) \
  (((TREE_CODE(DECL) == FUNCTION_DECL) \
    || (TREE_CODE(DECL) == FIELD_DECL) \
    || (TREE_CODE(DECL) == TYPE_DECL)) \
   && (is_attribute_p ("stdcall", (NAME)) \
       || is_attribute_p ("cdecl", (NAME))) \
   && (ARGS) == NULL)

/* The global __fltused is necessary to cause the printf/scanf routines
   for outputting/inputting floating point numbers to be loaded.  Since this
   is kind of hard to detect, we just do it all the time. */

#ifdef ASM_FILE_START
#undef ASM_FILE_START
#endif
#define ASM_FILE_START(FILE) \
  do {	fprintf (FILE, "\t.file\t");				\
	output_quoted_string (FILE, dump_base_name);		\
	fprintf (FILE, "\n");					\
        fprintf (FILE, ".global\t__fltused\n");			\
  } while (0)

/* if the switch "-mwindows" is passed to ld, then specify to the Microsoft
   linker the proper switches and libraries to build a graphical program */

#undef LIB_SPEC
#define LIB_SPEC "%{mwindows:-subsystem:windows -entry:WinMainCRTStartup \
  USER32.LIB GDI32.LIB COMDLG32.LIB WINSPOOL.LIB} \
 %{!mwindows:-subsystem:console -entry:mainCRTStartup} \
 %{mcrtmt:OLDNAMES.LIB LIBCMT.LIB KERNEL32.LIB ADVAPI32.LIB} \
 %{!mcrtmt:OLDNAMES.LIB LIBC.LIB KERNEL32.LIB ADVAPI32.LIB} \
 %{g:-debugtype:coff -debug:full} \
 %{v}"

#include "winnt/winnt.h"

