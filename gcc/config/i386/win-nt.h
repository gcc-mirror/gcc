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
      ((FUNDECL && (TREE_CODE_CLASS (TREE_CODE (FUNDECL)) == 'd') ? \
        chain_member_value (get_identifier ("stdcall"), \
                            DECL_MACHINE_ATTRIBUTES (FUNDECL) \
                           )                                      : 0 \
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

#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(FILE,SIZE) \
  winnt_function_prologue (FILE, SIZE)


#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dunix -Di386 -DWIN32 -D_WIN32 \
  -DWINNT -D_M_IX86=300 -D_X86_=1 -DALMOST_STDC -D_MSC_VER=800\
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

#define HAVE_probe 1
#define gen_probe() gen_rtx(ASM_INPUT, VOIDmode, "call __stkchk\n")

#include "winnt/winnt.h"

