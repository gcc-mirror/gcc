/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows NT 3.x, using a Unix style C library and tools,
   as distinct from winnt.h, which is used to build GCC for use with a
   windows style library and tool set and uses the Microsoft tools.
   Copyright (C) 1995, 1996, 1997, 1998 Free Software Foundation, Inc.

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

#undef CPP_SPEC
#define CPP_SPEC "-remap %(cpp_cpu) %[cpp_cpu] %{posix:-D_POSIX_SOURCE}"

/* We have to dynamic link to get to the system DLLs.  All of libc, libm and
   the Unix stuff is in cygwin.dll.  The import library is called
   'libcygwin.a'.  For Windows applications, include more libraries, but
   always include kernel32.  We'd like to specific subsystem windows to
   ld, but that doesn't work just yet.  */

#undef LIB_SPEC
#define LIB_SPEC "-lcygwin %{mwindows:-luser32 -lgdi32 -lcomdlg32} -lkernel32 \
  -ladvapi32 -lshell32"

#define LINK_SPEC "%{mwindows:--subsystem windows}"

/* Normally, -lgcc is not needed since everything in it is in the DLL, but we
   want to allow things to be added to it when installing new versions of
   GCC without making a new CYGWIN.DLL, so we leave it.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s"

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_UNSIGNED 1
#define WCHAR_TYPE_SIZE 16
#define WCHAR_TYPE "short unsigned int"
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

/* This macro gets just the user-specified name out of the string in a
   SYMBOL_REF.  Discard trailing @[NUM] encoded by ENCODE_SECTION_INFO.   */

#undef  STRIP_NAME_ENCODING
#define STRIP_NAME_ENCODING(VAR,SYMBOL_NAME)				\
do {									\
  char *_p;								\
  char *_name = ((SYMBOL_NAME) + ((SYMBOL_NAME)[0] == '*'));		\
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
      
/* Emit code to check the stack when allocating more that 4000
   bytes in one go. */

#define CHECK_STACK_LIMIT 4000

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   and returns float values in the 387 and needs stack probes */
#undef TARGET_DEFAULT

#define TARGET_DEFAULT \
   (MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS | MASK_STACK_PROBE) 

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", 1<<(LOG))

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
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC)	\
do {								\
  if ((DECL) && TREE_CODE (DECL) == FUNCTION_DECL)		\
    fprintf (STREAM, "\t.section %s,\"x\"\n", (NAME));		\
  else if ((DECL) && DECL_READONLY_SECTION (DECL, RELOC))	\
    fprintf (STREAM, "\t.section %s,\"\"\n", (NAME));		\
  else								\
    fprintf (STREAM, "\t.section %s,\"w\"\n", (NAME));		\
  /* Functions may have been compiled at various levels of	\
     optimization so we can't use `same_size' here.  Instead,	\
     have the linker pick one.  */				\
  if ((DECL) && DECL_ONE_ONLY (DECL))				\
    fprintf (STREAM, "\t.linkonce %s\n",			\
	     TREE_CODE (DECL) == FUNCTION_DECL			\
	     ? "discard" : "same_size");			\
} while (0)

#undef ASM_COMMENT_START
#define ASM_COMMENT_START " #"

/* Don't assume anything about the header files. */
#define NO_IMPLICIT_EXTERN_C
