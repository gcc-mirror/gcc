/* Definitions of target machine for GNU compiler.  Clipper/Clix version.
   Copyright (C) 1988, 1993 Free Software Foundation, Inc.

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

#include "clipper/clipper.h"

#include "svr3.h"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dclipper -Dunix -Asystem(unix) -Asystem(svr3) -Acpu(clipper) -Amachine(clipper)"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef LIB_SPEC

#undef HAVE_ATEXIT
#define HAVE_ATEXIT

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)			\
do {							\
  unsigned char *s;					\
  int i;						\
  for (i = 0, s = (unsigned char *)(PTR); i < (LEN); s++, i++)	\
    {							\
      if ((i % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));			\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
} while (0)

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)	\
{					\
  union { int i[2]; double d; } _d_;	\
  _d_.d = VALUE;				\
  fprintf (FILE, "\t.long 0x%08x,0x%08x\n", _d_.i[0],_d_.i[1]); \
}

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)	\
{					\
  union { int i; float f; } _f_;	\
  _f_.f = VALUE;				\
  fprintf (FILE, "\t.long 0x%08x\n", _f_.i); \
}

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  fprintf(FILE, "\t.align %d\n", 1 << (LOG))


#define ASM_LONG ".long"
#define BSS_SECTION_ASM_OP  ".bss"
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP ".section .init,\"x\""


/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   ASM_OUTPUT_CONSTRUCTOR should be defined to push the address of the
   constructor.  */

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("subq   $8,sp");				\
  asm ("loadq  $0,r0");				\
  asm ("storw  r0,(sp)")

/* don't need end marker */

#undef CTOR_LIST_END

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)	\
  do {						\
    init_section ();				\
    fputs ("\tloada  ", FILE);			\
    assemble_name (FILE, NAME);			\
    fputs (",r0\n\tsubq   $8,sp\n\tstorw   r0,(sp)\n", FILE);	\
  } while (0)


/* fini psect is 8 aligned */

#define DTOR_LIST_BEGIN	\
  asm (DTORS_SECTION_ASM_OP);				\
  func_ptr __DTOR_LIST__[2] = { (func_ptr) (-1), 0 };

/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */

#undef ASM_OUTPUT_DESTRUCTOR
#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    fini_section ();                   					\
    fprintf (FILE, "%s\t ", ASM_LONG);					\
    assemble_name (FILE, NAME);              				\
    fprintf (FILE, ",0\n");						\
  } while (0)


/* On clix crt1.o first calls init code and then sets environ and a valid
   chrclass. Unfortunately stdio routines bomb with unset chrclass.
   Therefore we set chrclass prior to calling global constructors. */

#undef DO_GLOBAL_CTORS_BODY
#define DO_GLOBAL_CTORS_BODY					\
do {								\
  func_ptr *p, *beg = alloca (0);				\
  _setchrclass (0);						\
  for (p = beg; *p; p+=2)					\
    ;								\
  while (p != beg)						\
    { p-= 2; (*p) (); }						\
} while (0)


#undef DO_GLOBAL_DTORS_BODY
#define DO_GLOBAL_DTORS_BODY	\
  func_ptr *f = &__DTOR_LIST__[2];	/* 0,1 contains -1,0 */	\
  int n = 0;							\
  while (*f)							\
    {								\
     f+= 2;				/* skip over alignment 0 */	\
     n++;							\
    }								\
  f -= 2;							\
  while (--n >= 0)						\
    {								\
     (*f) ();							\
     f-= 2;				/* skip over alignment 0 */	\
    }


