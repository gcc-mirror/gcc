/* Definitions of target machine for GNU compiler.  Clipper/Clix version.
   Copyright (C) 1988, 1993, 1996, 1997, 1999, 2000 Free Software Foundation, Inc.

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

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dclipper -Dunix -Asystem=unix -Asystem=svr3 -Acpu=clipper -Amachine=clipper"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}} crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s crtn.o%s"

#undef LIB_SPEC

#define TARGET_MEM_FUNCTIONS

#define ASM_OUTPUT_ASCII(FILE,PTR,LEN)			\
do {							\
  const unsigned char *s = (const unsigned char *)(PTR);\
  size_t i, limit = (LEN);				\
  for (i = 0; i < limit; s++, i++)			\
    {							\
      if ((i % 8) == 0)					\
	fputs ("\n\t.byte\t", (FILE));			\
      fprintf ((FILE), "%s0x%x", (i%8?",":""), (unsigned)*s); \
    }							\
  fputs ("\n", (FILE));					\
} while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG) \
  fprintf(FILE, "\t.align %d\n", 1 << (LOG))


#define BSS_SECTION_ASM_OP  "\t.bss"
#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP "\t.section .init,\"x\""


/* Define a few machine-specific details of the implementation of
   constructors.

   The __CTORS_LIST__ goes in the .init section.  Define CTOR_LIST_BEGIN
   and CTOR_LIST_END to contribute to the .init section an instruction to
   push a word containing 0 (or some equivalent of that).

   TARGET_ASM_CONSTRUCTOR should be defined to push the address of the
   constructor.  */

#define CTOR_LIST_BEGIN				\
  asm (INIT_SECTION_ASM_OP);			\
  asm ("subq   $8,sp");				\
  asm ("loadq  $0,r0");				\
  asm ("storw  r0,(sp)")

/* don't need end marker */

#undef CTOR_LIST_END

/* fini psect is 8 aligned */

#define DTOR_LIST_BEGIN	\
  asm (DTORS_SECTION_ASM_OP);				\
  func_ptr __DTOR_LIST__[2] = { (func_ptr) (-1), 0 };

#undef TARGET_ASM_CONSTRUCTOR
#define TARGET_ASM_CONSTRUCTOR  clix_asm_out_constructor
#undef TARGET_ASM_DESTRUCTOR
#define TARGET_ASM_DESTRUCTOR   clix_asm_out_destructor

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


