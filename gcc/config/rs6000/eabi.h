/* Core target definitions for GNU compiler
   for IBM RS/6000 PowerPC targeted to embedded ELF systems.
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Cygnus Support.

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

#include "rs6000/sysv4.h"

/* For now, make stabs the default debugging type, not dwarf. */
#undef	PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Make int foo : 8 not cause structures to be aligned to an int boundary */

#undef	PCC_BITFIELD_TYPE_MATTERS
#define	PCC_BITFIELD_TYPE_MATTERS (TARGET_BITFIELD_TYPE)

/* Define this macro to be the value 1 if instructions will fail to
   work if given data not on the nominal alignment.  If instructions
   will merely go slower in that case, define this macro as 0.

   Note, little endian systems trap on unaligned addresses, so never
   turn off strict alignment in that case. */
#undef	STRICT_ALIGNMENT
#define	STRICT_ALIGNMENT (TARGET_STRICT_ALIGN || TARGET_LITTLE_ENDIAN)

/* Align stack to 8 byte boundaries, rather than 16 bytes Sys V.4 uses */
#undef	STACK_BOUNDARY
#define	STACK_BOUNDARY	64

/* No data type wants to be aligned rounder than this.  */
#undef	BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 64

/* Put PC relative got entries in .got2 */
#undef	MINIMAL_TOC_SECTION_ASM_OP
#define MINIMAL_TOC_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE) ? "\t.section\t\".got2\",\"aw\"" : "\t.section\t\".got1\",\"aw\"")

/* Put relocatable data in .data, not .rodata so initialized pointers can be updated */
#undef	CONST_SECTION_ASM_OP
#define CONST_SECTION_ASM_OP \
  ((TARGET_RELOCATABLE) ? "\t.section\t\".data\"\t# .rodata" : "\t.section\t\".rodata\"")

/* Invoke an initializer function to set up the GOT */
#define NAME__MAIN "__eabi"
#define INVOKE__main 1

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (PowerPC Embedded)");

#undef CPP_PREDEFINES
#define CPP_PREDEFINES \
  "-DPPC -D__embedded__ -Asystem(embedded) -Acpu(powerpc) -Amachine(powerpc)"

/* Don't use startfiles or libraries except for libgcc.a */
#undef  STARTFILE_SPEC
#define	STARTFILE_SPEC ""

#undef	LIB_SPEC
#define	LIB_SPEC ""

#undef	LIBGCC_SPEC
#define	LIBGCC_SPEC "libgcc.a%s"

#undef	ENDFILE_SPEC
#define	ENDFILE_SPEC ""

/* This is how to output an assembler line defining an `int' constant.
   For -mrelocatable, we mark all addresses that need to be fixed up
   in the .fixup section.  */
#undef	ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE,VALUE)					\
do {									\
  static int recurse = 0;						\
  if (TARGET_RELOCATABLE						\
      && in_section != in_toc						\
      && in_section != in_text						\
      && in_section != in_ctors						\
      && in_section != in_dtors						\
      && !recurse							\
      && GET_CODE (VALUE) != CONST_INT					\
      && GET_CODE (VALUE) != CONST_DOUBLE				\
      && CONSTANT_P (VALUE))						\
    {									\
      static int labelno = 0;						\
      char buf[256], *p;						\
									\
      recurse = 1;							\
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", labelno++);		\
      STRIP_NAME_ENCODING (p, buf);					\
      fprintf (FILE, "%s:\n", p);					\
      fprintf (FILE, "\t.long (");					\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, ")@fixup\n");					\
      fprintf (FILE, "\t.section\t\".fixup\",\"aw\"\n");		\
      ASM_OUTPUT_ALIGN (FILE, 2);					\
      fprintf (FILE, "\t.long\t%s\n", p);				\
      fprintf (FILE, "\t.previous\n");					\
      recurse = 0;							\
    }									\
  else									\
    {									\
      fprintf (FILE, "\t.long ");					\
      output_addr_const (FILE, (VALUE));				\
      fprintf (FILE, "\n");						\
    }									\
} while (0)

