/* Definitions of various defaults for how to do assembler output
   (most of which are designed to be appropriate for GAS or for
   some BSD assembler).
   Copyright (C) 1992, 1996-1999 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@monkeys.com)

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

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable or function named NAME.
   LABELNO is an integer which is different for each call.  */

#ifndef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
  do {									\
    int len = strlen (NAME);						\
    char *temp = (char *) alloca (len + 3);				\
    temp[0] = 'L';							\
    strcpy (&temp[1], (NAME));						\
    temp[len + 1] = '.';						\
    temp[len + 2] = 0;							\
    (OUTPUT) = (char *) alloca (strlen (NAME) + 11);			\
    ASM_GENERATE_INTERNAL_LABEL (OUTPUT, temp, LABELNO);		\
  } while (0)
#endif

#ifndef ASM_STABD_OP
#define ASM_STABD_OP ".stabd"
#endif

/* This is how to output an element of a case-vector that is absolute.
   Some targets don't use this, but we have to define it anyway.  */

#ifndef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
do { fprintf (FILE, "\t%s\t", ASM_LONG);				\
     ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", (VALUE));			\
     fputc ('\n', FILE);						\
   } while (0)
#endif

/* Provide default for ASM_OUTPUT_ALTERNATE_LABEL_NAME.  */
#ifndef ASM_OUTPUT_ALTERNATE_LABEL_NAME
#define ASM_OUTPUT_ALTERNATE_LABEL_NAME(FILE,INSN) \
do { ASM_OUTPUT_LABEL(FILE,LABEL_ALTERNATE_NAME (INSN)); } while (0)
#endif

/* choose a reasonable default for ASM_OUTPUT_ASCII.  */

#ifndef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    unsigned char *_hide_p = (unsigned char *) (MYSTRING);		      \
    int _hide_thissize = (MYLENGTH);					      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int i;								      \
      fprintf (asm_out_file, "\t.ascii \"");				      \
									      \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  register int c = p[i];					      \
	  if (c == '\"' || c == '\\')					      \
	    putc ('\\', asm_out_file);					      \
	  if (c >= ' ' && c < 0177)					      \
	    putc (c, asm_out_file);					      \
	  else								      \
	    {								      \
	      fprintf (asm_out_file, "\\%o", c);			      \
	      /* After an octal-escape, if a digit follows,		      \
		 terminate one string constant and start another.	      \
		 The Vax assembler fails to stop reading the escape	      \
		 after three digits, so this is the only way we		      \
		 can get it to parse the data properly.  */		      \
	      if (i < thissize - 1 && ISDIGIT(p[i + 1]))		      \
		fprintf (asm_out_file, "\"\n\t.ascii \"");		      \
	  }								      \
	}								      \
      fprintf (asm_out_file, "\"\n");					      \
    }									      \
  }									      \
  while (0)
#endif

#ifndef ASM_IDENTIFY_GCC
  /* Default the definition, only if ASM_IDENTIFY_GCC is not set,
     because if it is set, we might not want ASM_IDENTIFY_LANGUAGE
     outputting labels, if we do want it to, then it must be defined
     in the tm.h file.  */
#ifndef ASM_IDENTIFY_LANGUAGE
#define ASM_IDENTIFY_LANGUAGE(FILE) output_lang_identify (FILE);
#endif
#endif

/* This is how we tell the assembler to equate two values.  */
#ifdef SET_ASM_OP
#ifndef ASM_OUTPUT_DEF
#define ASM_OUTPUT_DEF(FILE,LABEL1,LABEL2)				\
 do {	fprintf ((FILE), "\t%s\t", SET_ASM_OP);				\
	assemble_name (FILE, LABEL1);					\
	fprintf (FILE, ",");						\
	assemble_name (FILE, LABEL2);					\
	fprintf (FILE, "\n");						\
  } while (0)
#endif
#endif

/* This is how to output a reference to a user-level label named NAME.  */

#ifndef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)  asm_fprintf ((FILE), "%U%s", (NAME))
#endif

/* This determines whether or not we support weak symbols.  */
#ifndef SUPPORTS_WEAK
#ifdef ASM_WEAKEN_LABEL
#define SUPPORTS_WEAK 1
#else
#define SUPPORTS_WEAK 0
#endif
#endif

/* If the target supports weak symbols, define TARGET_ATTRIBUTE_WEAK to
   provide a weak attribute.  Else define it to nothing. 

   This would normally belong in gansidecl.h, but SUPPORTS_WEAK is
   not available at that time.

   Note, this is only for use by target files which we know are to be
   compiled by GCC.  */
#ifndef TARGET_ATTRIBUTE_WEAK
# if SUPPORTS_WEAK
#  define TARGET_ATTRIBUTE_WEAK __attribute__ ((weak))
# else
#  define TARGET_ATTRIBUTE_WEAK
# endif
#endif

/* If we have a definition of INCOMING_RETURN_ADDR_RTX, assume that
   the rest of the DWARF 2 frame unwind support is also provided.  */
#if !defined (DWARF2_UNWIND_INFO) && defined (INCOMING_RETURN_ADDR_RTX)
#define DWARF2_UNWIND_INFO 1
#endif

#if defined (DWARF2_UNWIND_INFO) && !defined (EH_FRAME_SECTION)
# if defined (EH_FRAME_SECTION_ASM_OP)
#  define EH_FRAME_SECTION() eh_frame_section();
# else
   /* If we aren't using crtstuff to run ctors, don't use it for EH.  */
#  if defined (ASM_OUTPUT_SECTION_NAME) && defined (ASM_OUTPUT_CONSTRUCTOR)
#   define EH_FRAME_SECTION_ASM_OP	".section\t.eh_frame,\"aw\""
#   define EH_FRAME_SECTION() \
     do { named_section (NULL_TREE, ".eh_frame", 0); } while (0)
#  endif
# endif
#endif

/* By default, we generate a label at the beginning and end of the
   text section, and compute the size of the text section by
   subtracting the two.  However, on some platforms that doesn't 
   work, and we use the section itself, rather than a label at the
   beginning of it, to indicate the start of the section.  On such
   platforms, define this to zero.  */
#ifndef DWARF2_GENERATE_TEXT_SECTION_LABEL
#define DWARF2_GENERATE_TEXT_SECTION_LABEL 1
#endif

/* Supply a default definition for PROMOTE_PROTOTYPES.  */
#ifndef PROMOTE_PROTOTYPES
#define PROMOTE_PROTOTYPES	0
#endif

/* Number of hardware registers that go into the DWARF-2 unwind info.
   If not defined, equals FIRST_PSEUDO_REGISTER  */

#ifndef DWARF_FRAME_REGISTERS
#define DWARF_FRAME_REGISTERS FIRST_PSEUDO_REGISTER
#endif
