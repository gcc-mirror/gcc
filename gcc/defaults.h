/* Definitions of various defaults for how to do assembler output
   (most of which are designed to be appropriate for GAS or for
   some BSD assembler).

   Written by Ron Guilmette (rfg@netcom.com)

Copyright (C) 1992 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

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

/* This is how to output an element of a case-vector that is relative.
   Some targets don't use this, but we have to define it anyway.  */

#ifndef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL) \
do { fprintf (FILE, "\t%s\t", ASM_SHORT);				\
     ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", (VALUE));			\
     fputc ('-', FILE);							\
     ASM_OUTPUT_INTERNAL_LABEL (FILE, "L", (REL));			\
     fputc ('\n', FILE);						\
   } while (0)
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
	      if (i < thissize - 1					      \
		  && p[i + 1] >= '0' && p[i + 1] <= '9')		      \
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
