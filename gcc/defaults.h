/* Definitions of various defaults for how to do assembler output
   (most of which are designed to be appropriate for GAS or for
   some BSD assembler).

   Written by Ron Guilmette (rfg@ncd.com)

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

/* choose a reasonable default for ASM_OUTPUT_ASCII.  */

#ifndef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    char *_hide_p = (char *) (MYSTRING);				      \
    int _hide_thissize = (MYLENGTH);					      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      char *p = _hide_p;						      \
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
