/* Target definitions for GNU compiler for mc680x0 running NeXTSTEP 2.1
   Copyright (C) 1989, 90, 91, 92, 93, 1994 Free Software Foundation, Inc.

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

/* Changed for NeXTStep2.1, Ch. Kranz, 2/94, 3/94 */

#include "m68k/next.h"
#include "nextstep21.h"

/* for #include <mach.h> in libgcc2.c */
#define NeXTStep21	

#undef	ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
 do { if (REAL_VALUE_ISINF (VALUE))					\
        {								\
          if (REAL_VALUE_NEGATIVE (VALUE))				\
            fprintf (FILE, "#0r-99e999");				\
          else								\
            fprintf (FILE, "#0r99e999");				\
        }								\
      else								\
        { char dstr[30];						\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);		\
          fprintf (FILE, "\t.double 0r%s\n", dstr);			\
        }								\
    } while (0)

/* This is how to output an assembler line defining a `float' constant.  */
#undef	ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
 do { if (REAL_VALUE_ISINF (VALUE))					\
        {								\
          if (REAL_VALUE_NEGATIVE (VALUE))				\
            fprintf (FILE, "\t.single 0r-99e999\n");			\
          else								\
            fprintf (FILE, "\t.single 0r99e999\n");			\
        }								\
      else								\
        { char dstr[30];						\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.20e", dstr);		\
          fprintf (FILE, "\t.single 0r%s\n", dstr);			\
        }								\
    } while (0)

/* called from m68k.c line 1881 */
#undef	ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(CODE,FILE,VALUE)		\
  do{ 								\
      if (CODE != 'f')						\
        {							\
          long l;						\
          REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);		\
          if (sizeof (int) == sizeof (long))			\
            asm_fprintf ((FILE), "%I0x%x", l);			\
          else							\
            asm_fprintf ((FILE), "%I0x%lx", l);			\
        }							\
      else if (REAL_VALUE_ISINF (VALUE))			\
        {							\
          if (REAL_VALUE_NEGATIVE (VALUE))			\
            fprintf (FILE, "#0r-99e999");			\
          else							\
            fprintf (FILE, "#0r99e999");			\
        }							\
      else							\
        { char dstr[30];					\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.9g", dstr);	\
          fprintf (FILE, "#0r%s", dstr);			\
        }							\
    } while (0)

#undef	ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
 do { if (REAL_VALUE_ISINF (VALUE))					\
        {								\
          if (REAL_VALUE_NEGATIVE (VALUE))				\
            fprintf (FILE, "#0r-99e999");				\
          else								\
            fprintf (FILE, "#0r99e999");				\
        }								\
      else								\
       { char dstr[30];							\
          REAL_VALUE_TO_DECIMAL ((VALUE), "%.20g", dstr);		\
          fprintf (FILE, "#0r%s", dstr);				\
       }								\
    } while (0)

