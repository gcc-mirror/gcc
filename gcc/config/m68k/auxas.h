/* Definitions for Motorola 680x0 running A/UX using /bin/as
   Copyright (C) 1996, 1997 Free Software Foundation, Inc.

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

#define USE_BIN_AS

#ifndef USE_COLLECT2
#define USE_COLLECT2
#endif

#ifndef __ASSEMBLY__

#include "m68k/sgs.h"

#define ASM_SPEC "%{m68030:-68030 }%{m68040:-68040 }"

/* Modify AT&T SGS assembler syntax */
/* A/UX's as doesn't do dots in pseudo-ops */

#define SDB_DEBUGGING_INFO

#define NO_DOLLAR_IN_LABEL
#define NO_DOT_IN_LABEL

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP	"\ttext"

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP	"\tdata\t1"

#undef BYTE_ASM_OP
#define	BYTE_ASM_OP		"byte"

#undef WORD_ASM_OP
#define WORD_ASM_OP		"short"

#undef LONG_ASM_OP
#define LONG_ASM_OP		"long"

#undef SPACE_ASM_OP
#define SPACE_ASM_OP		"space"

#undef ALIGN_ASM_OP
#define ALIGN_ASM_OP		"align"

#undef GLOBAL_ASM_OP
#define GLOBAL_ASM_OP		"\tglobal"

#undef SWBEG_ASM_OP
#define SWBEG_ASM_OP		"swbeg"

#undef SET_ASM_OP
#define SET_ASM_OP		"set"

#undef ASM_PN_FORMAT
#define ASM_PN_FORMAT		"%s%%%d"

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"L%"

#define ADDITIONAL_REGISTER_NAMES { "%a6", 14, "%a7", 15 }

#undef ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(FILE,VALUE)                      \
( fprintf ((FILE), "\t%s ", LONG_ASM_OP),               \
  output_addr_const ((FILE), (VALUE)),                  \
  fprintf ((FILE), "\n"))

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\tcomm\t", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
( fputs ("\tlcomm\t", (FILE)),				\
  assemble_name ((FILE), (NAME)),			\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)				\
  output_file_directive ((FILE), main_input_filename)

#undef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME)		\
( fputs ("\tfile\t", (FILE)),				\
  output_quoted_string ((FILE), (NAME)),		\
  fputc ('\n', (FILE)) )

#undef ASM_OUTPUT_CASE_FETCH
#define ASM_OUTPUT_CASE_FETCH(file, labelno, regname)	\
    asm_fprintf (file, "10(%Rpc,%s.", regname)

#define SGS_NO_LI

/* Random macros describing parts of SDB data.  */

#define PUT_SDB_SCL(a) \
  fprintf(asm_out_file, "\tscl\t%d%s", (a), SDB_DELIM)

#define PUT_SDB_INT_VAL(a) \
  fprintf (asm_out_file, "\tval\t%d%s", (a), SDB_DELIM)

#define PUT_SDB_VAL(a)				\
( fputs ("\tval\t", asm_out_file),		\
  output_addr_const (asm_out_file, (a)),	\
  fprintf (asm_out_file, SDB_DELIM))

#define PUT_SDB_DEF(a)				\
do { fprintf (asm_out_file, "\tdef\t");		\
     ASM_OUTPUT_LABELREF (asm_out_file, a); 	\
     fprintf (asm_out_file, SDB_DELIM); } while (0)

#define PUT_SDB_PLAIN_DEF(a) fprintf(asm_out_file,"\tdef\t~%s%s", a, SDB_DELIM)

#define PUT_SDB_ENDEF fputs("\tendef\n", asm_out_file)

#define PUT_SDB_TYPE(a) fprintf(asm_out_file, "\ttype\t0%o%s", a, SDB_DELIM)

#define PUT_SDB_SIZE(a) fprintf(asm_out_file, "\tsize\t%d%s", a, SDB_DELIM)

#define PUT_SDB_START_DIM fprintf(asm_out_file, "\tdim\t")

#define PUT_SDB_NEXT_DIM(a) fprintf(asm_out_file, "%d,", a)

#define PUT_SDB_LAST_DIM(a) fprintf(asm_out_file, "%d%s", a, SDB_DELIM)

#define PUT_SDB_TAG(a)				\
do { fprintf (asm_out_file, "\ttag\t");		\
     ASM_OUTPUT_LABELREF (asm_out_file, a);	\
     fprintf (asm_out_file, SDB_DELIM); } while (0)

#define PUT_SDB_BLOCK_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~bb%s\tval\t~%s\tscl\t100%s\tline\t%d%s\tendef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_BLOCK_END(LINE)			\
  fprintf (asm_out_file,			\
	   "\tdef\t~eb%s\tval\t~%s\tscl\t100%s\tline\t%d%s\tendef\n",  \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~bf%s\tval\t~%s\tscl\t101%s\tline\t%d%s\tendef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_FUNCTION_END(LINE)		\
  fprintf (asm_out_file,			\
	   "\tdef\t~ef%s\tval\t~%s\tscl\t101%s\tline\t%d%s\tendef\n", \
	   SDB_DELIM, SDB_DELIM, SDB_DELIM, (LINE), SDB_DELIM)

#define PUT_SDB_EPILOGUE_END(NAME)			\
do { fprintf (asm_out_file, "\tdef\t");			\
     ASM_OUTPUT_LABELREF (asm_out_file, NAME);		\
     fprintf (asm_out_file,				\
	      "%s\tval\t~%s\tscl\t-1%s\tendef\n",	\
	      SDB_DELIM, SDB_DELIM, SDB_DELIM); } while (0)

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), "~%dfake", (NUMBER));

#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)		\
    fprintf((FILE), "\tln\t%d\n", 			\
	    (sdb_begin_function_line > 1 ?		\
	     last_linenum - sdb_begin_function_line : 1))

#define ASM_MOV_INSN	"mov.l"

#define FUNCTION_PROFILER_SYMBOL "mcount%"

#endif /* !__ASSEMBLY__ */
