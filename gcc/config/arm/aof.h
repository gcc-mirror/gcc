/* Definitions of target machine for GNU compiler, for Advanced RISC Machines
   ARM compilation, AOF Assembler.
   Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rearnsha@armltd.co.uk)

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
   


#define AOF_ASSEMBLER

#define LINK_LIBGCC_SPECIAL 1

#define LINK_SPEC "%{aof} %{bin} %{aif} %{ihf} %{shl,*} %{reent*} %{split} \
		   %{ov*,*} %{reloc*} -nodebug"

#define STARTFILE_SPEC "crtbegin.o%s"

#define ENDFILE_SPEC "crtend.o%s"

#ifndef ASM_SPEC
#define ASM_SPEC "%{g -g} -arch 4 \
-apcs 3%{mapcs-32:/32bit}%{mapcs-26:/26bit}%{!mapcs-26:%{!macps-32:/26bit}}"
#endif

#ifndef LIB_SPEC
#define LIB_SPEC "%{Eb: armlib_h.32b%s}%{!Eb: armlib_h.32l%s}"
#endif

#define LIBGCC_SPEC "libgcc.a%s"

/* Dividing the Output into Sections (Text, Data, ...) */
/* AOF Assembler syntax is a nightmare when it comes to areas, since once
   we change from one area to another, we can't go back again.  Instead,
   we must create a new area with the same attributes and add the new output
   to that.  Unfortunately, there is nothing we can do here to guarantee that
   two areas with the same attributes will be linked adjacently in the
   resulting executable, so we have to be careful not to do pc-relative 
   addressing across such boundaries.  */
char *aof_text_section ();
#define TEXT_SECTION_ASM_OP aof_text_section ()

#define SELECT_RTX_SECTION(MODE,RTX) text_section ();

char *aof_data_section ();
#define DATA_SECTION_ASM_OP aof_data_section ()

#define EXTRA_SECTIONS in_zero_init, in_ctor, in_dtor, in_common

#define EXTRA_SECTION_FUNCTIONS	\
ZERO_INIT_SECTION		\
CTOR_SECTION			\
DTOR_SECTION			\
COMMON_SECTION

#define ZERO_INIT_SECTION					\
void								\
zero_init_section ()						\
{								\
  static int zero_init_count = 1;				\
  if (in_section != in_zero_init)				\
    {								\
      fprintf (asm_out_file, "\tAREA |C$$zidata%d|,NOINIT\n",	\
	       zero_init_count++);				\
      in_section = in_zero_init;				\
    }								\
}

#define CTOR_SECTION							\
void									\
ctor_section ()								\
{									\
  static int ctors_once = 0;						\
  if (in_section != in_ctor)						\
    {									\
      if (ctors_once)							\
	{								\
	  fprintf (stderr,						\
		   "Attempt to output more than one ctor section\n");	\
	  abort ();							\
	}								\
      fprintf (asm_out_file, "\t%s\n", CTORS_SECTION_ASM_OP);		\
      in_section = in_ctor;						\
      ctors_once = 1;							\
    }									\
}

#define DTOR_SECTION							\
void									\
dtor_section ()								\
{									\
  static int dtors_once = 0;						\
  if (in_section != in_dtor)						\
    {									\
      if (dtors_once)							\
	{								\
	  fprintf (stderr,						\
		   "Attempt to output more than one dtor section\n");	\
	  abort ();							\
	}								\
      fprintf (asm_out_file, "\t%s\n", DTORS_SECTION_ASM_OP);		\
      in_section = in_dtor;						\
      dtors_once = 1;							\
    }									\
}

/* Used by ASM_OUTPUT_COMMON (below) to tell varasm.c that we've
   changed areas.  */
#define COMMON_SECTION						\
void								\
common_section ()						\
{								\
  static int common_count = 1;					\
  if (in_section != in_common)					\
    {								\
      in_section = in_common;					\
    }								\
}
#define CTOR_LIST_BEGIN					\
asm (CTORS_SECTION_ASM_OP);				\
extern func_ptr __CTOR_END__[1];			\
func_ptr __CTOR_LIST__[1] = {__CTOR_END__};

#define CTOR_LIST_END					\
asm (CTORS_SECTION_ASM_OP);				\
func_ptr __CTOR_END__[1] = { (func_ptr) 0 };

#define DO_GLOBAL_CTORS_BODY		\
do {					\
  func_ptr *ptr = __CTOR_LIST__ + 1;	\
  while (*ptr)				\
    (*ptr++) ();			\
} while (0)

#define DTOR_LIST_BEGIN					\
asm (DTORS_SECTION_ASM_OP);				\
extern func_ptr __DTOR_END__[1];			\
func_ptr __DTOR_LIST__[1] = {__DTOR_END__};

#define DTOR_LIST_END					\
asm (DTORS_SECTION_ASM_OP);				\
func_ptr __DTOR_END__[1] = { (func_ptr) 0 };

#define DO_GLOBAL_DTORS_BODY		\
do {					\
  func_ptr *ptr = __DTOR_LIST__ + 1;	\
  while (*ptr)				\
    (*ptr++) ();			\
} while (0)

#define JUMP_TABLES_IN_TEXT_SECTION 1

#ifndef ARM_OS_NAME
#define ARM_OS_NAME "(generic)"
#endif

/* For the AOF linker, we need to reference __main to force the standard
   library to get linked in. */

#define ASM_FILE_START(STREAM)					\
{								\
  extern char *version_string;					\
  fprintf ((STREAM), "%s Generated by gcc %s for ARM/%s\n", 	\
	   ASM_COMMENT_START, version_string, ARM_OS_NAME);	\
  fprintf ((STREAM), "__r0\tRN\t0\n");				\
  fprintf ((STREAM), "__a1\tRN\t0\n");				\
  fprintf ((STREAM), "__a2\tRN\t1\n");				\
  fprintf ((STREAM), "__a3\tRN\t2\n");				\
  fprintf ((STREAM), "__a4\tRN\t3\n");				\
  fprintf ((STREAM), "__v1\tRN\t4\n");				\
  fprintf ((STREAM), "__v2\tRN\t5\n");				\
  fprintf ((STREAM), "__v3\tRN\t6\n");				\
  fprintf ((STREAM), "__v4\tRN\t7\n");				\
  fprintf ((STREAM), "__v5\tRN\t8\n");				\
  fprintf ((STREAM), "__v6\tRN\t9\n");				\
  fprintf ((STREAM), "__sl\tRN\t10\n");				\
  fprintf ((STREAM), "__fp\tRN\t11\n");				\
  fprintf ((STREAM), "__ip\tRN\t12\n");				\
  fprintf ((STREAM), "__sp\tRN\t13\n");				\
  fprintf ((STREAM), "__lr\tRN\t14\n");				\
  fprintf ((STREAM), "__pc\tRN\t15\n");				\
  fprintf ((STREAM), "__f0\tFN\t0\n");				\
  fprintf ((STREAM), "__f1\tFN\t1\n");				\
  fprintf ((STREAM), "__f2\tFN\t2\n");				\
  fprintf ((STREAM), "__f3\tFN\t3\n");				\
  fprintf ((STREAM), "__f4\tFN\t4\n");				\
  fprintf ((STREAM), "__f5\tFN\t5\n");				\
  fprintf ((STREAM), "__f6\tFN\t6\n");				\
  fprintf ((STREAM), "__f7\tFN\t7\n");				\
  text_section ();						\
}

/* Some systems use __main in a way incompatible with its use in gcc, in these
   cases use the macros NAME__MAIN to give a quoted symbol and SYMBOL__MAIN to
   give the same symbol without quotes for an alternative entry point.  You
   must define both, or neither. */
#define NAME__MAIN "__gccmain"
#define SYMBOL__MAIN __gccmain

#define ASM_FILE_END(STREAM)		\
do					\
{					\
  if (flag_pic)				\
    aof_dump_pic_table (STREAM);	\
  aof_dump_imports (STREAM);		\
  fputs ("\tEND\n", (STREAM));		\
} while (0);

#define ASM_IDENTIFY_GCC(STREAM) fputs ("|gcc2_compiled.|\n", (STREAM))

#define ASM_COMMENT_START ";"

#define ASM_APP_ON ""

#define ASM_APP_OFF ""

#define ASM_OUTPUT_LONG_DOUBLE(STREAM,VALUE) \
  ASM_OUTPUT_DOUBLE((STREAM),(VALUE))

#define ASM_OUTPUT_DOUBLE(STREAM,VALUE)				\
do {								\
  char dstr[30];						\
  long l[2];							\
  REAL_VALUE_TO_TARGET_DOUBLE ((VALUE), l);			\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.14g", dstr);		\
  fprintf ((STREAM), "\tDCD &%lx, &%lx\t%s double %s\n",	\
	   l[0], l[1], ASM_COMMENT_START, dstr);		\
} while (0)

#define ASM_OUTPUT_FLOAT(STREAM,VALUE)			\
do {							\
  char dstr[30];					\
  long l;						\
  REAL_VALUE_TO_TARGET_SINGLE ((VALUE), l);		\
  REAL_VALUE_TO_DECIMAL ((VALUE), "%.7g", dstr);	\
  fprintf ((STREAM), "\tDCD &%lx\t%s double %s\n",	\
	   l, ASM_COMMENT_START, dstr);			\
} while (0)

#define ASM_OUTPUT_INT(STREAM,VALUE)		\
  (fprintf ((STREAM), "\tDCD\t"),		\
   output_addr_const ((STREAM), (VALUE)),	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_SHORT(STREAM,VALUE)		\
  (fprintf ((STREAM), "\tDCW\t"),		\
   output_addr_const ((STREAM), (VALUE)),	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_CHAR(STREAM,VALUE)		\
  (fprintf ((STREAM), "\tDCB\t"),		\
   output_addr_const ((STREAM), (VALUE)),	\
   fputc ('\n', (STREAM)))

#define ASM_OUTPUT_BYTE(STREAM,VALUE)		\
  fprintf ((STREAM), "\tDCB\t%d\n", (VALUE))

#define ASM_OUTPUT_ASCII(STREAM,PTR,LEN)		\
{							\
  int i;						\
  char *ptr = (PTR);					\
  fprintf ((STREAM), "\tDCB");				\
  for (i = 0; i < (LEN); i++)				\
    fprintf ((STREAM), " &%02x%s", 			\
	     (unsigned ) *(ptr++),			\
	     (i + 1 < (LEN)				\
	      ? ((i & 3) == 3 ? "\n\tDCB" : ",")	\
	      : "\n"));					\
}

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == '\n')

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Output of Uninitialized Variables */

#define ASM_OUTPUT_COMMON(STREAM,NAME,SIZE,ROUNDED)		\
  (common_section (),						\
   fprintf ((STREAM), "\tAREA "),				\
   assemble_name ((STREAM), (NAME)),				\
   fprintf ((STREAM), ", DATA, COMMON\n\t%% %d\t%s size=%d\n",	\
	    (ROUNDED), ASM_COMMENT_START, SIZE))

#define ASM_OUTPUT_LOCAL(STREAM,NAME,SIZE,ROUNDED)	\
   (zero_init_section (),				\
    assemble_name ((STREAM), (NAME)),			\
    fprintf ((STREAM), "\n"),				\
    fprintf ((STREAM), "\t%% %d\t%s size=%d\n",		\
	     (ROUNDED), ASM_COMMENT_START, SIZE))

/* Output and Generation of Labels */

extern int arm_main_function;

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)		\
do {							\
  fprintf ((STREAM), "\tEXPORT\t");			\
  assemble_name ((STREAM), (NAME));			\
  fputc ('\n', (STREAM));				\
  if ((NAME)[0] == 'm' && ! strcmp ((NAME), "main"))	\
    arm_main_function = 1;				\
} while (0)

#define ASM_OUTPUT_LABEL(STREAM,NAME)	\
do {					\
  assemble_name (STREAM,NAME);		\
  fputs ("\n", STREAM);			\
} while (0)

#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL) \
{						\
  if (TARGET_POKE_FUNCTION_NAME)		\
    arm_poke_function_name ((STREAM), (NAME));	\
  ASM_OUTPUT_LABEL (STREAM, NAME);		\
  if (! TREE_PUBLIC (DECL))			\
    {						\
      fputs ("\tKEEP ", STREAM);		\
      ASM_OUTPUT_LABEL (STREAM, NAME);		\
    }						\
  aof_delete_import ((NAME));			\
}

#define ASM_DECLARE_OBJECT_NAME(STREAM,NAME,DECL) \
{						\
  ASM_OUTPUT_LABEL (STREAM, NAME);		\
  if (! TREE_PUBLIC (DECL))			\
    {						\
      fputs ("\tKEEP ", STREAM);		\
      ASM_OUTPUT_LABEL (STREAM, NAME);		\
    }						\
  aof_delete_import ((NAME));			\
}

#define ASM_OUTPUT_EXTERNAL(STREAM,DECL,NAME)	\
 aof_add_import ((NAME))

#define ASM_OUTPUT_EXTERNAL_LIBCALL(STREAM,SYMREF)	\
 (fprintf ((STREAM), "\tIMPORT\t"),			\
  assemble_name ((STREAM), XSTR ((SYMREF), 0)),		\
  fputc ('\n', (STREAM)))

#define ASM_OUTPUT_LABELREF(STREAM,NAME)	\
  fprintf ((STREAM), "|%s|", NAME)

#define ASM_GENERATE_INTERNAL_LABEL(STRING,PREFIX,NUM)	\
  sprintf ((STRING), "*|%s..%d|", (PREFIX), (NUM))

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR,NAME,NUMBER)	\
 ((OUTVAR) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER)))

/* How initialization functions are handled */

#define CTORS_SECTION_ASM_OP "AREA\t|C$$gnu_ctorsvec|, DATA, READONLY"
#define DTORS_SECTION_ASM_OP "AREA\t|C$$gnu_dtorsvec|, DATA, READONLY"

#define ASM_OUTPUT_CONSTRUCTOR(STREAM,NAME)	\
do {						\
  ctor_section ();				\
  fprintf ((STREAM), "\tDCD\t");		\
  assemble_name ((STREAM), (NAME));		\
  fputc ('\n', (STREAM));			\
} while (0);

#define ASM_OUTPUT_DESTRUCTOR(STREAM,NAME)	\
do {						\
  dtor_section ();				\
  fprintf ((STREAM), "\tDCD\t");		\
  assemble_name ((STREAM), (NAME));		\
  fputc ('\n', (STREAM));			\
} while (0);

/* Output of Assembler Instructions */

#define REGISTER_NAMES			\
{					\
  "a1", "a2", "a3", "a4",	\
  "v1", "v2", "v3", "v4",	\
  "v5", "v6", "sl", "fp",	\
  "ip", "sp", "lr", "pc",	\
  "f0", "f1", "f2", "f3",	\
  "f4", "f5", "f6", "f7",	\
  "cc", "sfp", "afp"		\
}

#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {"r0", 0}, {"a1", 0},				\
  {"r1", 1}, {"a2", 1},				\
  {"r2", 2}, {"a3", 2},				\
  {"r3", 3}, {"a4", 3},		      		\
  {"r4", 4}, {"v1", 4},				\
  {"r5", 5}, {"v2", 5},				\
  {"r6", 6}, {"v3", 6},				\
  {"r7", 7}, {"wr", 7},				\
  {"r8", 8}, {"v5", 8},				\
  {"r9", 9}, {"v6", 9},				\
  {"r10", 10}, {"sl", 10}, {"v7", 10},		\
  {"r11", 11}, {"fp", 11},			\
  {"r12", 12}, {"ip", 12}, 			\
  {"r13", 13}, {"sp", 13}, 			\
  {"r14", 14}, {"lr", 14},			\
  {"r15", 15}, {"pc", 15}			\
}

#define REGISTER_PREFIX "__"
#define USER_LABEL_PREFIX ""
#define LOCAL_LABEL_PREFIX ""

/* AOF does not prefix user function names with an underscore.  */
#define ARM_MCOUNT_NAME "_mcount"

/* Output of Dispatch Tables */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM,BODY,VALUE,REL)		\
  fprintf ((STREAM), "\tb\t|L..%d|\n", (VALUE))

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)	\
  fprintf ((STREAM), "\tDCD\t|L..%d|\n", (VALUE))

/* A label marking the start of a jump table is a data label. */
#define ASM_OUTPUT_CASE_LABEL(STREAM,PREFIX,NUM,TABLE)	\
  fprintf ((STREAM), "\tALIGN\n|%s..%d|\n", (PREFIX), (NUM))

/* Assembler Commands for Alignment */

#define ASM_OUTPUT_SKIP(STREAM,NBYTES)		\
 fprintf ((STREAM), "\t%%\t%d\n", (NBYTES))

#define ASM_OUTPUT_ALIGN(STREAM,POWER)			\
do {							\
  register int amount = 1 << (POWER);			\
  if (amount == 2)					\
    fprintf ((STREAM), "\tALIGN 2\n");			\
  else if (amount == 4)					\
    fprintf ((STREAM), "\tALIGN\n");			\
  else							\
    fprintf ((STREAM), "\tALIGN %d\n", amount);		\
} while (0)

#include "arm/arm.h"

#undef DBX_DEBUGGING_INFO
