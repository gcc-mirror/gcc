/* Target definitions for GNU compiler for Alliant FX/2800
   running Concentrix 2.2
   Copyright (C) 1991, 1996, 1998, 1999, 2000 Free Software Foundation, Inc.
   Contributed by Howard Chu (hyc@hanauma.jpl.nasa.gov).

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

/* The Alliant fx2800 running Concentrix 2.x is weird.  This is basically
   a BSD 4.3 based operating system, but it uses svr4 ELF format object
   files and it somehow puts BSD stabs records into the ELF files for
   symbolic debug information.  The assembler is "mostly an SVR4 assembler
   with some Alliant additions. We based it on the `Intel 80860 Assembly
   Language Specification' from AT&T." */

/* This file consists of three sections. The first section establishes
   definitions unique to the Alliant FX/2800. The next section reconciles
   differences between Alliant and i860v4.h, and the last overrides the
   remaining differences with svr4.h */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (i860 Alliant)");

/* atexit is not present prior to Concentrix 2.2. Uncomment the following
   if you're on 2.1 or older. */
 
/* #define NEED_ATEXIT */

#define I860_STRICT_ABI_PROLOGUES

/* There is no avoiding this; -L does not exist at all (in Concentrix 2.2).  */
#define LINK_LIBGCC_SPECIAL 1

/* Most of the Alliant-specific definitions here are to get stab info that
   Alliant's dbx can understand. */

#define DBX_DEBUGGING_INFO
#define DEFAULT_GDB_EXTENSIONS 0
#define DBX_NO_XREFS
#define DBX_NO_EXTRA_TAGS

/* Alliant dbx also needs to see the function stab before anything
   else in the function. */

#define DBX_FUNCTION_FIRST
#define DBX_LBRAC_FIRST

/* Alliant dbx also needs to see the end of a function somewhere. */

#define DBX_OUTPUT_FUNCTION_END(file,decl)	\
	fprintf (file, ".stab \"\",.,0x%x,0,0\n", N_EFUN)

/* Alliant dbx has predefined types, so they must be emitted with the
   proper type numbers. The defined types are:

     Type #	C, Fortran, Pascal Types
	--	------------------------
	 1	char, integer*1
	 2	short, 	integer*2
	 3	int, long, integer*4, integer
	 4	logical*1, byte
	 5	logical*2
	 6	logical*4, logical
	 7	float, real*4, 	real
	 8	double, real*8,	double
	 9	single complex, complex*8, complex
	10	double complex, doublecomplex
	11	character
	12	void
	13	nil
	14	boolean
	15	unsigned char, ubyte
	16	unsigned short, uword
	17	unsigned, unsigned int, unsigned long, ulong
	18	quad, logical*8
	19	long long, integer*8
	20	unsigned long long, uquad*8
    21-100	reserved for future predefined types
	100	long redefine same as 3
	101	unsigned long same as 17
	--	--------------------
	102	First user program type

   Since long and unsigned long are int references, they must be handled
   as special cases. The Alliant compiler doesn't use types 18-20, so it
   sets long & unsigned long in 18 & 19, not in 100 & 101 as shown above. */

#define DBX_OUTPUT_STANDARD_TYPES(syms)	\
{ static const char *const dtyps[] = {					\
	"", "char", "short int", "int", "logical*1",			\
	"logical*2", "logical*4", "float", "double", "complex",		\
	"doublecomplex", "character", "void", "nil", "boolean",		\
	"unsigned char", "short unsigned int", "unsigned int",		\
	"logical*8", "long long int", "long long unsigned int",""};	\
									\
  tree decl;								\
  int i;								\
									\
  for (i=1;*dtyps[i];i++)						\
    for (decl = syms; decl; decl = TREE_CHAIN(decl))			\
	if ((TREE_CODE (decl) == TYPE_DECL) && DECL_NAME(decl) &&	\
	    !strcmp(IDENTIFIER_POINTER(DECL_NAME(decl)), dtyps[i])) {	\
		TYPE_SYMTAB_ADDRESS (TREE_TYPE (decl)) = i;		\
		typevec[i] = TYPE_DEFINED;				\
		dbxout_symbol (decl, 0);				\
		break;							\
	}								\
									\
  for (decl = syms; decl; decl = TREE_CHAIN(decl))			\
    if ((TREE_CODE (decl) == TYPE_DECL) && DECL_NAME(decl) &&		\
	!strcmp(IDENTIFIER_POINTER(DECL_NAME(decl)),"long int")) {	\
      TYPE_SYMTAB_ADDRESS (TREE_TYPE (decl)) = i;			\
      typevec[i] = TYPE_DEFINED;					\
      fprintf(asmfile,".stab \"long int:t%d=3\",0,0x%x,0,0\n",		\
		i++,N_LSYM);						\
      TREE_ASM_WRITTEN (decl) = 1;					\
      break;								\
    }									\
									\
  for (decl = syms; decl; decl = TREE_CHAIN(decl))			\
    if ((TREE_CODE (decl) == TYPE_DECL) && DECL_NAME(decl) && !strcmp(	\
	IDENTIFIER_POINTER(DECL_NAME(decl)),"long unsigned int")) {	\
      TYPE_SYMTAB_ADDRESS (TREE_TYPE (decl)) = i;			\
      typevec[i] = TYPE_DEFINED;					\
      fprintf(asmfile,".stab \"long unsigned int:t%d=17\",0,0x%x,0,0\n",\
		i++,N_LSYM);						\
      TREE_ASM_WRITTEN (decl) = 1;					\
      break;								\
    }									\
  next_type_number = i; };

/* Alliant dbx doesn't understand split names... */

#define DBX_CONTIN_LENGTH 0

/* The syntax for stabs records is also different; there is only a single
   ".stab" directive instead of the 3 directives in BSD, and the order of
   arguments is slightly changed. */

#define ASM_STABS_OP	"\t.stab "
#define ASM_STABN_OP	"\t.stab "
#define ASM_STABD_OP	"\t.stab "

#define DBX_MEMPARM_STABS_LETTER 'k'
#define DBX_REGPARM_STABS_LETTER 'r'

#undef  ASM_OUTPUT_SOURCE_LINE
#define ASM_OUTPUT_SOURCE_LINE(file,num)		\
	fprintf (file, "\t.stab \"\",.,0x%x,0,%d\n",	\
		N_SLINE,num)

#if 0	/* Alliant dbx only reads first N_SO, so it
	   ignores the filename if dir is present. */
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(file,name)		\
	fprintf (file, ".stab \"%s/\",.Ltext0,0x%x,0,0\n",	\
		name, N_SO)
#else
#define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(file,name)
#endif

#define DBX_OUTPUT_MAIN_SOURCE_FILENAME(file,name)		\
	fprintf (file, ".stab ");				\
	output_quoted_string (file, name);			\
	fprintf (file, ",.Ltext0,0x%x,0,0\n", N_SO);		\
	text_section ();					\
	ASM_OUTPUT_INTERNAL_LABEL (file, "Ltext", 0)

#define DBX_OUTPUT_SOURCE_FILENAME(file,name)			\
  do {	fprintf (file, ".stab ");				\
	output_quoted_string (file, name);			\
	fprintf (file, ",.Ltext0,0x%x,0,0\n", N_SOL);		\
  } while (0)

#define DBX_OUTPUT_CONSTANT_SYMBOL(file,name,ival)		\
	fprintf (file, ".stab \"%s:c=i%d\",0,0x%x,0,0\n",	\
		name, ival, N_LSYM)

#define DBX_FINISH_SYMBOL(decl)	\
	int line = 0;						\
	fprintf (asmfile, "\",");				\
	if (current_sym_addr)					\
	  output_addr_const (asmfile, current_sym_addr);	\
	else							\
	  fprintf (asmfile, "%d", current_sym_value);		\
	if (decl != 0 && TREE_CODE(decl) == FUNCTION_DECL)	\
	  line=DECL_SOURCE_LINE (decl);				\
	fprintf (asmfile, ",0x%x,%d,%d\n", current_sym_code,	\
	  line!=0?64:0,line)

#define DBX_OUTPUT_CATCH(file,decl,name)		\
  fprintf (file, ".stab \"%s:C1\",",			\
	   IDENTIFIER_POINTER (DECL_NAME (decl)));	\
  assemble_name (file, name);				\
  fprintf (file, ",0x%x,0,0\n", N_CATCH)

#define DBX_OUTPUT_LBRAC(file,name)	\
  if (depth > 1) {			\
    fprintf (file, ".stab \"\",");	\
    assemble_name (file, name);		\
    fprintf (file, ",0x%x,0,%d\n", N_LBRAC, depth); }

#define DBX_OUTPUT_RBRAC(file,name)	\
  if (depth > 1) {			\
    fprintf (file, ".stab \"\",");	\
    assemble_name (file, name);		\
    fprintf (file, ",0x%x,0,%d\n", N_RBRAC, depth); }

#define DBX_OUTPUT_ENUM(file,type)				\
  fprintf (file, "e3");						\
  CHARS(2);							\
  for (tem = TYPE_VALUES (type); tem; tem = TREE_CHAIN (tem))	\
    {								\
      fprintf (asmfile, "%s:%d,",				\
	       IDENTIFIER_POINTER (TREE_PURPOSE (tem)),		\
	       TREE_INT_CST_LOW (TREE_VALUE (tem)));		\
      CHARS (11 + IDENTIFIER_LENGTH (TREE_PURPOSE (tem)));	\
      if (TREE_CHAIN (tem) != 0)				\
	CONTIN;							\
    }								\
  putc (';', asmfile);						\
  CHARS (1);

/* Undefine some things defined in i860.h because the native C compiler
   on the FX/2800 emits code to do these operations inline.  For GCC,
   we will use the default implementation of these things... i.e.
   generating calls to libgcc routines.  */

#undef DIVSI3_LIBCALL
#undef UDIVSI3_LIBCALL
#undef REMSI3_LIBCALL
#undef UREMSI3_LIBCALL

/* Global pointer needs to be 8 byte aligned? Link error if not... */

#define DATA_ALIGNMENT(dummy,align)	\
	((TREE_PUBLIC (decl) &&	\
	 (POINTER_TYPE_P (TREE_TYPE (decl)))) ? 64: align)

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)	\
	fprintf (FILE, "\tcall __mcount_\n\tnop\n")

/* Overrides for i860v4.h begin here */

/* Provide a set of pre-definitions and pre-assertions appropriate for
   the i860 running Concentrix 2.x.  */

#undef CPP_PREDEFINES 
#define CPP_PREDEFINES "-Di860 -Dunix -DBSD4_3 -Dalliant -Asystem=unix -Asystem=bsd -Acpu=i860 -Amachine=i860"

#undef I860_REG_PREFIX
#undef ASM_COMMENT_START
#define ASM_COMMENT_START "//"

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)
#undef ASM_OUTPUT_FUNCTION_PREFIX
#define ASM_OUTPUT_FUNCTION_PREFIX(FILE,NAME) \
  fputs("\tnop\n", (FILE));			\
  current_function_original_name = (NAME)
#undef ASM_OUTPUT_PROLOGUE_SUFFIX

/* Overrides for svr4.h begin here */

#undef SWITCH_TAKES_ARG
#undef WORD_SWITCH_TAKES_ARG

#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#undef MD_STARTFILE_PREFIX
#undef MD_EXEC_PREFIX

/* Generate an error message if -p option is selected. Concentrix 2.x
   does not support prof format profiling, only gprof is supported. */

#define CPP_SPEC	"%{p:%e-p option not supported: use -pg instead}"

/* Provide an appropriate LIB_SPEC. The crtend.o file provides part of the
   support for getting C++ file-scope static objects constructed before
   entering `main'. */

#undef LIB_SPEC
#define LIB_SPEC \
	"%{g*:-lg} %{!pg:-lc}%{pg:-lc_p} crtend.o%s"

/* Tell linker to strip local symbols, since assembler may not. */

#undef LINK_SPEC
#define LINK_SPEC	"-X"

/* Get the correct startup file for regular or profiled code. Also
   use the crtbegin.o file for C++ ... */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
	"%{!pg:crt0.o%s}%{pg:gcrt0.o%s} crtbegin.o%s"

#undef SCCS_DIRECTIVE
#undef NO_DOLLAR_IN_LABEL
#undef TARGET_MEM_FUNCTIONS

#undef DWARF_DEBUGGING_INFO

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"

#undef ASM_OUTPUT_EXTERNAL_LIBCALL

/* ??? Is this used anywhere?  */
#undef BSS_ASM_OP
#define BSS_ASM_OP	"\t.lcomm "

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)					\
do {				 				\
     if (current_function_original_name != NULL) {		\
       const char *long_op = integer_asm_op (4, FALSE);		\
       tdesc_section();						\
       fprintf ((FILE), "%s __ETEXT\n", long_op);		\
       fprintf ((FILE), "%s 0\n", long_op);			\
       fputs ("\t.long\t__ETEXT\n", (FILE));			\
       fputs ("\t.long\t0\n", (FILE));				\
       text_section();						\
       fputs("__ETEXT:\n", (FILE));				\
     }								\
     if (!flag_no_ident)					\
	fprintf ((FILE), "\t.ident\t\"GCC: (GNU) %s\"\n",	\
		 version_string);				\
   } while (0)
