/* Definitions of target machine for GNU compiler, for DEC Alpha on Cray
   T3E running Unicos/Mk.
   Copyright (C) 2001
   Free Software Foundation, Inc.
   Contributed by Roman Lechtchinsky (rl@cs.tu-berlin.de)

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

#undef TARGET_ABI_UNICOSMK
#define TARGET_ABI_UNICOSMK 1

/* CAM requires a slash before floating-pointing instruction suffixes.  */

#undef TARGET_AS_SLASH_BEFORE_SUFFIX
#define TARGET_AS_SLASH_BEFORE_SUFFIX 1

/* The following defines are necessary for the standard headers to work
   correctly.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__unix=1 -D_UNICOS=205 -D_CRAY=1 -D_CRAYT3E=1 -D_CRAYMPP=1 -D_CRAYIEEE=1 -D_ADDR64=1 -D_LD64=1 -D__UNICOSMK__ -D__INT_MAX__=9223372036854775807 -D__SHRT_MAX__=2147483647"

/* Disable software floating point emulation because it requires a 16-bit
   type which we do not have.  */

#ifndef __GNUC__
#undef REAL_ARITHMETIC
#endif

#define SHORT_TYPE_SIZE 32

#undef INT_TYPE_SIZE
#define INT_TYPE_SIZE 64

/* This is consistent with the definition Cray CC uses.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 64

/*
#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
*/

/* Alphas are operated in big endian mode on the Cray T3E.  */

#undef BITS_BIG_ENDIAN
#undef BYTES_BIG_ENDIAN
#undef WORDS_BIG_ENDIAN
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1


/* Every structure's size must be a multiple of this.  */

#undef STRUCTURE_SIZE_BOUNDARY
#define STRUCTURE_SIZE_BOUNDARY 64

/* No data type wants to be aligned rounder than this.  */

#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 256

/* Include the frame pointer in fixed_regs and call_used_regs as it can't be 
   used as a general-purpose register even in frameless functions.
   ??? The global_regs hack is needed for now because -O2 sometimes tries to 
   eliminate $15 increments/decrements in frameless functions.  */

#undef CONDITIONAL_REGISTER_USAGE
#define CONDITIONAL_REGISTER_USAGE	\
  do {					\
    fixed_regs[15] = 1;			\
    call_used_regs[15] = 1;		\
    global_regs[15] = 1;		\
  } while(0)

/* The stack frame grows downward.  */

#define FRAME_GROWS_DOWNWARD

/* Define the offset between two registers, one to be eliminated, and the
   other its replacement, at the start of a routine. This is somewhat
   complicated on the T3E which is why we use a function.  */

extern int unicosmk_initial_elimination_offset PARAMS ((int, int));

#undef INITIAL_ELIMINATION_OFFSET
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  do {									\
    (OFFSET) = unicosmk_initial_elimination_offset ((FROM), (TO));	\
  } while (0)


/* Define this if stack space is still allocated for a parameter passed
   in a register. On the T3E, stack space is preallocated for all outgoing
   arguments, including those passed in registers. To avoid problems, we
   assume that at least 48 bytes (i.e. enough space for all arguments passed
   in registers) are allocated.  */

#define REG_PARM_STACK_SPACE(DECL) 48
#define OUTGOING_REG_PARM_STACK_SPACE

/* If an argument can't be passed in registers even though not all argument
   registers have been used yet, it is passed on the stack in the space 
   preallocated for these registers.  */

#define STACK_PARMS_IN_REG_PARM_AREA

/* This evaluates to nonzero if we do not know how to pass TYPE solely in
   registers. This is the case for all arguments that do not fit in two
   registers.  */

#define MUST_PASS_IN_STACK(MODE,TYPE)					\
  ((TYPE) != 0                                          		\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST      		\
       || (TREE_ADDRESSABLE (TYPE) || ALPHA_ARG_SIZE (MODE, TYPE, 0) > 2)))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On Unicos/Mk, this is a structure that contains various information for
   the static subroutine information block (SSIB) and the call information
   word (CIW).  */

typedef struct {

  /* The overall number of arguments.  */
  int num_args;

  /* The overall size of the arguments in words.  */
  int num_arg_words;

  /* The number of words passed in registers.  */
  int num_reg_words;

  /* If an argument must be passed in the stack, all subsequent arguments
     must be passed there, too. This flag indicates whether this is the
     case.  */
  int force_stack;

  /* This array indicates whether a word is passed in an integer register or
     a floating point one.  */

  /* For each of the 6 register arguments, the corresponding flag in this
     array indicates whether the argument is passed in an integer or a
     floating point register.  */
  int reg_args_type[6];

} unicosmk_arg_info;

#undef CUMULATIVE_ARGS
#define CUMULATIVE_ARGS unicosmk_arg_info

/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to a
   function whose data type is FNTYPE.  For a library call, FNTYPE is 0.  */

#undef INIT_CUMULATIVE_ARGS
#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)	\
  do { (CUM).num_args = 0;					\
       (CUM).num_arg_words = 0;					\
       (CUM).num_reg_words = 0;					\
       (CUM).force_stack = 0;					\
  } while(0)

/* Update the data in CUM to advance over an argument of mode MODE and data
   type TYPE. (TYPE is null for libcalls where that information may not be
   available.)

   On Unicos/Mk, at most 6 words can be passed in registers. Structures
   which fit in two words are passed in registers, larger structures are
   passed on stack.  */

#undef FUNCTION_ARG_ADVANCE
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
do {								\
  int size;							\
								\
  size = ALPHA_ARG_SIZE (MODE, TYPE, NAMED);			\
                                                                \
  if (size > 2 || MUST_PASS_IN_STACK (MODE, TYPE)		\
      || (CUM).num_reg_words + size > 6)			\
    (CUM).force_stack = 1;					\
                                                                \
  if (! (CUM).force_stack)					\
    {								\
      int i;							\
      int isfloat;						\
      isfloat = (GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT	\
              || GET_MODE_CLASS (MODE) == MODE_FLOAT);		\
      for (i = 0; i < size; i++)				\
        {							\
          (CUM).reg_args_type[(CUM).num_reg_words] = isfloat;	\
          ++(CUM).num_reg_words;				\
        }							\
    }								\
  (CUM).num_arg_words += size;					\
  ++(CUM).num_args;						\
} while(0)

/* We want the default definition for this.
   ??? In fact, we should delete the definition from alpha.h as it
   corresponds to the default definition for little-endian machines.  */

#undef FUNCTION_ARG_PADDING

/* An argument is passed either entirely in registers or entirely on stack.  */
 
#undef FUNCTION_ARG_PARTIAL_NREGS
/* #define FUNCTION_ARG_PARTIAL_NREGS(CUM,MODE,TYPE,NAMED) 0 */

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   On Unicos/Mk, the standard subroutine __T3E_MISMATCH stores all register
   arguments on the stack. Unfortunately, it doesn't always store the first
   one (i.e. the one that arrives in $16 or $f16). This is not a problem
   with stdargs as we always have at least one named argument there. This is
   not always the case when varargs.h is used, however. In such cases, we
   have to store the first argument ourselves. We use the information from
   the CIW to determine whether the first argument arrives in $16 or $f16.  */

#undef SETUP_INCOMING_VARARGS
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL)	\
{ if ((CUM).num_reg_words < 6)						\
    {									\
      if (! (NO_RTL))							\
        {								\
	  int start;							\
									\
	  start = (CUM).num_reg_words;					\
	  if (!current_function_varargs || start == 0)			\
	    ++start;							\
									\
          emit_insn (gen_umk_mismatch_args (GEN_INT (start)));		\
	  if (current_function_varargs && (CUM).num_reg_words == 0)	\
	    {								\
	      rtx tmp;							\
	      rtx int_label, end_label;					\
									\
	      tmp = gen_reg_rtx (DImode);				\
	      emit_move_insn (tmp,					\
			      gen_rtx_ZERO_EXTRACT (DImode,		\
						    gen_rtx_REG (DImode, 2),\
						    (GEN_INT (1)),	\
						    (GEN_INT (7))));	\
	      int_label = gen_label_rtx ();				\
	      end_label = gen_label_rtx ();				\
	      emit_insn (gen_cmpdi (tmp, GEN_INT (0)));			\
	      emit_jump_insn (gen_beq (int_label));			\
	      emit_move_insn (gen_rtx_MEM (DFmode, virtual_incoming_args_rtx),\
			      gen_rtx_REG (DFmode, 48));		\
	      emit_jump (end_label);					\
	      emit_label (int_label);					\
	      emit_move_insn (gen_rtx_MEM (DImode, virtual_incoming_args_rtx),\
			      gen_rtx_REG (DImode, 16));		\
	      emit_label (end_label);					\
	    }								\
	  emit_insn (gen_arg_home_umk ());				\
        }								\
									\
      PRETEND_SIZE = 0;							\
    }									\
}

/* This ensures that $15 increments/decrements in leaf functions won't get
   eliminated.  */

#undef EPILOGUE_USES
#define EPILOGUE_USES(REGNO)  ((REGNO) == 26 || (REGNO) == 15)

/* Machine-specific function data.  */

struct machine_function
{
  /* List of call information words for calls from this function.  */
  struct rtx_def *first_ciw;
  struct rtx_def *last_ciw;
  int ciw_count;

  /* List of deferred case vectors.  */
  struct rtx_def *addr_list;
};

/* Would have worked, only the stack doesn't seem to be executable
#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE)			\
do { fprintf (FILE, "\tbr $1,0\n");			\
     fprintf (FILE, "\tldq $0,12($1)\n");		\
     fprintf (FILE, "\tldq $1,20($1)\n");		\
     fprintf (FILE, "\tjmp $31,(r0)\n");		\
     fprintf (FILE, "\tbis $31,$31,$31\n");		\
     fprintf (FILE, "\tbis $31,$31,$31\n");		\
} while (0) */

/* We don't support nested functions (yet).  */

#undef TRAMPOLINE_TEMPLATE
#define TRAMPOLINE_TEMPLATE(FILE) abort ()

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction. On Unicos/Mk, we don't support relative case
   vectors yet, thus the entries should be absolute addresses.  */ 

#undef CASE_VECTOR_MODE
#define CASE_VECTOR_MODE DImode

#undef CASE_VECTOR_PC_RELATIVE

/* Define this as 1 if `char' should by default be signed; else as 0.  */
/* #define DEFAULT_SIGNED_CHAR 1 */

/* The Cray assembler is really weird with respect to sections. It has only
   named sections and you can't reopen a section once it has been closed.
   This means that we have to generate unique names whenever we want to
   reenter the text or the data section. The following is a rather bad hack
   as TEXT_SECTION_ASM_OP and DATA_SECTION_ASM_OP are supposed to be
   constants.  */

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP unicosmk_text_section ()

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP unicosmk_data_section ()

/* There are ni read-only sections on Unicos/Mk.  */

#undef READONLY_DATA_SECTION
#define READONLY_DATA_SECTION data_section

/* Define extra sections for common data and SSIBs (static subroutine
   information blocks). The actual section header is output by the callers
   of these functions.  */

#undef EXTRA_SECTIONS
#undef EXTRA_SECTION_FUNCTIONS

#define EXTRA_SECTIONS in_common, in_ssib
#define EXTRA_SECTION_FUNCTIONS	\
COMMON_SECTION			\
SSIB_SECTION	

extern void common_section PARAMS ((void));
#define COMMON_SECTION		\
void				\
common_section ()		\
{				\
  in_section = in_common;	\
}

extern void ssib_section PARAMS ((void));
#define SSIB_SECTION		\
void				\
ssib_section ()			\
{				\
  in_section = in_ssib;		\
}

/* A C expression which evaluates to true if declshould be placed into a
   unique section for some target-specific reason. On Unicos/Mk, functions
   and public variables are always placed in unique sections.  */ 

/*
#define UNIQUE_SECTION_P(DECL) (TREE_PUBLIC (DECL)		\
				|| TREE_CODE (DECL) == FUNCTION_DECL)
*/
#define UNIQUE_SECTION(DECL, RELOC) unicosmk_unique_section (DECL, RELOC)

/* This outputs text to go at the start of an assembler file.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)	unicosmk_asm_file_start (FILE)

/* This outputs text to go at the end of an assembler file.  */

#undef ASM_FILE_END
#define ASM_FILE_END(FILE)	unicosmk_asm_file_end (FILE)

/* We take care of that in ASM_FILE_START.  */

#undef ASM_OUTPUT_SOURCE_FILENAME

/* There is no directive for declaring a label as global. Instead, an 
   additional colon must be appended when the label is defined.  */

#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(FILE,NAME)

/* This is how to output a label for a jump table.  Arguments are the same as
   for ASM_OUTPUT_INTERNAL_LABEL, except the insn for the jump table is
   passed.  */

#undef ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLEINSN)	\
  ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM)

/* CAM has some restrictions with respect to string literals. It won't
   accept lines with more that 256 characters which means that we have
   to split long strings. Moreover, it only accepts escape sequences of
   the form \nnn in the range 0 to 127. We generate .byte directives for
   escapes characters greater than 127. And finally, ` must be escaped.  */

#undef ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(MYFILE, MYSTRING, MYLENGTH) \
  do {									      \
    FILE *_hide_asm_out_file = (MYFILE);				      \
    const unsigned char *_hide_p = (const unsigned char *) (MYSTRING);	      \
    int _hide_thissize = (MYLENGTH);					      \
    int _size_so_far = 0;						      \
    {									      \
      FILE *asm_out_file = _hide_asm_out_file;				      \
      const unsigned char *p = _hide_p;					      \
      int thissize = _hide_thissize;					      \
      int in_ascii = 0;							      \
      int i;								      \
									      \
      for (i = 0; i < thissize; i++)					      \
	{								      \
	  register int c = p[i];					      \
									      \
	  if (c > 127)							      \
	    {								      \
	      if (in_ascii)						      \
		{							      \
		  fprintf (asm_out_file, "\"\n");			      \
		  in_ascii = 0;						      \
		}							      \
									      \
	      fprintf (asm_out_file, "\t.byte\t%d\n", c);		      \
	    }								      \
	  else								      \
	    {								      \
	      if (! in_ascii)						      \
		{							      \
		  fprintf (asm_out_file, "\t.ascii\t\"");		      \
		  in_ascii = 1;						      \
		  _size_so_far = 0;					      \
		}							      \
	      else if (_size_so_far >= 64)				      \
		{							      \
		  fprintf (asm_out_file, "\"\n\t.ascii\t\"");		      \
		  _size_so_far = 0;					      \
		}							      \
									      \
	      if (c == '\"' || c == '\\' || c == '`')			      \
		putc ('\\', asm_out_file);				      \
	      if (c >= ' ')						      \
		putc (c, asm_out_file);					      \
	      else							      \
		fprintf (asm_out_file, "\\%.3o", c);			      \
	      ++ _size_so_far;						      \
	    }								      \
	}								      \
      if (in_ascii)							      \
	fprintf (asm_out_file, "\"\n");					      \
    }									      \
  } while(0)

/* This is how to output an element of a case-vector that is absolute.  */

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)	\
  fprintf (FILE, "\t.quad $L%d\n", (VALUE))

/* This is how to output an element of a case-vector that is relative.
   (Unicos/Mk does not use such vectors yet).  */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL) abort ()

/* We can't output case vectors in the same section as the function code
   because CAM doesn't allow data definitions in code sections. Thus, we
   simply record the case vectors and put them in a separate section after
   the function.  */

#define ASM_OUTPUT_ADDR_VEC(LAB,VEC) \
  unicosmk_defer_case_vector ((LAB),(VEC))

#define ASM_OUTPUT_ADDR_DIFF_VEC(LAB,VEC) abort ()

/* This is how to output an assembler line that says to advance the location
   counter to a multiple of 2**LOG bytes. Annoyingly, CAM always uses zeroes
   to fill the unused space which does not work in code sections. We have to 
   be careful not to use the .align directive in code sections.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(STREAM,LOG) unicosmk_output_align (STREAM, LOG)

/* This is how to advance the location counter by SIZE bytes.  */

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM,SIZE)			\
  fprintf ((STREAM), "\t.byte\t0:%d\n", (SIZE));

/* This says how to output an assembler line to define a global common
   symbol. We need the alignment information because it has to be supplied
   in the section header.  */ 

#undef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)	\
  unicosmk_output_common ((FILE), (NAME), (SIZE), (ALIGN))

/* This says how to output an assembler line to define a local symbol.  */

#undef ASM_OUTPUT_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGN) \
  do { data_section ();					\
       fprintf (FILE, "\t.align\t%d\n", floor_log2 ((ALIGN) / BITS_PER_UNIT));\
       ASM_OUTPUT_LABEL ((FILE), (NAME));		\
       fprintf (FILE, "\t.byte 0:%d\n", SIZE);		\
  } while (0)

/* CAM does not allow us to declare a symbol as external first and then
   define it in the same file later. Thus, we keep a list of all external
   references, remove all symbols defined locally from it and output it at
   the end of the asm file.  */
   
#define ASM_OUTPUT_EXTERNAL(FILE,DECL,NAME) \
  unicosmk_add_extern ((NAME))

#define ASM_OUTPUT_EXTERNAL_LIBCALL(STREAM,SYMREF)	\
  unicosmk_add_extern (XSTR ((SYMREF), 0))

/* This is how to declare an object. We don't have to output anything if
   it is a global variable because those go into unique `common' sections
   and the section name is globally visible. For local variables, we simply
   output the label. In any case, we have to record that no extern
   declaration should be generated for the symbol.  */

#define ASM_DECLARE_OBJECT_NAME(STREAM,NAME,DECL) 	\
  do { tree name_tree;					\
       name_tree = get_identifier ((NAME));		\
       TREE_ASM_WRITTEN (name_tree) = 1;		\
       if (!TREE_PUBLIC (DECL))				\
	 {						\
	   assemble_name (STREAM, NAME);		\
	   fputs (":\n", STREAM);			\
         }						\
  } while(0)

/*
#define ASM_OUTPUT_SECTION_NAME(STREAM, DECL, NAME, RELOC)	\
  unicosmk_output_section_name ((STREAM), (DECL), (NAME), (RELOC))
*/

/* Switch into a generic section.  */
#define TARGET_ASM_NAMED_SECTION unicosmk_asm_named_section

#undef ASM_OUTPUT_MAX_SKIP_ALIGN
#define ASM_OUTPUT_MAX_SKIP_ALIGN(STREAM,POWER,MAXSKIP)

/* We have to define these because we do not use the floating-point
   emulation. Unfortunately, atof does not accept hex literals.  */ 

#ifndef REAL_ARITHMETIC
#define REAL_VALUE_ATOF(x,s) atof(x)
#define REAL_VALUE_HTOF(x,s) atof(x)

#define REAL_VALUE_TO_TARGET_SINGLE(IN, OUT)			\
do {								\
  union {							\
    float f;							\
    HOST_WIDE_INT l;						\
  } u;								\
								\
  u.f = (IN);							\
  (OUT) = (u.l >> 32) & 0xFFFFFFFF;				\
} while (0) 

#define REAL_VALUE_TO_TARGET_DOUBLE(IN, OUT)			\
do {								\
  union {							\
    REAL_VALUE_TYPE f;						\
    HOST_WIDE_INT l;						\
  } u;								\
								\
  u.f = (IN);							\
  (OUT)[0] = (u.l >> 32) & 0xFFFFFFFF;				\
  (OUT)[1] = (u.l & 0xFFFFFFFF);				\
} while (0)

#endif

#undef NM_FLAGS

#undef OBJECT_FORMAT_COFF

/* We cannot generate debugging information on Unicos/Mk.  */

#undef SDB_DEBUGGING_INFO
#undef MIPS_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef DWARF_DEBUGGING_INFO
#undef DWARF2_DEBUGGING_INFO
#undef DWARF2_UNWIND_INFO
#undef INCOMING_RETURN_ADDR_RTX


/* We use the functions provided by the system library for integer
   division.  */

#undef UDIVDI3_LIBCALL
#undef DIVDI3_LIBCALL
#define UDIVDI3_LIBCALL	"$uldiv"
#define DIVDI3_LIBCALL "$sldiv"

/* This is necessary to prevent gcc from generating calls to __divsi3.  */

#define INIT_TARGET_OPTABS					\
  do {								\
    sdiv_optab->handlers[(int) SImode].libfunc = NULL_RTX;	\
    udiv_optab->handlers[(int) SImode].libfunc = NULL_RTX;	\
  } while (0)

#undef ASM_OUTPUT_SOURCE_LINE

/* We don't need a start file.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

/* These are the libraries we have to link with.
   ??? The Craylibs directory should be autoconfed.  */
#undef LIB_SPEC
#define LIB_SPEC "-L/opt/ctl/craylibs/craylibs -lu -lm -lc -lsma"

#undef BUILD_VA_LIST_TYPE
#undef EXPAND_BUILTIN_VA_START
#undef EXPAND_BUILTIN_VA_ARG

#define EH_FRAME_IN_DATA_SECTION 1
