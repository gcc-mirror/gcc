/* Definitions of target machine for GNU compiler.  MIPS version.
   Contributed by   A. Lichnewsky,	lich@inria.inria.fr
   Changed by Michael Meissner,		meissner@osf.org
   Copyright (C) 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

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


/* Make Saber happier on obstack.[ch].  */
#if defined(__mips__) || defined(mips)
#define __PTR_TO_INT(P) ((int)(P))
#define __INT_TO_PTR(P) ((char *)(P))
#endif

/* Standard GCC variables that we reference.  */

extern char    *asm_file_name;
extern char	call_used_regs[];
extern int	current_function_calls_alloca;
extern int	flag_omit_frame_pointer;
extern int	frame_pointer_needed;
extern char    *language_string;
extern int	may_call_alloca;
extern int	optimize;
extern char   **save_argv;
extern int	target_flags;
extern char    *version_string;

/* MIPS external variables defined in mips.c.  */

/* comparison type */
enum cmp_type {
  CMP_SI,				/* compare integers */
  CMP_SF,				/* compare single precision floats */
  CMP_DF,				/* compare double precision floats */
  CMP_MAX				/* max comparison type */
};

/* types of delay slot */
enum delay_type {
  DELAY_NONE,				/* no delay slot */
  DELAY_LOAD,				/* load from memory delay */
  DELAY_HILO,				/* move from/to hi/lo registers */
  DELAY_FCMP				/* delay after doing c.<xx>.{d,s} */
};

/* Which processor to schedule for.  Since there is no difference between
   a R2000 and R3000 in terms of the scheduler, we collapse them into
   just an R3000.  The elements of the enumeration must match exactly
   the cpu attribute in the mips.md machine description.  */

enum processor_type {
  PROCESSOR_DEFAULT,
  PROCESSOR_R3000,
  PROCESSOR_R6000,
  PROCESSOR_R4000
};

/* Recast the cpu class to be the cpu attribute.  */
#define mips_cpu_attr ((enum attr_cpu)mips_cpu)

/* Which type of block move to do (whether or not the last store is
   split out so it can fill a branch delay slot).  */

enum block_move_type {
  BLOCK_MOVE_NORMAL,			/* generate complete block move */
  BLOCK_MOVE_NOT_LAST,			/* generate all but last store */
  BLOCK_MOVE_LAST			/* generate just the last store */
};

extern char mips_reg_names[][8];	/* register names (a0 vs. $4). */
extern char mips_print_operand_punct[];	/* print_operand punctuation chars */
extern char *current_function_name;	/* current function being compiled */
extern char *current_function_file;	/* filename current function is in */
extern int num_source_filenames;	/* current .file # */
extern int inside_function;		/* != 0 if inside of a function */
extern int ignore_line_number;		/* != 0 if we are to ignore next .loc */
extern int file_in_function_warning;	/* warning given about .file in func */
extern int sdb_label_count;		/* block start/end next label # */
extern int mips_section_threshold;	/* # bytes of data/sdata cutoff */
extern int g_switch_value;		/* value of the -G xx switch */
extern int g_switch_set;		/* whether -G xx was passed.  */
extern int sym_lineno;			/* sgi next label # for each stmt */
extern int set_noreorder;		/* # of nested .set noreorder's  */
extern int set_nomacro;			/* # of nested .set nomacro's  */
extern int set_noat;			/* # of nested .set noat's  */
extern int set_volatile;		/* # of nested .set volatile's  */
extern int mips_branch_likely;		/* emit 'l' after br (branch likely) */
extern int mips_dbx_regno[];		/* Map register # to debug register # */
extern char mips_rtx_classify[];	/* classify an RTX code */
extern struct rtx_def *branch_cmp[2];	/* operands for compare */
extern enum cmp_type branch_type;	/* what type of branch to use */
extern enum processor_type mips_cpu;	/* which cpu are we scheduling for */
extern int mips_isa;			/* architectural level */
extern char *mips_cpu_string;		/* for -mcpu=<xxx> */
extern char *mips_isa_string;		/* for -mips{1,2,3} */
extern int dslots_load_total;		/* total # load related delay slots */
extern int dslots_load_filled;		/* # filled load delay slots */
extern int dslots_jump_total;		/* total # jump related delay slots */
extern int dslots_jump_filled;		/* # filled jump delay slots */
extern int dslots_number_nops;		/* # of nops needed by previous insn */
extern int num_refs[3];			/* # 1/2/3 word references */
extern struct rtx_def *mips_load_reg;	/* register to check for load delay */
extern struct rtx_def *mips_load_reg2;	/* 2nd reg to check for load delay */
extern struct rtx_def *mips_load_reg3;	/* 3rd reg to check for load delay */
extern struct rtx_def *mips_load_reg4;	/* 4th reg to check for load delay */

/* Functions within mips.c that we reference.  */

extern void		abort_with_insn ();
extern int		arith32_operand ();
extern int		arith_operand ();
extern int		cmp_op ();
extern int		cmp2_op ();
extern long		compute_frame_size ();
extern int		epilogue_reg_mentioned_p ();
extern void		expand_block_move ();
extern int		equality_op ();
extern int		fcmp_op ();
extern void		final_prescan_insn ();
extern int		fpsw_register_operand ();
extern struct rtx_def *	function_arg ();
extern void		function_arg_advance ();
extern int		function_arg_partial_nregs ();
extern void		function_epilogue ();
extern void		function_prologue ();
extern void		gen_conditional_branch ();
extern struct rtx_def * gen_int_relational ();
extern void		init_cumulative_args ();
extern int		large_int ();
extern int		md_register_operand ();
extern int		mips_address_cost ();
extern void		mips_asm_file_end ();
extern void		mips_asm_file_start ();
extern int		mips_const_double_ok ();
extern void		mips_count_memory_refs ();
extern int		mips_debugger_offset ();
extern void		mips_declare_object ();
extern int		mips_epilogue_delay_slots ();
extern void		mips_expand_epilogue ();
extern void		mips_expand_prologue ();
extern char	       *mips_fill_delay_slot ();
extern char	       *mips_move_1word ();
extern char	       *mips_move_2words ();
extern void		mips_output_double ();
extern int		mips_output_external ();
extern void		mips_output_float ();
extern void		mips_output_filename ();
extern void		mips_output_lineno ();
extern char	       *output_block_move ();
extern void		override_options ();
extern int		pc_or_label_operand ();
extern void		print_operand_address ();
extern void		print_operand ();
extern void		print_options ();
extern int		reg_or_0_operand ();
extern int		simple_epilogue_p ();
extern int		simple_memory_operand ();
extern int		small_int ();
extern void		trace();
extern int		uns_arith_operand ();
extern int		uns_cmp_op ();

/* Recognition functions that return if a condition is true.  */
extern int		address_operand ();
extern int		const_double_operand ();
extern int		const_int_operand ();
extern int		general_operand ();
extern int		immediate_operand ();
extern int		memory_address_p ();
extern int		memory_operand ();
extern int		nonimmediate_operand ();
extern int		nonmemory_operand ();
extern int		register_operand ();
extern int		scratch_operand ();

/* Functions to change what output section we are using.  */
extern void		data_section ();
extern void		rdata_section ();
extern void		readonly_data_section ();
extern void		sdata_section ();
extern void		text_section ();

/* Functions in the rest of the compiler that we reference.  */
extern void		abort_with_insn ();
extern void		debug_rtx ();
extern void		fatal_io_error ();
extern int		get_frame_size ();
extern int		offsettable_address_p ();
extern void		output_address ();
extern char	       *permalloc ();
extern int		reg_mentioned_p ();

/* Functions in the standard library that we reference.  */
extern void		abort ();
extern int		atoi ();
extern char	       *getenv ();
extern char	       *mktemp ();


/* Stubs for half-pic support if not OSF/1 reference platform.  */

#ifndef HALF_PIC_P
#define HALF_PIC_P() 0
#define HALF_PIC_NUMBER_PTRS 0
#define HALF_PIC_NUMBER_REFS 0
#define HALF_PIC_ENCODE(DECL)
#define HALF_PIC_DECLARE(NAME)
#define HALF_PIC_INIT()	error ("half-pic init called on systems that don't support it.")
#define HALF_PIC_ADDRESS_P(X) 0
#define HALF_PIC_PTR(X) X
#define HALF_PIC_FINISH(STREAM)
#endif


/* Switch  Recognition by gcc.c.  Add -G xx support */

#ifdef SWITCH_TAKES_ARG
#undef SWITCH_TAKES_ARG
#endif

#define SWITCH_TAKES_ARG(CHAR)						\
  ((CHAR) == 'D' || (CHAR) == 'U' || (CHAR) == 'o'			\
   || (CHAR) == 'e' || (CHAR) == 'T' || (CHAR) == 'u'			\
   || (CHAR) == 'I' || (CHAR) == 'm'					\
   || (CHAR) == 'L' || (CHAR) == 'A' || (CHAR) == 'G')

/* Sometimes certain combinations of command options do not make sense
   on a particular target machine.  You can define a macro
   `OVERRIDE_OPTIONS' to take account of this.  This macro, if
   defined, is executed once just after all the command options have
   been parsed.

   On the MIPS, it is used to handle -G.  We also use it to set up all
   of the tables referenced in the other macros.  */

#define OVERRIDE_OPTIONS override_options ()

/* Zero or more C statements that may conditionally modify two
   variables `fixed_regs' and `call_used_regs' (both of type `char
   []') after they have been initialized from the two preceding
   macros.

   This is necessary in case the fixed or call-clobbered registers
   depend on target flags.

   You need not define this macro if it has no work to do.

   If the usage of an entire class of registers depends on the target
   flags, you may indicate this to GCC by using this macro to modify
   `fixed_regs' and `call_used_regs' to 1 for each of the registers in
   the classes which should not be used by GCC.  Also define the macro
   `REG_CLASS_FROM_LETTER' to return `NO_REGS' if it is called with a
   letter for a class that shouldn't be used.

   (However, if this class is not included in `GENERAL_REGS' and all
   of the insn patterns whose constraints permit this class are
   controlled by target switches, then GCC will automatically avoid
   using these registers when the target switches are opposed to
   them.)  */

#define CONDITIONAL_REGISTER_USAGE					\
do									\
  {									\
    if (!TARGET_HARD_FLOAT)						\
      {									\
	int regno;							\
									\
	for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)	\
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
  }									\
while (0)


/* Some machines may desire to change what optimizations are
   performed for various optimization levels.   This macro, if
   defined, is executed once just after the optimization level is
   determined and before the remainder of the command options have
   been parsed.  Values set in this macro are used as the default
   values for the other command line options.

   LEVEL is the optimization level specified; 2 if -O2 is
   specified, 1 if -O is specified, and 0 if neither is specified.  */

#define OPTIMIZATION_OPTIONS(LEVEL)					\
{									\
  flag_no_function_cse			= TRUE;				\
  flag_gnu_linker			= FALSE;			\
									\
  if (LEVEL)								\
    {									\
      flag_omit_frame_pointer		= TRUE;				\
      flag_delayed_branch		= TRUE;				\
      flag_thread_jumps			= TRUE;				\
      flag_schedule_insns_after_reload	= TRUE;				\
    }									\
									\
  if (LEVEL >= 2)							\
    {									\
      flag_strength_reduce		= TRUE;				\
      flag_cse_follow_jumps		= TRUE;				\
      flag_expensive_optimizations	= TRUE;				\
      flag_rerun_cse_after_loop		= TRUE;				\
      flag_schedule_insns		= TRUE;				\
    }									\
									\
  if (LEVEL >= 3)							\
    {									\
      flag_inline_functions		= TRUE;				\
    }									\
}



/* Complain about missing specs and predefines that should be defined in each
   of the target tm files to override the defaults.  This is mostly a place-
   holder until I can get each of the files updated [mm].  */

#if defined(OSF_OS) \
    || defined(DECSTATION) \
    || defined(SGI_TARGET) \
    || defined(MIPS_NEWS) \
    || defined(MIPS_SYSV) \
    || defined(MIPS_SVR4) \
    || defined(MIPS_BSD43)

#ifndef CPP_PREDEFINES
	#error "Define CPP_PREDEFINES in the appropriate tm.h file"
#endif

#ifndef CPP_SPEC
	#error "Define CPP_SPEC in the appropriate tm.h file"
#endif

#ifndef LINK_SPEC
	#error "Define LINK_SPEC in the appropriate tm.h file"
#endif

#ifndef LIB_SPEC
	#error "Define LIB_SPEC in the appropriate tm.h file"
#endif

#ifndef STARTFILE_SPEC
	#error "Define STARTFILE_SPEC in the appropriate tm.h file"
#endif

#ifndef MACHINE_TYPE
	#error "Define MACHINE_TYPE in the appropriate tm.h file"
#endif
#endif

/* Tell collect what flags to pass to nm.  */
#ifndef NM_FLAGS
#define NM_FLAGS "-Bp"
#endif


/* Names to predefine in the preprocessor for this target machine.  */

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DSYSTYPE_BSD43 \
-D_mips -D_unix -D_host_mips -D_MIPSEB -D_R3000 -D_SYSTYPE_BSD43"
#endif

/* Extra switches sometimes passed to the assembler.  */

#ifndef ASM_SPEC
#define ASM_SPEC "\
%{!mgas: \
	%{!mrnames: %{!.s:-nocpp} %{.s: %{cpp} %{nocpp}}} \
	%{pipe: %e-pipe is not supported.} \
	%{EB} %{!EB:-EB} \
	%{EL: %e-EL not supported} \
	%{mips1} %{mips2} %{mips3} \
	%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3} \
	%{g} %{g0} %{g1} %{g2} %{g3} %{v} %{K}} \
%{G*}"

#endif				/* ASM_SPEC */

/* Specify to run a post-processor, mips-tfile after the assembler
   has run to stuff the mips debug information into the object file.
   This is needed because the $#!%^ MIPS assembler provides no way
   of specifying such information in the assembly file.  If we are
   cross compiling, disable mips-tfile unless the user specifies
   -mmips-tfile.  */

#ifndef ASM_FINAL_SPEC
#ifndef CROSS_COMPILE
#define ASM_FINAL_SPEC "\
%{!mgas: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#else				/* CROSS_COMPILE */
#define ASM_FINAL_SPEC "\
%{!mgas: %{mmips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#endif	/* CROSS_COMPILE */
#endif	/* ASM_FINAL_SPEC */

/* Redefinition of libraries used.  Mips doesn't support normal
   UNIX style profiling via calling _mcount.  It does offer
   profiling that samples the PC, so do what we can... */

#ifndef LIB_SPEC
#define LIB_SPEC "%{pg:-lprof1} %{p:-lprof1} -lc"
#endif

/* Extra switches sometimes passed to the linker.  */

#ifndef LINK_SPEC
#define LINK_SPEC "\
%{G*} \
%{!mgas: \
	%{pipe: %e-pipe is not supported.} \
	%{EB} %{!EB:-EB} \
	%{EL: %e-EL not supported} \
	%{mips1} %{mips2} %{mips3} \
	%{bestGnum} %{shared} %{non_shared}}"
#endif				/* LINK_SPEC defined */

/* Specs for the compiler proper */

#ifndef CC1_SPEC
#define CC1_SPEC "\
%{O*: %{!mno-gpOPT:%{!mno-gpopt: -mgpopt}}} \
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{G*} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{save-temps: }"
#endif

/* Preprocessor specs */

#ifndef CPP_SPEC
#define CPP_SPEC "\
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C -D_LANGUAGE_OBJECTIVE_C} \
%{.S:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{!.S:	-D__LANGUAGE_C -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}"
#endif

/* If defined, this macro is an additional prefix to try after
   `STANDARD_EXEC_PREFIX'.  */

#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/lib/cmplrs/cc/"
#endif

#ifndef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/lib/cmplrs/cc/"
#endif


/* Print subsidiary information on the compiler version in use.  */

#define MIPS_VERSION "[AL 1.1, MM 33]"

#ifndef MACHINE_TYPE
#define MACHINE_TYPE "BSD Mips"
#endif

#ifndef TARGET_VERSION_INTERNAL
#define TARGET_VERSION_INTERNAL(STREAM)					\
  fprintf (STREAM, " %s %s", MIPS_VERSION, MACHINE_TYPE)
#endif

#ifndef TARGET_VERSION
#define TARGET_VERSION TARGET_VERSION_INTERNAL (stderr)
#endif


#define SDB_DEBUGGING_INFO		/* generate info for mips-tfile */
#define DBX_DEBUGGING_INFO		/* generate stabs (OSF/rose) */
#define MIPS_DEBUGGING_INFO		/* MIPS specific debugging info */

#ifndef PREFERRED_DEBUGGING_TYPE	/* assume SDB_DEBUGGING_INFO */
#define PREFERRED_DEBUGGING_TYPE ((len > 1 && !strncmp (str, "ggdb", len)) ? DBX_DEBUG : SDB_DEBUG)
#endif

/* By default, turn on GDB extensions.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* If we are passing smuggling stabs through the MIPS ECOFF object
   format, put a comment in front of the .stab<x> operation so
   that the MIPS assembler does not choke.  The mips-tfile program
   will correctly put the stab into the object file.  */

#define ASM_STABS_OP	((TARGET_GAS) ? ".stabs" : " #.stabs")
#define ASM_STABN_OP	((TARGET_GAS) ? ".stabn" : " #.stabn")
#define ASM_STABD_OP	((TARGET_GAS) ? ".stabd" : " #.stabd")

/* Forward references to tags are allowed.  */
#define SDB_ALLOW_FORWARD_REFERENCES

/* Unknown tags are also allowed.  */
#define SDB_ALLOW_UNKNOWN_REFERENCES

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#define DBX_CONTIN_LENGTH 1500


/* How to renumber registers for dbx and gdb. */
#define DBX_REGISTER_NUMBER(REGNO) mips_dbx_regno[ (REGNO) ]


/* Overrides for the COFF debug format.  */
#define PUT_SDB_SCL(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.scl\t%d;", (a));	\
} while (0)

#define PUT_SDB_INT_VAL(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.val\t%d;", (a));	\
} while (0)

#define PUT_SDB_VAL(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fputs ("\t.val\t", asm_out_text_file);		\
  output_addr_const (asm_out_text_file, (a));		\
  fputc (';', asm_out_text_file);			\
} while (0)

#define PUT_SDB_DEF(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t#.def\t");		\
  ASM_OUTPUT_LABELREF (asm_out_text_file, a); 		\
  fputc (';', asm_out_text_file);			\
} while (0)

#define PUT_SDB_PLAIN_DEF(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t#.def\t.%s;", (a));	\
} while (0)

#define PUT_SDB_ENDEF					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.endef\n");		\
} while (0)

#define PUT_SDB_TYPE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.type\t0x%x;", (a));	\
} while (0)

#define PUT_SDB_SIZE(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.size\t%d;", (a));	\
} while (0)

#define PUT_SDB_DIM(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.dim\t%d;", (a));	\
} while (0)

#ifndef PUT_SDB_START_DIM
#define PUT_SDB_START_DIM				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.dim\t");		\
} while (0)
#endif

#ifndef PUT_SDB_NEXT_DIM
#define PUT_SDB_NEXT_DIM(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "%d,", a);		\
} while (0)
#endif

#ifndef PUT_SDB_LAST_DIM
#define PUT_SDB_LAST_DIM(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "%d;", a);		\
} while (0)
#endif

#define PUT_SDB_TAG(a)					\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t.tag\t");		\
  ASM_OUTPUT_LABELREF (asm_out_text_file, a); 		\
  fputc (';', asm_out_text_file);			\
} while (0)

/* For block start and end, we create labels, so that
   later we can figure out where the correct offset is.
   The normal .ent/.end serve well enough for functions,
   so those are just commented out.  */

#define PUT_SDB_BLOCK_START(LINE)			\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file,				\
	   "$Lb%d:\n\t#.begin\t$Lb%d\t%d\n",		\
	   sdb_label_count,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_BLOCK_END(LINE)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file,				\
	   "$Le%d:\n\t#.bend\t$Le%d\t%d\n",		\
	   sdb_label_count,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_FUNCTION_START(LINE)

#define PUT_SDB_FUNCTION_END(LINE)

#define PUT_SDB_EPILOGUE_END(NAME)

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), ".%dfake", (NUMBER));

/* Correct the offset of automatic variables and arguments.  Note that
   the MIPS debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the MIPS 3.0 linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  */

#define DEBUGGER_AUTO_OFFSET(X)		mips_debugger_offset (X, 0)
#define DEBUGGER_ARG_OFFSET(OFFSET, X)	mips_debugger_offset (X, OFFSET)


/* Tell collect that the object format is ECOFF */
#ifndef OBJECT_FORMAT_ROSE
#define OBJECT_FORMAT_COFF	/* Object file looks like COFF */
#define EXTENDED_COFF		/* ECOFF, not normal coff */
#endif

/* Don't use the default definitions, because we don't have gld.
   Also, we don't want stabs when generating ECOFF output.
   Instead we depend on collect to handle these.  */

#define ASM_OUTPUT_CONSTRUCTOR(file, name)
#define ASM_OUTPUT_DESTRUCTOR(file, name)


/* Run-time compilation parameters selecting different hardware subsets.  */

/* Macros used in the machine description to test the flags.  */

					/* Bits for real switches */
#define MASK_INT64	0x00000001	/* ints are 64 bits */
#define MASK_LONG64	0x00000002	/* longs are 64 bits */
#define MASK_LLONG128	0x00000004	/* long longs are 128 bits */
#define MASK_GPOPT	0x00000008	/* Optimize for global pointer */
#define MASK_GAS	0x00000010	/* Gas used instead of MIPS as */
#define MASK_NAME_REGS	0x00000020	/* Use MIPS s/w reg name convention */
#define MASK_STATS	0x00000040	/* print statistics to stderr */
#define MASK_MEMCPY	0x00000080	/* call memcpy instead of inline code*/
#define MASK_SOFT_FLOAT	0x00000100	/* software floating point */
#define MASK_FLOAT64	0x00000200	/* fp registers are 64 bits */
#define MASK_ABICALLS	0x00000400	/* emit .abicalls/.cprestore/.cpload */
#define MASK_HALF_PIC	0x00000800	/* Emit OSF-style pic refs to externs*/
#define MASK_UNUSED1	0x00001000
#define MASK_UNUSED2	0x00002000
#define MASK_UNUSED3	0x00004000
#define MASK_UNUSED4	0x00008000
#define MASK_UNUSED5	0x00010000
#define MASK_UNUSED6	0x00020000
#define MASK_UNUSED7	0x00040000
#define MASK_UNUSED8	0x00080000

					/* Dummy switches used only in spec's*/
#define MASK_MIPS_TFILE	0x00000000	/* flag for mips-tfile usage */

					/* switches not used yet */
#define MASK_WC8	0x00000000	/* wchar's are  8 bits, not 32 */
#define MASK_WC16	0x00000000	/* wchar's are 16 bits, not 32 */
#define MASK_WC32	0x00000000	/* dummy for consistency */

					/* Debug switches, not documented */
#define MASK_DEBUG	0x40000000	/* Eliminate version # in .s file */
#define MASK_DEBUG_A	0x20000000	/* don't allow <label>($reg) addrs */
#define MASK_DEBUG_B	0x10000000	/* GO_IF_LEGITIMATE_ADDRESS debug */
#define MASK_DEBUG_C	0x08000000	/* don't expand seq, etc. */
#define MASK_DEBUG_D	0x04000000	/* don't do define_split's */
#define MASK_DEBUG_E	0x02000000	/* function_arg debug */
#define MASK_DEBUG_F	0x01000000	/* don't try to suppress load nop's */
#define MASK_DEBUG_G	0x00800000	/* don't support 64 bit arithmetic */
#define MASK_DEBUG_H	0x00400000	/* allow ints in FP registers */
#define MASK_DEBUG_I	0x00200000	/* unused */
#define MASK_DEBUG_J	0x00100000	/* unused */

					/* r4000 64 bit sizes */
#define TARGET_INT64		(target_flags & MASK_INT64)
#define TARGET_LONG64		(target_flags & MASK_LONG64)
#define TARGET_LLONG128		(target_flags & MASK_LLONG128)
#define TARGET_FLOAT64		(target_flags & MASK_FLOAT64)

					/* Mips vs. GNU assembler */
#define TARGET_GAS		(target_flags & MASK_GAS)
#define TARGET_UNIX_ASM		(!TARGET_GAS)
#define TARGET_MIPS_AS		TARGET_UNIX_ASM

					/* Debug Mode */
#define TARGET_DEBUG_MODE	(target_flags & MASK_DEBUG)
#define TARGET_DEBUG_A_MODE	(target_flags & MASK_DEBUG_A)
#define TARGET_DEBUG_B_MODE	(target_flags & MASK_DEBUG_B)
#define TARGET_DEBUG_C_MODE	(target_flags & MASK_DEBUG_C)
#define TARGET_DEBUG_D_MODE	(target_flags & MASK_DEBUG_D)
#define TARGET_DEBUG_E_MODE	(target_flags & MASK_DEBUG_E)
#define TARGET_DEBUG_F_MODE	(target_flags & MASK_DEBUG_F)
#define TARGET_DEBUG_G_MODE	(target_flags & MASK_DEBUG_G)
#define TARGET_DEBUG_H_MODE	(target_flags & MASK_DEBUG_H)
#define TARGET_DEBUG_I_MODE	(target_flags & MASK_DEBUG_I)
#define TARGET_DEBUG_J_MODE	(target_flags & MASK_DEBUG_J)

					/* Reg. Naming in .s ($21 vs. $a0) */
#define TARGET_NAME_REGS	(target_flags & MASK_NAME_REGS)

					/* Optimize for Sdata/Sbss */
#define TARGET_GP_OPT		(target_flags & MASK_GPOPT)

					/* print program statistics */
#define TARGET_STATS		(target_flags & MASK_STATS)

					/* call memcpy instead of inline code */
#define TARGET_MEMCPY		(target_flags & MASK_MEMCPY)

					/* .abicalls, etc from Pyramid V.4 */
#define TARGET_ABICALLS		(target_flags & MASK_ABICALLS)

					/* OSF pic references to externs */
#define TARGET_HALF_PIC		(target_flags & MASK_HALF_PIC)

					/* wchar size */
#define TARGET_WC8		(target_flags & MASK_WC8)
#define TARGET_WC16		(target_flags & MASK_WC16)
#define TARGET_WC32		((target_flags & (MASK_WC8 | MASK_WC16)) == 0)

					/* software floating point */
#define TARGET_SOFT_FLOAT	(target_flags & MASK_SOFT_FLOAT)
#define TARGET_HARD_FLOAT	(! TARGET_SOFT_FLOAT)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
{									\
  {"int64",		  MASK_INT64 | MASK_LONG64},			\
  {"long64",		  MASK_LONG64},					\
  {"longlong128",	  MASK_INT64 | MASK_LONG64 | MASK_LLONG128},	\
  {"mips-as",		 -MASK_GAS},					\
  {"gas",		  MASK_GAS},					\
  {"rnames",		  MASK_NAME_REGS},				\
  {"no-rnames",		 -MASK_NAME_REGS},				\
  {"gpOPT",		  MASK_GPOPT},					\
  {"gpopt",		  MASK_GPOPT},					\
  {"no-gpOPT",		 -MASK_GPOPT},					\
  {"no-gpopt",		 -MASK_GPOPT},					\
  {"stats",		  MASK_STATS},					\
  {"no-stats",		 -MASK_STATS},					\
  {"memcpy",		  MASK_MEMCPY},					\
  {"no-memcpy",		 -MASK_MEMCPY},					\
  {"wc8",		  MASK_WC8},					\
  {"wc16",		  MASK_WC16},					\
  {"wc32",		  MASK_WC32},					\
  {"mips-tfile",	  MASK_MIPS_TFILE},				\
  {"no-mips-tfile",	 -MASK_MIPS_TFILE},				\
  {"soft-float",	  MASK_SOFT_FLOAT},				\
  {"hard-float",	 -MASK_SOFT_FLOAT},				\
  {"fp64",		  MASK_FLOAT64},				\
  {"fp32",		 -MASK_FLOAT64},				\
  {"abicalls",		  MASK_ABICALLS},				\
  {"no-abicalls",	 -MASK_ABICALLS},				\
  {"half-pic",		  MASK_HALF_PIC},				\
  {"no-half-pic",	 -MASK_HALF_PIC},				\
  {"debug",		  MASK_DEBUG},					\
  {"debuga",		  MASK_DEBUG_A},				\
  {"debugb",		  MASK_DEBUG_B},				\
  {"debugc",		  MASK_DEBUG_C},				\
  {"debugd",		  MASK_DEBUG_D},				\
  {"debuge",		  MASK_DEBUG_E},				\
  {"debugf",		  MASK_DEBUG_F},				\
  {"debugg",		  MASK_DEBUG_G},				\
  {"debugh",		  MASK_DEBUG_H},				\
  {"debugi",		  MASK_DEBUG_I},				\
  {"debugj",		  MASK_DEBUG_J},				\
  {"",			  TARGET_DEFAULT}				\
}

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

/* This macro is similar to `TARGET_SWITCHES' but defines names of
   command options that have values.  Its definition is an
   initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   fixed part of the option name, and the address of a variable. 
   The variable, type `char *', is set to the variable part of the
   given option if the fixed part matches.  The actual option name
   is made by appending `-m' to the specified name.

   Here is an example which defines `-mshort-data-NUMBER'.  If the
   given option is `-mshort-data-512', the variable `m88k_short_data'
   will be set to the string `"512"'.

	extern char *m88k_short_data;
	#define TARGET_OPTIONS { { "short-data-", &m88k_short_data } }  */

#define TARGET_OPTIONS							\
{									\
  { "cpu=",	&mips_cpu_string	},				\
  { "ips",	&mips_isa_string	}				\
}

/* Macros to decide whether certain features are available or not,
   depending on the instruction set architecture level.  */

#define BRANCH_LIKELY_P()	(mips_isa >= 2)
#define HAVE_64BIT_P()		(mips_isa >= 3)
#define HAVE_SQRT_P()		(mips_isa >= 2)


/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
*/
/* #define BITS_BIG_ENDIAN */

/* Define this if most significant byte of a word is the lowest numbered. */
#ifndef BYTES_BIG_ENDIAN
#ifndef DECSTATION
#define BYTES_BIG_ENDIAN 1
#else
#define BYTES_BIG_ENDIAN 0
#endif
#endif

/* Define this if most significant word of a multiword number is the lowest. */
#ifndef WORDS_BIG_ENDIAN
#ifndef DECSTATION
#define WORDS_BIG_ENDIAN 1
#else
#define WORDS_BIG_ENDIAN 0
#endif
#endif

/* Define macros to easily access the most and least significant words
   without a lot of #ifdef's.  */

#if WORDS_BIG_ENDIAN
#define MOST_SIGNIFICANT_WORD	0
#define LEAST_SIGNIFICANT_WORD	1

#else
#define MOST_SIGNIFICANT_WORD	1
#define LEAST_SIGNIFICANT_WORD	0
#endif

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* A C expression for the size in bits of the type `int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE 32

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* A C expression for the size in bits of the type `char' on the
   target machine.  If you don't define this, the default is one
   quarter of a word.  (If this would be less than one storage unit,
   it is rounded up to one unit.)  */
#define CHAR_TYPE_SIZE BITS_PER_UNIT

/* A C expression for the size in bits of the type `float' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define FLOAT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `double' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define DOUBLE_TYPE_SIZE 64

/* A C expression for the size in bits of the type `long double' on
   the target machine.  If you don't define this, the default is two
   words.  */
#define LONG_DOUBLE_TYPE_SIZE 64

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
/* 8 is observed right on a DECstation and on riscos 4.02.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 64

/* Biggest alignment any structure field can require in bits.  */
#define BIGGEST_FIELD_ALIGNMENT 64

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Define this if you wish to imitate the way many other C compilers
   handle alignment of bitfields and the structures that contain
   them.

   The behavior is that the type written for a bitfield (`int',
   `short', or other integer type) imposes an alignment for the
   entire structure, as if the structure really did contain an
   ordinary field of that type.  In addition, the bitfield is placed
   within the structure so that it would fit within such a field,
   not crossing a boundary for it.

   Thus, on most machines, a bitfield whose type is written as `int'
   would not cross a four-byte boundary, and would force four-byte
   alignment for the whole structure.  (The alignment used may not
   be four bytes; it is controlled by the other alignment
   parameters.)

   If the macro is defined, its definition should be a C expression;
   a nonzero value for the expression enables this behavior.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* If defined, a C expression to compute the alignment given to a
   constant that is being placed in memory.  CONSTANT is the constant
   and ALIGN is the alignment that the object would ordinarily have.
   The value of this macro is used instead of that alignment to align
   the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string
   constants to be word aligned so that `strcpy' calls that copy
   constants can be done inline.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN)					\
  ((TREE_CODE (EXP) == STRING_CST  || TREE_CODE (EXP) == CONSTRUCTOR)	\
   && (ALIGN) < BITS_PER_WORD						\
	? BITS_PER_WORD							\
	: (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* Define this macro if an argument declared as `char' or `short' in a
   prototype should actually be passed as an `int'.  In addition to
   avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines. */

#define PROMOTE_PROTOTYPES

/* Define this macro if an instruction to load a value narrower
   than a word from memory into a register also zero-extends the
   value to the whole  register.  */

#define BYTE_LOADS_ZERO_EXTEND


/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   On the Mips, we have 32 integer registers, 32 floating point registers
   and the special registers hi, lo, and fp status.  */

#define FIRST_PSEUDO_REGISTER 67

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the MIPS, see conventions, page D-2  */

#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 1								\
}


/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */

#define CALL_USED_REGISTERS						\
{									\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 1,			\
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,			\
  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  1, 1, 1								\
}


/* Internal macros to classify a register number as to whether it's a
   general purpose register, a floating point register, a
   multiply/divide register, or a status register.

   The macro FP_CALL_REG_P also allows registers $4 and $6 as floating
   point registers to pass floating point as per MIPS spec. */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)
#define GP_DBX_FIRST 0

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)
#define FP_DBX_FIRST ((write_symbols == DBX_DEBUG) ? 38 : 32)

#define MD_REG_FIRST 64
#define MD_REG_LAST  65
#define MD_REG_NUM   (MD_REG_LAST - MD_REG_FIRST + 1)

#define ST_REG_FIRST 66
#define ST_REG_LAST  66
#define ST_REG_NUM   (ST_REG_LAST - ST_REG_FIRST + 1)

#define AT_REGNUM	(GP_REG_FIRST + 1)
#define HI_REGNUM	(MD_REG_FIRST + 0)
#define LO_REGNUM	(MD_REG_FIRST + 1)
#define FPSW_REGNUM	ST_REG_FIRST

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define FP_REG_P(REGNO) ((unsigned) ((REGNO) - FP_REG_FIRST) < FP_REG_NUM)
#define MD_REG_P(REGNO) ((unsigned) ((REGNO) - MD_REG_FIRST) < MD_REG_NUM)
#define ST_REG_P(REGNO) ((REGNO) == ST_REG_FIRST)

#define FP_CALL_REG_P(REGNO)					\
  (FP_REG_P (REGNO)						\
   || (REGNO) == (4 + GP_REG_FIRST)				\
   || (REGNO) == (6 + GP_REG_FIRST))

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the MIPS, all general registers are one word long.  Except on
   the R4000 with the FR bit set, the floating point uses register
   pairs, with the second register not being allocatable.  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (! FP_REG_P (REGNO)							\
	? ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD) \
	: (((GET_MODE_SIZE (MODE) + (2*UNITS_PER_WORD) - 1) / (2*UNITS_PER_WORD)) \
		<< (TARGET_FLOAT64 == 0)))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  Require that DImode and DFmode be in even registers.  For
   DImode, this makes some of the insns easier to write, since you
   don't have to worry about a DImode value in registers 3 & 4,
   producing a result in 4 & 5.

   To make the code simpler HARD_REGNO_MODE_OK now just references an
   array built in override_options.  Because machmodes.h is not yet
   included before this file is processed, the MODE bound can't be
   expressed here.  */

extern char mips_hard_regno_mode_ok[][FIRST_PSEUDO_REGISTER];

#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  mips_hard_regno_mode_ok[ (int)(MODE) ][ (REGNO) ]

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)					\
  ((GET_MODE_CLASS (MODE1) == MODE_FLOAT ||				\
    GET_MODE_CLASS (MODE1) == MODE_COMPLEX_FLOAT)			\
   == (GET_MODE_CLASS (MODE2) == MODE_FLOAT ||				\
       GET_MODE_CLASS (MODE2) == MODE_COMPLEX_FLOAT))

/* MIPS pc is not overloaded on a register.	*/
/* #define PC_REGNUM xx				*/

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM (GP_REG_FIRST + 29)

/* Offset from the stack pointer to the first available location.  */
#define STACK_POINTER_OFFSET 0

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM (GP_REG_FIRST + 30)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED (current_function_calls_alloca)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM GP_REG_FIRST

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_REG_FIRST + 2)

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */
/* #define STRUCT_VALUE_REGNUM (GP_REG_FIRST + 4) */

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */
#define STRUCT_VALUE ((rtx)0)

/* Mips registers used in prologue/epilogue code when the stack frame
   is larger than 32K bytes.  These registers must come from the
   scratch register set, and not used for passing and returning
   arguments and any other information used in the calling sequence
   (such as pic).  */

#define MIPS_TEMP1_REGNUM (GP_REG_FIRST + 8)
#define MIPS_TEMP2_REGNUM (GP_REG_FIRST + 9)

/* Define this macro if it is as good or better to call a constant
   function address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1

/* Define this macro if it is as good or better for a function to
   call itself with an explicit address than to call an address
   kept in a register.  */
#define NO_RECURSIVE_FUNCTION_CSE 1

/* The register number of the register used to address a table of
   static data addresses in memory.  In some cases this register is
   defined by a processor's "application binary interface" (ABI). 
   When this macro is defined, RTL is generated for this register
   once, as with the stack pointer and frame pointer registers.  If
   this macro is not defined, it is up to the machine-dependent
   files to allocate such a register (if necessary).  */
#define PIC_OFFSET_TABLE_REGNUM (GP_REG_FIRST + 28)


/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */

enum reg_class
{
  NO_REGS,			/* no registers in set */
  GR_REGS,			/* integer registers */
  FP_REGS,			/* floating point registers */
  HI_REG,			/* hi register */
  LO_REG,			/* lo register */
  MD_REGS,			/* multiply/divide registers (hi/lo) */
  ST_REGS,			/* status registers (fp status) */
  ALL_REGS,			/* all registers */
  LIM_REG_CLASSES		/* max value + 1 */
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define GENERAL_REGS GR_REGS

/* An initializer containing the names of the register classes as C
   string constants.  These names are used in writing some of the
   debugging dumps.  */

#define REG_CLASS_NAMES							\
{									\
  "NO_REGS",								\
  "GR_REGS",								\
  "FP_REGS",								\
  "HI_REG",								\
  "LO_REG",								\
  "MD_REGS",								\
  "ST_REGS",								\
  "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes,
   as integers which are bit masks.  The Nth integer specifies the
   contents of class N.  The way the integer MASK is interpreted is
   that register R is in the class if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not
   suffice.  Then the integers are replaced by sub-initializers,
   braced groupings containing several integers.  Each
   sub-initializer must be suitable as an initializer for the type
   `HARD_REG_SET' which is defined in `hard-reg-set.h'.  */

#define REG_CLASS_CONTENTS						\
{									\
  { 0x00000000, 0x00000000, 0x00000000 },	/* no registers */	\
  { 0xffffffff, 0x00000000, 0x00000000 },	/* integer registers */	\
  { 0x00000000, 0xffffffff, 0x00000000 },	/* floating registers*/	\
  { 0x00000000, 0x00000000, 0x00000001 },	/* hi register */	\
  { 0x00000000, 0x00000000, 0x00000002 },	/* lo register */	\
  { 0x00000000, 0x00000000, 0x00000003 },	/* mul/div registers */	\
  { 0x00000000, 0x00000000, 0x00000004 },	/* status registers */	\
  { 0xffffffff, 0xffffffff, 0x00000007 }	/* all registers */	\
}


/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */

extern enum reg_class mips_regno_to_class[];

#define REGNO_REG_CLASS(REGNO) mips_regno_to_class[ (REGNO) ]

/* A macro whose definition is the name of the class to which a
   valid base register must belong.  A base register is one used in
   an address which is the register value plus a displacement.  */

#define BASE_REG_CLASS  GR_REGS

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS GR_REGS


/* REGISTER AND CONSTANT CLASSES */

/* Get reg_class from a letter such as appears in the machine
   description.

   DEFINED REGISTER CLASSES:

   'd'  General (aka integer) registers
   'f'	Floating point registers
   'h'	Hi register
   'l'	Lo register
   'x'	Multiply/divide registers
   'z'	FP Status register */

extern enum reg_class mips_char_to_class[];

#define REG_CLASS_FROM_LETTER(C) mips_char_to_class[ (C) ]

/* The letters I, J, K, L, M, N, O, and P in a register constraint
   string can be used to stand for particular ranges of immediate
   operands.  This macro defines what the ranges are.  C is the
   letter, and VALUE is a constant value.  Return 1 if VALUE is
   in the range specified by C.  */

/* For MIPS:

   `I'	is used for the range of constants an arithmetic insn can
	actually contain (16 bits signed integers).

   `J'	is used for the range which is just zero (ie, $r0).

   `K'	is used for the range of constants a logical insn can actually
	contain (16 bit zero-extended integers).

   `L'	is used for the range of constants that be loaded with lui
	(ie, the bottom 16 bits are zero).

   `M'	is used for the range of constants that take two words to load
	(ie, not matched by `I', `K', and `L').

   `N'	is used for negative 16 bit constants.

   `O'	is an exact power of 2 (not yet used in the md file).

   `P'	is used for positive 16 bit constants.  */

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x8000) < 0x10000)
#define SMALL_INT_UNSIGNED(X) ((unsigned) (INTVAL (X)) < 0x10000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? ((unsigned) ((VALUE) + 0x8000) < 0x10000)		\
   : (C) == 'J' ? ((VALUE) == 0)					\
   : (C) == 'K' ? ((unsigned) (VALUE) < 0x10000)			\
   : (C) == 'L' ? (((VALUE) & 0xffff0000) == (VALUE))			\
   : (C) == 'M' ? ((((VALUE) & ~0x0000ffff) != 0)			\
		   && (((VALUE) & ~0x0000ffff) != ~0x0000ffff)		\
		   && ((VALUE) & 0x0000ffff) != 0)			\
   : (C) == 'N' ? (((VALUE) & ~0x0000ffff) == ~0x0000ffff)		\
   : (C) == 'O' ? (exact_log2 (VALUE) >= 0)				\
   : (C) == 'P' ? ((VALUE) != 0 && (((VALUE) & ~0x0000ffff) == 0))	\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

/* For Mips

  'G'	: Floating point 0 */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'G'								\
   && CONST_DOUBLE_HIGH (VALUE) == 0					\
   && CONST_DOUBLE_LOW (VALUE) == 0)

/* Letters in the range `Q' through `U' may be defined in a
   machine-dependent fashion to stand for arbitrary operand types. 
   The machine description macro `EXTRA_CONSTRAINT' is passed the
   operand as its first argument and the constraint letter as its
   second operand.

   `Q'	is for memory references which take more than 1 instruction.
   `R'	is for memory references which take 1 word for the instruction.
   `S'	is for references to extern items which are PIC for OSF/rose.  */

#define EXTRA_CONSTRAINT(OP,CODE)					\
  ((GET_CODE (OP) != MEM) ? FALSE					\
   : ((CODE) == 'Q')	  ? !simple_memory_operand (OP, GET_MODE (OP))	\
   : ((CODE) == 'R')	  ? simple_memory_operand (OP, GET_MODE (OP))	\
   : ((CODE) == 'S')	  ? (HALF_PIC_P () && CONSTANT_P (OP)		\
			     && HALF_PIC_ADDRESS_P (OP))		\
   : FALSE)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
  ((GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT				\
     || GET_MODE_CLASS (GET_MODE (X)) == MODE_COMPLEX_FLOAT)		\
	    ? (TARGET_SOFT_FLOAT ? GR_REGS : FP_REGS)			\
	    : ((GET_MODE (X) == VOIDmode)				\
		? GR_REGS						\
		: CLASS))

/* Certain machines have the property that some registers cannot be
   copied to some other registers without using memory.  Define this
   macro on those machines to be a C expression that is non-zero if
   objects of mode MODE in registers of CLASS1 can only be copied to
   registers of class CLASS2 by storing a register of CLASS1 into
   memory and loading that memory location into a register of CLASS2.

   Do not define this macro if its value would always be zero.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE)			\
  (!TARGET_DEBUG_H_MODE							\
   && GET_MODE_CLASS (MODE) == MODE_INT					\
   && ((CLASS1 == FP_REGS && CLASS2 == GR_REGS)				\
       || (CLASS1 == GR_REGS && CLASS2 == FP_REGS)))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)					\
 ((((MODE) == DFmode) || ((MODE) == SFmode)) ? 2			\
  : ((MODE) == VOIDmode)? ((CLASS) == FP_REGS ? 2 : 1)			\
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* If defined, this is a C expression whose value should be
   nonzero if the insn INSN has the effect of mysteriously
   clobbering the contents of hard register number REGNO.  By
   "mysterious" we mean that the insn's RTL expression doesn't
   describe such an effect.

   If this macro is not defined, it means that no insn clobbers
   registers mysteriously.  This is the usual situation; all else
   being equal, it is best for the RTL expression to show all the
   activity.  */

/* #define INSN_CLOBBERS_REGNO_P(INSN, REGNO) */


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET current_function_outgoing_args_size

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.

   The MIPS ABI states that functions which dynamically allocate the
   stack must not have 0 for STACK_DYNAMIC_OFFSET, since it looks like
   we are trying to create a second frame pointer to the function, so
   allocate some stack space to make it happy.

   However, the linker currently complains about linking any code that
   dynamically allocates stack space, and there seems to be a bug in
   STACK_DYNAMIC_OFFSET, so don't define this right now.  */

#if 0
#define STACK_DYNAMIC_OFFSET(FUNDECL)					\
  ((current_function_outgoing_args_size == 0 && current_function_calls_alloca) \
	? 4*UNITS_PER_WORD						\
	: current_function_outgoing_args_size)
#endif

/* Structure to be filled in by compute_frame_size with register
   save masks, and offsets for the current function.  */

struct mips_frame_info
{
  long total_size;		/* # bytes that the entire frame takes up */
  long var_size;		/* # bytes that variables take up */
  long args_size;		/* # bytes that outgoing arguments take up */
  long extra_size;		/* # bytes of extra gunk */
  int  gp_reg_size;		/* # bytes needed to store gp regs */
  int  fp_reg_size;		/* # bytes needed to store fp regs */
  long mask;			/* mask of saved gp registers */
  long fmask;			/* mask of saved fp registers */
  long gp_save_offset;		/* offset from vfp to store gp registers */
  long fp_save_offset;		/* offset from vfp to store fp registers */
  long gp_sp_offset;		/* offset from new sp to store gp registers */
  long fp_sp_offset;		/* offset from new sp to store fp registers */
  int  initialized;		/* != 0 if frame size already calculated */
  int  num_gp;			/* number of gp registers saved */
  int  num_fp;			/* number of fp registers saved */
};

extern struct mips_frame_info current_frame_info;

/* Store in the variable DEPTH the initial difference between the
   frame pointer reg contents and the stack pointer reg contents,
   as of the start of the function body.  This depends on the layout
   of the fixed parts of the stack frame and on how registers are saved.  */

/* #define INITIAL_FRAME_POINTER_OFFSET(VAR)				\
    ((VAR) = compute_frame_size (get_frame_size ())) */

/* If defined, this macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame.  If
   it is not defined, the only elimination attempted by the compiler
   is to replace references to the frame pointer with references to
   the stack pointer.

   The definition of this macro is a list of structure
   initializations, each of which specifies an original and
   replacement register.

   On some machines, the position of the argument pointer is not
   known until the compilation is completed.  In such a case, a
   separate hard register must be used for the argument pointer. 
   This register can be eliminated by replacing it with either the
   frame pointer or the argument pointer, depending on whether or not
   the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack
   pointer is specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   FRAME_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}


/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.  */

#define CAN_ELIMINATE(FROM, TO)						\
  (!frame_pointer_needed						\
   || ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM))

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			 \
{  compute_frame_size (get_frame_size ());				 \
  if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)	 \
    (OFFSET) = 0;							 \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM) \
    (OFFSET) = current_frame_info.total_size;				 \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) \
    (OFFSET) = current_frame_info.total_size;				 \
  else									 \
    abort ();								 \
}


/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the vax, sp@- in a byte insn really pushes a word.  */

/* #define PUSH_ROUNDING(BYTES) 0 */

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   `current_function_outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue
   should increase the stack frame size by this amount.

   It is not proper to define both `PUSH_ROUNDING' and
   `ACCUMULATE_OUTGOING_ARGS'.  */
#define ACCUMULATE_OUTGOING_ARGS

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above
   the first argument's address.

   On the MIPS, we must skip the first argument position if we are
   returning a structure or a union, to account for it's address being
   passed in $4.  However, at the current time, this produces a compiler
   that can't bootstrap, so comment it out for now.  */

#if 0
#define FIRST_PARM_OFFSET(FNDECL)					\
  (FNDECL != 0								\
   && TREE_TYPE (FNDECL) != 0						\
   && TREE_TYPE (TREE_TYPE (FNDECL)) != 0				\
   && (TREE_CODE (TREE_TYPE (TREE_TYPE (FNDECL))) == RECORD_TYPE	\
       || TREE_CODE (TREE_TYPE (TREE_TYPE (FNDECL))) == UNION_TYPE)	\
		? UNITS_PER_WORD					\
		: 0)
#else
#define FIRST_PARM_OFFSET(FNDECL) 0
#endif

/* When a parameter is passed in a register, stack space is still
   allocated for it.  For the MIPS, stack space must be allocated, cf
   Asm Lang Prog Guide page 7-8.

   BEWARE that some space is also allocated for non existing arguments
   in register. In case an argument list is of form GF used registers
   are a0 (a2,a3), but we should push over a1...  */

#define REG_PARM_STACK_SPACE(FNDECL) ((4*UNITS_PER_WORD) - FIRST_PARM_OFFSET (FNDECL))

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers. 
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in 
   `current_function_outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* Align stack frames on 64 bits (Double Word ).  */
#define STACK_BOUNDARY 64

/* Make sure 16 bytes are always allocated on the stack.  */

#ifndef STACK_ARGS_ADJUST
#define STACK_ARGS_ADJUST(SIZE)						\
{									\
  if (SIZE.constant < 16)						\
    SIZE.constant = 16;							\
}
#endif


/* A C expression that should indicate the number of bytes of its
   own arguments that a function function pops on returning, or 0
   if the function pops no arguments and the caller must therefore
   pop them all after the function returns.

   FUNTYPE is a C variable whose value is a tree node that
   describes the function in question.  Normally it is a node of
   type `FUNCTION_TYPE' that describes the data type of the function.
   From this it is possible to obtain the data types of the value
   and arguments (if known).

   When a call to a library function is being considered, FUNTYPE
   will contain an identifier node for the library function.  Thus,
   if you need to distinguish among various library functions, you
   can do so by their names.  Note that "library function" in this
   context means a function used to perform arithmetic, whose name
   is known specially in the compiler and was not mentioned in the
   C code being compiled.

   STACK-SIZE is the number of bytes of arguments passed on the
   stack.  If a variable number of bytes is passed, it is zero, and
   argument popping will always be the responsibility of the
   calling function.  */

#define RETURN_POPS_ARGS(FUNTYPE, SIZE) 0


/* Symbolic macros for the registers used to return integer and floating
   point values.  */

#define GP_RETURN (GP_REG_FIRST + 2)
#define FP_RETURN ((TARGET_SOFT_FLOAT) ? GP_RETURN : (FP_REG_FIRST + 0))

/* Symbolic macros for the first/last argument registers.  */

#define GP_ARG_FIRST (GP_REG_FIRST + 4)
#define GP_ARG_LAST  (GP_REG_FIRST + 7)
#define FP_ARG_FIRST (FP_REG_FIRST + 12)
#define FP_ARG_LAST  (FP_REG_FIRST + 15)

#define MAX_ARGS_IN_REGISTERS	4

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)						\
  gen_rtx (REG, MODE,							\
	   (GET_MODE_CLASS (MODE) == MODE_FLOAT)			\
		? FP_RETURN						\
		: GP_RETURN)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))


/* 1 if N is a possible register number for a function value.
   On the MIPS, R2 R3 and F0 F2 are the only register thus used.
   Currently, R2 and F0 are only implemented  here (C has no complex type)  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == GP_RETURN || (N) == FP_RETURN)

/* 1 if N is a possible register number for function argument passing.  */

#define FUNCTION_ARG_REGNO_P(N) (((N) >= GP_ARG_FIRST && (N) <= GP_ARG_LAST)   \
				 || ((N) >= FP_ARG_FIRST && (N) <= FP_ARG_LAST \
				     && (0 == (N) % 2)))

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type
   `tree', representing the data type of the value.

   Note that values of mode `BLKmode' are returned in memory
   regardless of this macro.  Also, the option `-fpcc-struct-return'
   takes effect regardless of this macro.  On most systems, it is
   possible to leave the macro undefined; this causes a default
   definition to be used, whose value is the constant 0.

   GCC normally converts 1 byte structures into chars, 2 byte
   structs into shorts, and 4 byte structs into ints, and returns
   them this way.  Defining the following macro overrides this,
   to give us MIPS cc compatibility.  */

#define RETURN_IN_MEMORY(TYPE)	\
  ((TREE_CODE (TYPE) == RECORD_TYPE) || (TREE_CODE (TYPE) == UNION_TYPE))


/* A code distinguishing the floating point format of the target
   machine.  There are three defined values: IEEE_FLOAT_FORMAT,
   VAX_FLOAT_FORMAT, and UNKNOWN_FLOAT_FORMAT.  */

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.
*/

typedef struct mips_args {
  int gp_reg_found;		/* whether a gp register was found yet */
  int arg_number;		/* argument number */
  int arg_words;		/* # total words the arguments take */
  int num_adjusts;		/* number of adjustments made */
				/* Adjustments made to args pass in regs.  */
  struct rtx_def *adjust[MAX_ARGS_IN_REGISTERS];
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

*/

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME)			\
  init_cumulative_args (&CUM, FNTYPE, LIBNAME)				\

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
  function_arg_advance (&CUM, MODE, TYPE, NAMED)

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg( &CUM, MODE, TYPE, NAMED)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero. */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED)

/* If defined, a C expression that gives the alignment boundary, in
   bits, of an argument with the specified mode and type.  If it is
   not defined,  `PARM_BOUNDARY' is used for all arguments.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  (((TYPE) != 0)							\
	? ((TYPE_ALIGN(TYPE) <= PARM_BOUNDARY)				\
		? PARM_BOUNDARY						\
		: TYPE_ALIGN(TYPE))					\
	: ((GET_MODE_ALIGNMENT(MODE) <= PARM_BOUNDARY)			\
		? PARM_BOUNDARY						\
		: GET_MODE_ALIGNMENT(MODE)))


/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE) function_prologue(FILE, SIZE)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) function_epilogue(FILE, SIZE)

/* Define the number of delay slots needed for the function epilogue.

   On the mips, we need a slot if either no stack has been allocated,
   or the only register saved is the return register.  */

#define DELAY_SLOTS_FOR_EPILOGUE mips_epilogue_delay_slots ()

/* Define whether INSN can be placed in delay slot N for the epilogue.
   No references to the stack must be made, since on the MIPS, the
   delay slot is done after the stack has been cleaned up.  */

#define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN,N)				\
  (get_attr_dslot (INSN) == DSLOT_NO					\
   && get_attr_length (INSN) == 1					\
   && ! epilogue_reg_mentioned_p (PATTERN (INSN)))

/* Tell prologue and epilogue if register REGNO should be saved / restored.  */

#define MUST_SAVE_REGISTER(regno) \
 ((regs_ever_live[regno] && !call_used_regs[regno])		\
  || (regno == FRAME_POINTER_REGNUM && frame_pointer_needed)	\
  || (regno == (GP_REG_FIRST + 31) && regs_ever_live[GP_REG_FIRST + 31]))

/* ALIGN FRAMES on double word boundaries */

#define MIPS_STACK_ALIGN(LOC) (((LOC)+7) & ~7)


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  fprintf (FILE, "\t.set\tnoreorder\n");				\
  fprintf (FILE, "\t.set\tnoat\n");					\
  fprintf (FILE, "\tmove\t%s,%s\t\t# save current return address\n",	\
	   reg_names[GP_REG_FIRST + 1], reg_names[GP_REG_FIRST + 31]);	\
  fprintf (FILE, "\tjal\t_mcount\n");					\
  fprintf (FILE, "\tsubu\t%s,%s,8\t\t# _mcount pops 2 words from  stack\n", \
	   reg_names[STACK_POINTER_REGNUM],				\
	   reg_names[STACK_POINTER_REGNUM]);				\
  fprintf (FILE, "\t.set\treorder\n");					\
  fprintf (FILE, "\t.set\tat\n");					\
}

/* Define this macro if the code for function profiling should come
   before the function prologue.  Normally, the profiling code comes
   after.  */

/* #define PROFILE_BEFORE_PROLOGUE */

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1


/* A C statement to output, on the stream FILE, assembler code for a
   block of data that contains the constant parts of a trampoline. 
   This code should not include a label--the label is taken care of
   automatically.  */

#define TRAMPOLINE_TEMPLATE(STREAM)					 \
{									 \
  fprintf (STREAM, "\t.word\t0x03e00821\t\t# move   $1,$31\n");		\
  fprintf (STREAM, "\t.word\t0x04110001\t\t# bgezal $0,.+8\n");		\
  fprintf (STREAM, "\t.word\t0x00000000\t\t# nop\n");			\
  fprintf (STREAM, "\t.word\t0x8fe30010\t\t# lw     $3,16($31)\n");	\
  fprintf (STREAM, "\t.word\t0x8fe20014\t\t# lw     $2,20($31)\n");	\
  fprintf (STREAM, "\t.word\t0x00600008\t\t# jr     $3\n");		\
  fprintf (STREAM, "\t.word\t0x0020f821\t\t# move   $31,$1\n");		\
  fprintf (STREAM, "\t.word\t0x00000000\t\t# <function address>\n");	\
  fprintf (STREAM, "\t.word\t0x00000000\t\t# <static chain value>\n");	\
}

/* A C expression for the size in bytes of the trampoline, as an
   integer.  */

#define TRAMPOLINE_SIZE (9*4)

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT'
   is used for aligning trampolines.  */

/* #define TRAMPOLINE_ALIGNMENT 32 */

/* A C statement to initialize the variable parts of a trampoline. 
   ADDR is an RTX for the address of the trampoline; FNADDR is an
   RTX for the address of the nested function; STATIC_CHAIN is an
   RTX for the static chain value that should be passed to the
   function when it is called.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CHAIN)			    \
{									    \
  rtx addr = ADDR;							    \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (addr, 28)), FUNC);   \
  emit_move_insn (gen_rtx (MEM, SImode, plus_constant (addr, 32)), CHAIN);  \
									    \
  /* Attempt to make stack executable */				    \
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "__enable_execute_stack"), \
		     0, VOIDmode, 1, addr, Pmode);			    \
}


/* Attempt to turn on access permissions for the stack.  */

#define TRANSFER_FROM_TRAMPOLINE					\
									\
void									\
__enable_execute_stack (addr)						\
     char *addr;							\
{									\
  int size = getpagesize ();						\
  int mask = ~(size-1);							\
  char *page = (char *) (((int) addr) & mask);				\
  char *end  = (char *) ((((int) (addr + TRAMPOLINE_SIZE)) & mask) + size); \
									\
  /* 7 is PROT_READ | PROT_WRITE | PROT_EXEC */				\
  if (mprotect (page, end - page, 7) < 0)				\
    perror ("mprotect of trampoline code");				\
									\
/*									\
  if (cacheflush (addr, TRAMPOLINE_SIZE, 1) < 0)			\
    perror ("cacheflush of trampoline code");				\
 */									\
}


/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define GP_REG_OR_PSEUDO_STRICT_P(regno) \
  GP_REG_P((regno < FIRST_PSEUDO_REGISTER) ? regno : reg_renumber[regno])

#define GP_REG_OR_PSEUDO_NONSTRICT_P(regno) \
  (((regno) >= FIRST_PSEUDO_REGISTER) || (GP_REG_P (regno)))

#define REGNO_OK_FOR_INDEX_P(regno)	GP_REG_OR_PSEUDO_STRICT_P (regno)
#define REGNO_OK_FOR_BASE_P(regno)	GP_REG_OR_PSEUDO_STRICT_P (regno)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT

#define REG_OK_STRICT_P 0
#define REG_OK_FOR_INDEX_P(X) GP_REG_OR_PSEUDO_NONSTRICT_P (REGNO (X))
#define REG_OK_FOR_BASE_P(X)  GP_REG_OR_PSEUDO_NONSTRICT_P (REGNO (X))

#else

#define REG_OK_STRICT_P 1
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
#define REG_OK_FOR_BASE_P(X)  REGNO_OK_FOR_BASE_P  (REGNO (X))

#endif


/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* A C compound statement with a conditional `goto LABEL;' executed
   if X (an RTX) is a legitimate memory address on the target
   machine for a memory operand of mode MODE.

   It usually pays to define several simpler macros to serve as
   subroutines for this one.  Otherwise it may be too complicated
   to understand.

   This macro must exist in two variants: a strict variant and a
   non-strict one.  The strict variant is used in the reload pass. 
   It must be defined so that any pseudo-register that has not been
   allocated a hard register is considered a memory reference.  In
   contexts where some kind of register is required, a
   pseudo-register with no hard register must be rejected.

   The non-strict variant is used in other passes.  It must be
   defined to accept all pseudo-registers in every context where
   some kind of register is required.

   Compiler source files that want to use the strict variant of
   this macro define the macro `REG_OK_STRICT'.  You should use an
   `#ifdef REG_OK_STRICT' conditional to define the strict variant
   in that case and the non-strict variant otherwise.

   Typically among the subroutines used to define
   `GO_IF_LEGITIMATE_ADDRESS' are subroutines to check for
   acceptable registers for various purposes (one for base
   registers, one for index registers, and so on).  Then only these
   subroutine macros need have two variants; the higher levels of
   macros may be the same whether strict or not.

   Normally, constant addresses which are the sum of a `symbol_ref'
   and an integer are stored inside a `const' RTX to mark them as
   constant.  Therefore, there is no need to recognize such sums
   specifically as legitimate addresses.  Normally you would simply
   recognize any `const' as legitimate.

   Usually `PRINT_OPERAND_ADDRESS' is not prepared to handle
   constant sums that are not marked with  `const'.  It assumes
   that a naked `plus' indicates indexing.  If so, then you *must*
   reject such naked constant sums as illegitimate addresses, so
   that none of them will be given to `PRINT_OPERAND_ADDRESS'.

   On some machines, whether a symbolic address is legitimate
   depends on the section that the address refers to.  On these
   machines, define the macro `ENCODE_SECTION_INFO' to store the
   information into the `symbol_ref', and then check for it here. 
   When you see a `const', you will have to look inside it to find
   the `symbol_ref' in order to determine the section.  */

#if 1
#define GO_PRINTF(x)	trace(x)
#define GO_PRINTF2(x,y)	trace(x,y)
#define GO_DEBUG_RTX(x) debug_rtx(x)

#else
#define GO_PRINTF(x)
#define GO_PRINTF2(x,y)
#define GO_DEBUG_RTX(x)
#endif

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)				\
{									\
  register rtx xinsn = (X);						\
									\
  if (TARGET_DEBUG_B_MODE)						\
    {									\
      GO_PRINTF2 ("\n========== GO_IF_LEGITIMATE_ADDRESS, %sstrict\n",	\
		  (REG_OK_STRICT_P) ? "" : "not ");			\
      GO_DEBUG_RTX (xinsn);						\
    }									\
									\
  if (GET_CODE (xinsn) == REG && REG_OK_FOR_BASE_P (xinsn))		\
    goto ADDR;								\
									\
  if (CONSTANT_ADDRESS_P (xinsn))					\
    goto ADDR;								\
									\
  if (GET_CODE (xinsn) == PLUS)						\
    {									\
      register rtx xplus0 = XEXP (xinsn, 0);				\
      register rtx xplus1 = XEXP (xinsn, 1);				\
      register enum rtx_code code0 = GET_CODE (xplus0);			\
      register enum rtx_code code1 = GET_CODE (xplus1);			\
									\
      if (code0 != REG && code1 == REG)					\
	{								\
	  xplus0 = XEXP (xinsn, 1);					\
	  xplus1 = XEXP (xinsn, 0);					\
	  code0 = GET_CODE (xplus0);					\
	  code1 = GET_CODE (xplus1);					\
	}								\
									\
      if (code0 == REG && REG_OK_FOR_BASE_P (xplus0))			\
	{								\
	  if (code1 == CONST_INT)					\
	    {								\
	      register unsigned adj_offset = INTVAL (xplus1) + 0x8000;	\
									\
	      if ((adj_offset <= 0xffff)				\
		  && (adj_offset + GET_MODE_SIZE (MODE) - 1 <= 0xffff))	\
		goto ADDR;						\
	    }								\
									\
	  /* For some code sequences, you actually get better code by	\
	     pretending that the MIPS supports an address mode of a	\
	     constant address + a register, even though the real	\
	     machine doesn't support it.  This is because the		\
	     assembler can use $r1 to load just the high 16 bits, add	\
	     in the register, and fold the low 16 bits into the memory	\
	     reference, whereas the compiler generates a 4 instruction	\
	     sequence.  On the other hand, CSE is not as effective.	\
	     It would be a win to generate the lui directly, but the	\
	     MIPS assembler does not have syntax to generate the	\
	     appropriate relocation.  */				\
									\
	  else if (!TARGET_DEBUG_A_MODE					\
		   && code0 == REG					\
		   && CONSTANT_ADDRESS_P (xplus1))			\
	    goto ADDR;							\
	}								\
    }									\
									\
  if (TARGET_DEBUG_B_MODE)						\
    GO_PRINTF ("Not a legitimate address\n");				\
}


/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  On most machines, this can be defined as
   `CONSTANT_P (X)', but a few machines are more restrictive in
   which constant addresses are supported.

   `CONSTANT_P' accepts integer-values expressions whose values are
   not explicitly known, such as `symbol_ref', `label_ref', and
   `high' expressions and `const' arithmetic expressions, in
   addition to `const_int' and `const_double' expressions.  */

#define CONSTANT_ADDRESS_P(X)						\
  (CONSTANT_P (X) && (!HALF_PIC_P () || !HALF_PIC_ADDRESS_P (X)))


/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   At present, GAS doesn't understand li.[sd], so don't allow it
   to be generated at present.  Also, the MIPS assembler does not
   grok li.d Infinity.  */

#define LEGITIMATE_CONSTANT_P(X)					\
  (GET_CODE (X) != CONST_DOUBLE || mips_const_double_ok (X, GET_MODE (X)))


/* A C compound statement that attempts to replace X with a valid
   memory address for an operand of mode MODE.  WIN will be a C
   statement label elsewhere in the code; the macro definition may
   use

          GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

   to avoid further processing if the address has become legitimate.

   X will always be the result of a call to `break_out_memory_refs',
   and OLDX will be the operand that was given to that function to
   produce X.

   The code generated by this macro should not alter the
   substructure of X.  If it transforms X into a more legitimate
   form, it should assign X (which will always be a C variable) a
   new value.

   It is not necessary for this macro to come up with a legitimate
   address.  The compiler has standard ways of doing so in all
   cases.  In fact, it is safe for this macro to do nothing.  But
   often a machine-dependent strategy can generate better code.

   For the MIPS, transform:

	memory(X + <large int>)

   into:

	Y = <large int> & ~0x7fff;
	Z = X + Y
	memory (Z + (<large int> & 0x7fff));

   This is for CSE to find several similar references, and only use one Z.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)				\
{									\
  register rtx xinsn = (X);						\
									\
  if (TARGET_DEBUG_B_MODE)						\
    {									\
      GO_PRINTF ("\n========== LEGITIMIZE_ADDRESS\n");			\
      GO_DEBUG_RTX (xinsn);						\
    }									\
									\
  if (GET_CODE (xinsn) == PLUS)						\
    {									\
      register rtx xplus0 = XEXP (xinsn, 0);				\
      register rtx xplus1 = XEXP (xinsn, 1);				\
      register enum rtx_code code0 = GET_CODE (xplus0);			\
      register enum rtx_code code1 = GET_CODE (xplus1);			\
									\
      if (code0 != REG && code1 == REG)					\
	{								\
	  xplus0 = XEXP (xinsn, 1);					\
	  xplus1 = XEXP (xinsn, 0);					\
	  code0 = GET_CODE (xplus0);					\
	  code1 = GET_CODE (xplus1);					\
	}								\
									\
      if (code0 == REG && REG_OK_FOR_BASE_P (xplus0)			\
	  && code1 == CONST_INT && !SMALL_INT (xplus1))			\
	{								\
	  rtx int_reg = gen_reg_rtx (Pmode);				\
	  rtx ptr_reg = gen_reg_rtx (Pmode);				\
									\
	  emit_move_insn (int_reg,					\
			  GEN_INT (INTVAL (xplus1) & ~ 0x7fff));	\
									\
	  emit_insn (gen_rtx (SET, VOIDmode,				\
			      ptr_reg,					\
			      gen_rtx (PLUS, Pmode, xplus0, int_reg)));	\
									\
	  X = gen_rtx (PLUS, Pmode, ptr_reg,				\
		       GEN_INT (INTVAL (xplus1) & 0x7fff));		\
	  goto WIN;							\
	}								\
    }									\
									\
  if (TARGET_DEBUG_B_MODE)						\
    GO_PRINTF ("LEGITIMIZE_ADDRESS could not fix.\n");			\
}


/* A C statement or compound statement with a conditional `goto
   LABEL;' executed if memory address X (an RTX) can have different
   meanings depending on the machine mode of the memory reference it
   is used for.

   Autoincrement and autodecrement addresses typically have
   mode-dependent effects because the amount of the increment or
   decrement is the size of the operand being addressed.  Some
   machines have other mode-dependent addresses.  Many RISC machines
   have no mode-dependent addresses.

   You may assume that ADDR is a valid address for the machine.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL) {}


/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL has been created and stored in `DECL_RTL (DECL)'. 
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   The best way to modify the name string is by adding text to the
   beginning, with suitable punctuation to prevent any ambiguity. 
   Allocate the new name in `saveable_obstack'.  You will have to
   modify `ASM_OUTPUT_LABELREF' to remove and decode the added text
   and output the name accordingly.

   You can also check the information stored in the `symbol_ref' in
   the definition of `GO_IF_LEGITIMATE_ADDRESS' or
   `PRINT_OPERAND_ADDRESS'. */

#define ENCODE_SECTION_INFO(DECL)					\
do									\
  {									\
    if (optimize && mips_section_threshold > 0 && TARGET_GP_OPT		\
	&& TREE_CODE (DECL) == VAR_DECL)				\
      {									\
	int size = int_size_in_bytes (TREE_TYPE (DECL));		\
									\
	if (size > 0 && size <= mips_section_threshold)			\
	  SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;		\
      }									\
									\
    else if (HALF_PIC_P ())						\
      HALF_PIC_ENCODE (DECL);						\
  }									\
while (0)


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1
#endif

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this macro as a C expression which is nonzero if
   accessing less than a word of memory (i.e. a `char' or a
   `short') is no faster than accessing a word of memory, i.e., if
   such access require more than one instruction or if there is no
   difference in cost between byte and (aligned) word loads.

   On RISC machines, it tends to generate better code to define
   this as 1, since it avoids making a QI or HI mode register.  */
#define SLOW_BYTE_ACCESS 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.

   Only 5 bits are used in SLLV and SRLV */

#define SHIFT_COUNT_TRUNCATED

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Define this macro to control use of the character `$' in
   identifier names.  The value should be 0, 1, or 2.  0 means `$'
   is not allowed by default; 1 means it is allowed by default if
   `-traditional' is used; 2 means it is allowed by default provided
   `-ansi' is not used.  1 is the default; there is no need to
   define this macro in that case. */

#ifndef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 1
#endif

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx a words's mode.  */

#define FUNCTION_MODE SImode

/* Define TARGET_MEM_FUNCTIONS if we want to use calls to memcpy and
   memset, instead of the BSD functions bcopy and bzero.  */

#if defined(MIPS_SYSV) || defined(OSF_OS)
#define TARGET_MEM_FUNCTIONS
#endif


/* A part of a C `switch' statement that describes the relative
   costs of constant RTL expressions.  It must contain `case'
   labels for expression codes `const_int', `const', `symbol_ref',
   `label_ref' and `const_double'.  Each case must ultimately reach
   a `return' statement to return the relative cost of the use of
   that kind of constant value in an expression.  The cost may
   depend on the precise value of the constant, which is available
   for examination in X.

   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'.  */

#define CONST_COSTS(X,CODE,OUTER_CODE)					\
  case CONST_INT:							\
    /* Always return 0, since we don't have different sized		\
       instructions, hence different costs according to Richard		\
       Kenner */							\
    return COSTS_N_INSNS (0);						\
									\
  case LABEL_REF:							\
    return COSTS_N_INSNS (2);						\
									\
  case CONST:								\
    {									\
      rtx offset = const0_rtx;						\
      rtx symref = eliminate_constant_term (X, &offset);		\
									\
      if (GET_CODE (symref) == LABEL_REF)				\
	return COSTS_N_INSNS (2);					\
									\
      if (GET_CODE (symref) != SYMBOL_REF)				\
	return COSTS_N_INSNS (4);					\
									\
      /* let's be paranoid.... */					\
      if (INTVAL (offset) < -32768 || INTVAL (offset) > 32767)		\
	return COSTS_N_INSNS (2);					\
									\
      return COSTS_N_INSNS (SYMBOL_REF_FLAG (symref) ? 1 : 2);		\
    }									\
									\
  case SYMBOL_REF:							\
    return COSTS_N_INSNS (SYMBOL_REF_FLAG (X) ? 1 : 2);			\
									\
  case CONST_DOUBLE:							\
    return COSTS_N_INSNS ((CONST_DOUBLE_HIGH (X) == 0			\
			   && CONST_DOUBLE_LOW (X)) ? 2 : 4);


/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast instructions.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine.

   If -mdebugd is used, change the multiply cost to 2, so multiply by
   a constant isn't converted to a series of shifts.  This helps
   strength reduction, and also makes it easier to identify what the
   compiler is doing.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)					\
  case MEM:								\
    {									\
      int num_words = (GET_MODE_SIZE (GET_MODE (X)) > UNITS_PER_WORD) ? 2 : 1; \
      if (simple_memory_operand (X, GET_MODE (X)))			\
	return COSTS_N_INSNS (num_words);				\
									\
      return COSTS_N_INSNS (2*num_words);				\
    }									\
									\
  case FFS:								\
    return COSTS_N_INSNS (6);						\
									\
  case NOT:								\
    return COSTS_N_INSNS ((GET_MODE (X) == DImode) ? 2 : 1);		\
									\
  case AND:								\
  case IOR:								\
  case XOR:								\
    if (GET_MODE (X) == DImode)						\
      return COSTS_N_INSNS (2);						\
									\
    if (GET_CODE (XEXP (X, 1)) == CONST_INT)				\
      {									\
	rtx number = XEXP (X, 1);					\
	if (SMALL_INT_UNSIGNED (number))				\
	  return COSTS_N_INSNS (1);					\
									\
	else if (SMALL_INT (number))					\
	  return COSTS_N_INSNS (2);					\
									\
	return COSTS_N_INSNS (3);					\
      }									\
									\
    return COSTS_N_INSNS (1);						\
									\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFT:								\
  case LSHIFTRT:							\
    if (GET_MODE (X) == DImode)						\
      return COSTS_N_INSNS ((GET_CODE (XEXP (X, 1)) == CONST_INT) ? 12 : 4); \
									\
    return COSTS_N_INSNS (1);						\
									\
  case ABS:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode || xmode == DFmode)				\
	return COSTS_N_INSNS (1);					\
									\
      return COSTS_N_INSNS (4);						\
    }									\
									\
  case PLUS:								\
  case MINUS:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode || xmode == DFmode)				\
	return COSTS_N_INSNS (2);					\
									\
      if (xmode == DImode)						\
	return COSTS_N_INSNS (4);					\
									\
      return COSTS_N_INSNS (1);						\
    }									\
									\
  case NEG:								\
    return COSTS_N_INSNS ((GET_MODE (X) == DImode) ? 4 : 1);		\
									\
  case MULT:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode)						\
	return COSTS_N_INSNS (4);					\
									\
      if (xmode == DFmode)						\
	return COSTS_N_INSNS (5);					\
									\
      return COSTS_N_INSNS (12);					\
    }									\
									\
  case DIV:								\
  case MOD:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode)						\
	return COSTS_N_INSNS (12);					\
									\
      if (xmode == DFmode)						\
	return COSTS_N_INSNS (19);					\
    }									\
    /* fall through */							\
									\
  case UDIV:								\
  case UMOD:								\
    return COSTS_N_INSNS (35);

/* An expression giving the cost of an addressing mode that
   contains ADDRESS.  If not defined, the cost is computed from the
   form of the ADDRESS expression and the `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation
   of the true cost of the addressing mode.  However, on RISC
   machines, all instructions normally have the same length and
   execution time.  Hence all addresses will have equal costs.

   In cases where more than one form of an address is known, the
   form with the lowest cost will be used.  If multiple forms have
   the same, lowest, cost, the one that is the most complex will be
   used.

   For example, suppose an address that is equal to the sum of a
   register and a constant is used twice in the same basic block. 
   When this macro is not defined, the address will be computed in
   a register and memory references will be indirect through that
   register.  On machines where the cost of the addressing mode
   containing the sum is no higher than that of a simple indirect
   reference, this will produce an additional instruction and
   possibly require an additional register.  Proper specification
   of this macro eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the
   cost is not relevant and can be any value; invalid addresses
   need not be assigned a different cost.

   On machines where an address involving more than one register is
   as cheap as an address computation involving only one register,
   defining `ADDRESS_COST' to reflect this can cause two registers
   to be live over a region of code where only one would have been
   if `ADDRESS_COST' were not defined in that manner.  This effect
   should be considered in the definition of this macro. 
   Equivalent costs should probably only be given to addresses with
   different numbers of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as
   a constant. */

#define ADDRESS_COST(ADDR) (REG_P (ADDR) ? 1 : mips_address_cost (ADDR))

/* A C expression for the cost of moving data from a register in
   class FROM to one in class TO.  The classes are expressed using
   the enumeration values such as `GENERAL_REGS'.  A value of 2 is
   the default; other values are interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the
   same as TO; on some machines it is expensive to move between
   registers if they are not general registers.

   If reload sees an insn consisting of a single `set' between two
   hard registers, and if `REGISTER_MOVE_COST' applied to their
   classes returns a value of 2, reload does not check to ensure
   that the constraints of the insn are met.  Setting a cost of
   other than 2 will allow reload to verify that the constraints are
   met.  You should do this if the `movM' pattern's constraints do
   not allow such copying.  */

#define REGISTER_MOVE_COST(FROM, TO) 4	/* force reload to use constraints */

/* A C expression for the cost of a branch instruction.  A value of
   1 is the default; other values are interpreted relative to that.  */

#define BRANCH_COST \
  ((mips_cpu == PROCESSOR_R4000 || mips_cpu == PROCESSOR_R6000) ? 2 : 1)


/* Used in by the peephole code.  */
#define classify_op(op,mode)	(mips_rtx_classify[ (int)GET_CODE (op) ])
#define additive_op(op,mode)	((classify_op (op,mode) & CLASS_ADD_OP)      != 0)
#define divmod_op(op,mode)	((classify_op (op,mode) & CLASS_DIVMOD_OP)   != 0)
#define unsigned_op(op,mode)	((classify_op (op,mode) & CLASS_UNSIGNED_OP) != 0)

#define CLASS_ADD_OP		0x01	/* operator is PLUS/MINUS */
#define CLASS_DIVMOD_OP		0x02	/* operator is {,U}{DIV,MOD} */
#define CLASS_UNSIGNED_OP	0x04	/* operator is U{DIV,MOD} */
#define CLASS_CMP_OP		0x08	/* operator is comparison */
#define CLASS_EQUALITY_OP	0x10	/* operator is == or != */
#define CLASS_FCMP_OP		0x08	/* operator is fp. compare */

#define CLASS_UNS_CMP_OP	(CLASS_UNSIGNED_OP | CLASS_CMP_OP)


/* Optionally define this if you have added predicates to
   `MACHINE.c'.  This macro is called within an initializer of an
   array of structures.  The first field in the structure is the
   name of a predicate and the second field is an array of rtl
   codes.  For each predicate, list all rtl codes that can be in
   expressions matched by the predicate.  The list should have a
   trailing comma.  Here is an example of two entries in the list
   for a typical RISC machine:

   #define PREDICATE_CODES \
     {"gen_reg_rtx_operand", {SUBREG, REG}},  \
     {"reg_or_short_cint_operand", {SUBREG, REG, CONST_INT}},

   Defining this macro does not affect the generated code (however,
   incorrect definitions that omit an rtl code that may be matched
   by the predicate can cause the compiler to malfunction). 
   Instead, it allows the table built by `genrecog' to be more
   compact and efficient, thus speeding up the compiler.  The most
   important predicates to include in the list specified by this
   macro are thoses used in the most insn patterns.  */

#define PREDICATE_CODES							\
  {"uns_arith_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"arith_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"arith32_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"reg_or_0_operand",		{ REG, CONST_INT, SUBREG }},		\
  {"small_int",			{ CONST_INT }},				\
  {"large_int",			{ CONST_INT }},				\
  {"md_register_operand",	{ REG }},				\
  {"mips_const_double_ok",	{ CONST_DOUBLE }},			\
  {"simple_memory_operand",	{ MEM, SUBREG }},			\
  {"equality_op",		{ EQ, NE }},				\
  {"cmp_op",			{ EQ, NE, GT, GE, GTU, GEU, LT, LE,	\
				  LTU, LEU }},				\
  {"cmp2_op",			{ EQ, NE, GT, GE, GTU, GEU, LT, LE,	\
				  LTU, LEU }},				\
  {"fcmp_op",			{ EQ, NE, GT, GE, LT, LE }},		\
  {"uns_cmp_op",		{ GTU, GEU, LTU, LEU }},


/* If defined, a C statement to be executed just prior to the
   output of assembler code for INSN, to modify the extracted
   operands so they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.  The
   contents of this vector are what will be used to convert the
   insn template into assembler code, so you can change the
   assembler output by changing the contents of the vector.

   We use it to check if the current insn needs a nop in front of it
   because of load delays, and also to update the delay slot
   statistics.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)			\
  final_prescan_insn (INSN, OPVEC, NOPERANDS)


/* Tell final.c how to eliminate redundant test instructions.
   Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* A C compound statement to set the components of `cc_status'
   appropriately for an insn INSN whose body is EXP.  It is this
   macro's responsibility to recognize insns that set the condition
   code as a byproduct of other activity as well as those that
   explicitly set `(cc0)'.

   This macro is not used on machines that do not use `cc0'.  */

#define NOTICE_UPDATE_CC(EXP, INSN)					\
do									\
  {									\
    enum attr_type type = get_attr_type (INSN);				\
    if (type == TYPE_ICMP || type == TYPE_FCMP)				\
      CC_STATUS_INIT;							\
  }									\
while (0)

/* A list of names to be used for additional modes for condition code
   values in registers.  These names are added to `enum machine_mode'
   and all have class `MODE_CC'.  By convention, they should start
   with `CC' and end with `mode'.

   You should only define this macro if your machine does not use
   `cc0' and only if additional modes are required.

   On the MIPS, we use CC_FPmode for all floating point except for not
   equal, CC_REV_FPmode for not equal (to reverse the sense of the
   jump), CC_EQmode for integer equality/inequality comparisons,
   CC_0mode for comparisons against 0, and CCmode for other integer
   comparisons. */

#define EXTRA_CC_MODES CC_EQmode, CC_FPmode, CC_0mode, CC_REV_FPmode

/* A list of C strings giving the names for the modes listed in
   `EXTRA_CC_MODES'.  */

#define EXTRA_CC_NAMES "CC_EQ", "CC_FP", "CC_0", "CC_REV_FP"

/* Returns a mode from class `MODE_CC' to be used when comparison
   operation code OP is applied to rtx X.  */

#define SELECT_CC_MODE(OP, X, Y)					\
  (GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT				\
	? SImode							\
	: ((OP == NE) ? CC_REV_FPmode : CC_FPmode))


/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.
   If we are optimizing to use the global pointer, create a temporary
   file to hold all of the text stuff, and write it out to the end.
   This is needed because the MIPS assembler is evidently one pass,
   and if it hasn't seen the relevant .comm/.lcomm/.extern/.sdata
   declaration when the code is processed, it generates a two
   instruction sequence.  */

#define ASM_FILE_START(STREAM) mips_asm_file_start (STREAM)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON " #APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF " #NO_APP\n"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).

   In order to support the two different conventions for register names,
   we use the name of a table set up in mips.c, which is overwritten
   if -mrnames is used.  */

#define REGISTER_NAMES							\
{									\
  &mips_reg_names[ 0][0],						\
  &mips_reg_names[ 1][0],						\
  &mips_reg_names[ 2][0],						\
  &mips_reg_names[ 3][0],						\
  &mips_reg_names[ 4][0],						\
  &mips_reg_names[ 5][0],						\
  &mips_reg_names[ 6][0],						\
  &mips_reg_names[ 7][0],						\
  &mips_reg_names[ 8][0],						\
  &mips_reg_names[ 9][0],						\
  &mips_reg_names[10][0],						\
  &mips_reg_names[11][0],						\
  &mips_reg_names[12][0],						\
  &mips_reg_names[13][0],						\
  &mips_reg_names[14][0],						\
  &mips_reg_names[15][0],						\
  &mips_reg_names[16][0],						\
  &mips_reg_names[17][0],						\
  &mips_reg_names[18][0],						\
  &mips_reg_names[19][0],						\
  &mips_reg_names[20][0],						\
  &mips_reg_names[21][0],						\
  &mips_reg_names[22][0],						\
  &mips_reg_names[23][0],						\
  &mips_reg_names[24][0],						\
  &mips_reg_names[25][0],						\
  &mips_reg_names[26][0],						\
  &mips_reg_names[27][0],						\
  &mips_reg_names[28][0],						\
  &mips_reg_names[29][0],						\
  &mips_reg_names[30][0],						\
  &mips_reg_names[31][0],						\
  &mips_reg_names[32][0],						\
  &mips_reg_names[33][0],						\
  &mips_reg_names[34][0],						\
  &mips_reg_names[35][0],						\
  &mips_reg_names[36][0],						\
  &mips_reg_names[37][0],						\
  &mips_reg_names[38][0],						\
  &mips_reg_names[39][0],						\
  &mips_reg_names[40][0],						\
  &mips_reg_names[41][0],						\
  &mips_reg_names[42][0],						\
  &mips_reg_names[43][0],						\
  &mips_reg_names[44][0],						\
  &mips_reg_names[45][0],						\
  &mips_reg_names[46][0],						\
  &mips_reg_names[47][0],						\
  &mips_reg_names[48][0],						\
  &mips_reg_names[49][0],						\
  &mips_reg_names[50][0],						\
  &mips_reg_names[51][0],						\
  &mips_reg_names[52][0],						\
  &mips_reg_names[53][0],						\
  &mips_reg_names[54][0],						\
  &mips_reg_names[55][0],						\
  &mips_reg_names[56][0],						\
  &mips_reg_names[57][0],						\
  &mips_reg_names[58][0],						\
  &mips_reg_names[59][0],						\
  &mips_reg_names[60][0],						\
  &mips_reg_names[61][0],						\
  &mips_reg_names[62][0],						\
  &mips_reg_names[63][0],						\
  &mips_reg_names[64][0],						\
  &mips_reg_names[65][0],						\
  &mips_reg_names[66][0],						\
}

/* print-rtl.c can't use REGISTER_NAMES, since it depends on mips.c.
   So define this for it.  */
#define DEBUG_REGISTER_NAMES						\
{									\
  "$0",   "at",   "v0",   "v1",   "a0",   "a1",   "a2",   "a3",		\
  "t0",   "t1",   "t2",   "t3",   "t4",   "t5",   "t6",   "t7",		\
  "s0",   "s1",   "s2",   "s3",   "s4",   "s5",   "s6",   "s7",		\
  "t8",   "t9",   "k0",   "k1",   "gp",   "sp",   "$fp",   "ra",	\
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",	\
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",	\
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",	\
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",	\
  "hi",   "lo",   "$fcr31"						\
}

/* If defined, a C initializer for an array of structures
   containing a name and a register number.  This macro defines
   additional names for hard registers, thus allowing the `asm'
   option in declarations to refer to registers using alternate
   names.

   We define both names for the integer registers here.  */

#define ADDITIONAL_REGISTER_NAMES					\
{									\
  { "$0",	 0 + GP_REG_FIRST },					\
  { "$1",	 1 + GP_REG_FIRST },					\
  { "$2",	 2 + GP_REG_FIRST },					\
  { "$3",	 3 + GP_REG_FIRST },					\
  { "$4",	 4 + GP_REG_FIRST },					\
  { "$5",	 5 + GP_REG_FIRST },					\
  { "$6",	 6 + GP_REG_FIRST },					\
  { "$7",	 7 + GP_REG_FIRST },					\
  { "$8",	 8 + GP_REG_FIRST },					\
  { "$9",	 9 + GP_REG_FIRST },					\
  { "$10",	10 + GP_REG_FIRST },					\
  { "$11",	11 + GP_REG_FIRST },					\
  { "$12",	12 + GP_REG_FIRST },					\
  { "$13",	13 + GP_REG_FIRST },					\
  { "$14",	14 + GP_REG_FIRST },					\
  { "$15",	15 + GP_REG_FIRST },					\
  { "$16",	16 + GP_REG_FIRST },					\
  { "$17",	17 + GP_REG_FIRST },					\
  { "$18",	18 + GP_REG_FIRST },					\
  { "$19",	19 + GP_REG_FIRST },					\
  { "$20",	20 + GP_REG_FIRST },					\
  { "$21",	21 + GP_REG_FIRST },					\
  { "$22",	22 + GP_REG_FIRST },					\
  { "$23",	23 + GP_REG_FIRST },					\
  { "$24",	24 + GP_REG_FIRST },					\
  { "$25",	25 + GP_REG_FIRST },					\
  { "$26",	26 + GP_REG_FIRST },					\
  { "$27",	27 + GP_REG_FIRST },					\
  { "$28",	28 + GP_REG_FIRST },					\
  { "$29",	29 + GP_REG_FIRST },					\
  { "$30",	30 + GP_REG_FIRST },					\
  { "$31",	31 + GP_REG_FIRST },					\
  { "$sp",	29 + GP_REG_FIRST },					\
  { "$fp",	30 + GP_REG_FIRST },					\
  { "at",	 1 + GP_REG_FIRST },					\
  { "v0",	 2 + GP_REG_FIRST },					\
  { "v1",	 3 + GP_REG_FIRST },					\
  { "a0",	 4 + GP_REG_FIRST },					\
  { "a1",	 5 + GP_REG_FIRST },					\
  { "a2",	 6 + GP_REG_FIRST },					\
  { "a3",	 7 + GP_REG_FIRST },					\
  { "t0",	 8 + GP_REG_FIRST },					\
  { "t1",	 9 + GP_REG_FIRST },					\
  { "t2",	10 + GP_REG_FIRST },					\
  { "t3",	11 + GP_REG_FIRST },					\
  { "t4",	12 + GP_REG_FIRST },					\
  { "t5",	13 + GP_REG_FIRST },					\
  { "t6",	14 + GP_REG_FIRST },					\
  { "t7",	15 + GP_REG_FIRST },					\
  { "s0",	16 + GP_REG_FIRST },					\
  { "s1",	17 + GP_REG_FIRST },					\
  { "s2",	18 + GP_REG_FIRST },					\
  { "s3",	19 + GP_REG_FIRST },					\
  { "s4",	20 + GP_REG_FIRST },					\
  { "s5",	21 + GP_REG_FIRST },					\
  { "s6",	22 + GP_REG_FIRST },					\
  { "s7",	23 + GP_REG_FIRST },					\
  { "t8",	24 + GP_REG_FIRST },					\
  { "t9",	25 + GP_REG_FIRST },					\
  { "k0",	26 + GP_REG_FIRST },					\
  { "k1",	27 + GP_REG_FIRST },					\
  { "gp",	28 + GP_REG_FIRST },					\
  { "sp",	29 + GP_REG_FIRST },					\
  { "fp",	30 + GP_REG_FIRST },					\
  { "ra",	31 + GP_REG_FIRST },					\
  { "$sp",	29 + GP_REG_FIRST },					\
  { "$fp",	30 + GP_REG_FIRST },					\
  { "cc",	FPSW_REGNUM },						\
}

/* Define results of standard character escape sequences.  */
#define TARGET_BELL	007
#define TARGET_BS	010
#define TARGET_TAB	011
#define TARGET_NEWLINE	012
#define TARGET_VT	013
#define TARGET_FF	014
#define TARGET_CR	015

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand X.  X is an RTL
   expression.

   CODE is a value that can be used to specify one of several ways
   of printing the operand.  It is used when identical operands
   must be printed differently depending on the context.  CODE
   comes from the `%' specification that was used to request
   printing of the operand.  If the specification was just `%DIGIT'
   then CODE is 0; if the specification was `%LTR DIGIT' then CODE
   is the ASCII code for LTR.

   If X is a register, this macro should print the register's name.
   The names can be found in an array `reg_names' whose type is
   `char *[]'.  `reg_names' is initialized from `REGISTER_NAMES'.

   When the machine description has a specification `%PUNCT' (a `%'
   followed by a punctuation character), this macro is called with
   a null pointer for X and the punctuation character for CODE.

   See mips.c for the MIPS specific codes.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

/* A C expression which evaluates to true if CODE is a valid
   punctuation character for use in the `PRINT_OPERAND' macro.  If
   `PRINT_OPERAND_PUNCT_VALID_P' is not defined, it means that no
   punctuation characters (except for the standard one, `%') are
   used in this way.  */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) mips_print_operand_punct[CODE]

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.

   On some machines, the syntax for a symbolic address depends on
   the section that the address refers to.  On these machines,
   define the macro `ENCODE_SECTION_INFO' to store the information
   into the `symbol_ref', and then check for it here.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)


/* A C statement, to be executed after all slot-filler instructions
   have been output.  If necessary, call `dbr_sequence_length' to
   determine the number of slots filled in a sequence (zero if not
   currently outputting a sequence), to decide how many no-ops to
   output, or whatever.

   Don't define this macro if it has nothing to do, but it is
   helpful in reading assembly output if the extent of the delay
   sequence is made explicit (e.g. with white space).

   Note that output routines for instructions with delay slots must
   be prepared to deal with not being output as part of a sequence
   (i.e.  when the scheduling pass is not run, or when no slot
   fillers could be found.)  The variable `final_sequence' is null
   when not processing a sequence, otherwise it contains the
   `sequence' rtx being output.  */

#define DBR_OUTPUT_SEQEND(STREAM)					\
do									\
  {									\
    if (set_nomacro > 0 && --set_nomacro == 0)				\
      fputs ("\t.set\tmacro\n", STREAM);				\
									\
    if (set_noreorder > 0 && --set_noreorder == 0)			\
      fputs ("\t.set\treorder\n", STREAM);				\
									\
    dslots_jump_filled++;						\
    fputs ("\n", STREAM);						\
  }									\
while (0)


/* How to tell the debugger about changes of source files.  Note, the
   mips ECOFF format cannot deal with changes of files inside of
   functions, which means the output of parser generators like bison
   is generally not debuggable without using the -l switch.  Lose,
   lose, lose.  Silicon graphics seems to want all .file's hardwired
   to 1.  */

#ifndef SET_FILE_NUMBER
#define SET_FILE_NUMBER() ++num_source_filenames
#endif

#define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME)			\
  mips_output_filename (STREAM, NAME)

/* This is how to output a note the debugger telling it the line number
   to which the following sequence of instructions corresponds.
   Silicon graphics puts a label after each .loc.  */

#ifndef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)
#endif

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE)				\
  mips_output_lineno (STREAM, LINE)

/* The MIPS implementation uses some labels for it's own purposed.  The
   following lists what labels are created, and are all formed by the
   pattern $L[a-z].*.  The machine independent portion of GCC creates
   labels matching:  $L[A-Z][0-9]+ and $L[0-9]+.

	LM[0-9]+	Silicon Graphics/ECOFF stabs label before each stmt.
	$Lb[0-9]+	Begin blocks for MIPS debug support
	$Lc[0-9]+	Label for use in s<xx> operation.
	$Le[0-9]+	End blocks for MIPS debug support
	$Lp\..+		Half-pic labels. */

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.

   If we are optimizing the gp, remember that this label has been put
   out, so we know not to emit an .extern for it in mips_asm_file_end.
   We use one of the common bits in the IDENTIFIER tree node for this,
   since those bits seem to be unused, and we don't have any method
   of getting the decl nodes from the name.  */

#define ASM_OUTPUT_LABEL(STREAM,NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)


/* A C statement (sans semicolon) to output to the stdio stream
   STREAM any text necessary for declaring the name NAME of an
   initialized variable which is being defined.  This macro must
   output the label definition (perhaps using `ASM_OUTPUT_LABEL'). 
   The argument DECL is the `VAR_DECL' tree node representing the
   variable.

   If this macro is not defined, then the variable name is defined
   in the usual manner as a label (by means of `ASM_OUTPUT_LABEL').  */

#define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL)			\
do									\
 {									\
   mips_declare_object (STREAM, NAME, "", ":\n", 0);			\
   HALF_PIC_DECLARE (NAME);						\
 }									\
while (0)


/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(STREAM,NAME)				\
  do {									\
    fputs ("\t.globl\t", STREAM);					\
    assemble_name (STREAM, NAME);					\
    fputs ("\n", STREAM);						\
  } while (0)

/* This says how to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)			\
  mips_declare_object (STREAM, NAME, "\n\t.comm\t", ",%u\n", (ROUNDED))

/* This says how to define a local common symbol (ie, not visible to
   linker).  */

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
  mips_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (ROUNDED))


/* This says how to output an external.  It would be possible not to
   output anything and let undefined symbol become external. However
   the assembler uses length information on externals to allocate in
   data/sdata bss/sbss, thereby saving exec time.  */

#define ASM_OUTPUT_EXTERNAL(STREAM,DECL,NAME) \
  mips_output_external(STREAM,DECL,NAME)

/* This says what to print at the end of the assembly file */
#define ASM_FILE_END(STREAM) mips_asm_file_end(STREAM)


/* This is how to declare a function name.  The actual work of
   emitting the label is moved to function_prologue, so that we can
   get the line number correctly emitted before the .ent directive,
   and after any .file directives.

   Also, switch files if we are optimizing the global pointer.  */

#define ASM_DECLARE_FUNCTION_NAME(STREAM,NAME,DECL)			\
{									\
  extern FILE *asm_out_text_file;					\
  if (TARGET_GP_OPT)							\
    STREAM = asm_out_text_file;						\
									\
  current_function_name = NAME;						\
  HALF_PIC_DECLARE (NAME);						\
}

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(STREAM,NAME) fprintf (STREAM, "%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM,PREFIX,NUM)			\
  fprintf (STREAM, "$%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf (LABEL, "*$%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(STREAM,VALUE)					\
  mips_output_double (STREAM, VALUE)


/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(STREAM,VALUE)					\
  mips_output_float (STREAM, VALUE)


/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(STREAM,VALUE)					\
do {									\
  fprintf (STREAM, "\t.word\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
} while (0)

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.half\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
}

#define ASM_OUTPUT_CHAR(STREAM,VALUE)					\
{									\
  fprintf (STREAM, "\t.byte\t");					\
  output_addr_const (STREAM, (VALUE));					\
  fprintf (STREAM, "\n");						\
}

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(STREAM,VALUE)					\
  fprintf (STREAM, "\t.byte\t0x%x\n", (VALUE))

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  fprintf (STREAM, "\t.word\t$L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (We  do not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, VALUE, REL)			\
  fprintf (STREAM, "\t.word\t$L%d-$L%d\n", VALUE, REL)

/* This is how to emit the initial label for switch statements.  We
   need to put the switch labels somewhere else from the text section,
   because the MIPS assembler gets real confused about line numbers if
   .word's appear in the text section.  */

#define ASM_OUTPUT_CASE_LABEL(STREAM, PREFIX, NUM, JUMPTABLE)		\
{									\
  rdata_section ();							\
  ASM_OUTPUT_ALIGN (STREAM, 2);						\
  ASM_OUTPUT_INTERNAL_LABEL (STREAM, PREFIX, NUM);			\
}

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
{									\
  int mask = (1 << (LOG)) - 1;						\
  fprintf (STREAM, "\t.align\t%d\n", (LOG));				\
}

/* This is how to output an assembler line to to advance the location
   counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(STREAM,SIZE)					\
  fprintf (STREAM, "\t.space\t%u\n", (SIZE))


/* This is how to output a string.  */
#define ASM_OUTPUT_ASCII(STREAM, STRING, LEN)				\
do {									\
  register int i, c, len = (LEN), cur_pos = 17;				\
  register unsigned char *string = (unsigned char *)(STRING);		\
  fprintf ((STREAM), "\t.ascii\t\"");					\
  for (i = 0; i < len; i++)						\
    {									\
      register int c = string[i];					\
									\
      switch (c)							\
	{								\
	case '\"':							\
	case '\\':							\
	  putc ('\\', (STREAM));					\
	  putc (c, (STREAM));						\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_NEWLINE:						\
	  fputs ("\\n", (STREAM));					\
	  if (i+1 < len							\
	      && (((c = string[i+1]) >= '\040' && c <= '~')		\
		  || c == TARGET_TAB))					\
	    cur_pos = 32767;		/* break right here */		\
	  else								\
	    cur_pos += 2;						\
	  break;							\
									\
	case TARGET_TAB:						\
	  fputs ("\\t", (STREAM));					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_FF:							\
	  fputs ("\\f", (STREAM));					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_BS:							\
	  fputs ("\\b", (STREAM));					\
	  cur_pos += 2;							\
	  break;							\
									\
	case TARGET_CR:							\
	  fputs ("\\r", (STREAM));					\
	  cur_pos += 2;							\
	  break;							\
									\
	default:							\
	  if (c >= ' ' && c < 0177)					\
	    {								\
	      putc (c, (STREAM));					\
	      cur_pos++;						\
	    }								\
	  else								\
	    {								\
	      fprintf ((STREAM), "\\%03o", c);				\
	      cur_pos += 4;						\
	    }								\
	}								\
									\
      if (cur_pos > 72 && i+1 < len)					\
	{								\
	  cur_pos = 17;							\
	  fprintf ((STREAM), "\"\n\t.ascii\t\"");			\
	}								\
    }									\
  fprintf ((STREAM), "\"\n");						\
} while (0)

/* Handle certain cpp directives used in header files on sysV.  */
#define SCCS_DIRECTIVE

/* Output #ident as a in the read-only data section.  */
#define ASM_OUTPUT_IDENT(FILE, STRING)					\
{									\
  char *p = STRING;							\
  int size = strlen (p) + 1;						\
  rdata_section ();							\
  assemble_string (p, size);						\
}


/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"	/* instructions */
#define DATA_SECTION_ASM_OP	"\t.data"	/* large data */
#define SDATA_SECTION_ASM_OP	"\t.sdata"	/* small data */
#define RDATA_SECTION_ASM_OP	"\t.rdata"	/* read-only data */
#define READONLY_DATA_SECTION	rdata_section

/* What other sections we support other than the normal .data/.text.  */

#define EXTRA_SECTIONS in_sdata, in_rdata, in_last_p1

/* Define the additional functions to select our additional sections.  */

/* on the MIPS it is not a good idea to put constants in the text
   section, since this defeats the sdata/data mechanism. This is
   especially true when -O is used. In this case an effort is made to
   address with faster (gp) register relative addressing, which can
   only get at sdata and sbss items (there is no stext !!)  However,
   if the constant is too large for sdata, and it's readonly, it
   will go into the .rdata section. */

#define EXTRA_SECTION_FUNCTIONS						\
void									\
sdata_section ()							\
{									\
  if (in_section != in_sdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", SDATA_SECTION_ASM_OP);		\
      in_section = in_sdata;						\
    }									\
}									\
									\
void									\
rdata_section ()							\
{									\
  if (in_section != in_rdata)						\
    {									\
      fprintf (asm_out_file, "%s\n", RDATA_SECTION_ASM_OP);		\
      in_section = in_rdata;						\
    }									\
}

/* Given a decl node or constant node, choose the section to output it in
   and select that section.  */

#define SELECT_RTX_SECTION(MODE,RTX)					\
{									\
  if ((GET_MODE_SIZE(MODE) / BITS_PER_UNIT) <= mips_section_threshold	\
      && mips_section_threshold > 0)					\
    sdata_section ();							\
  else									\
    rdata_section ();							\
}									\

#define SELECT_SECTION(DECL,RELOC)					\
{									\
  if (int_size_in_bytes (TREE_TYPE (DECL)) <= mips_section_threshold	\
      && mips_section_threshold > 0)					\
    sdata_section ();							\
  else if (TREE_CODE (DECL) == STRING_CST)				\
    {									\
      if (flag_writable_strings)					\
	data_section ();						\
      else								\
	rdata_section ();						\
    }									\
  else if (TREE_CODE (DECL) != VAR_DECL)				\
    rdata_section ();							\
  else if (!TREE_READONLY (DECL))					\
    data_section ();							\
  else									\
    rdata_section ();							\
}


/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),			\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)				\
do									\
  {									\
    fprintf (STREAM, "\tsubu\t%s,%s,8\n\tsw\t%s,0(%s)\n",		\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM]);				\
  }									\
while (0)

#define ASM_OUTPUT_REG_POP(STREAM,REGNO)				\
do									\
  {									\
    if (! set_noreorder)						\
      fprintf (STREAM, "\t.set\tnoreorder\n");				\
									\
    dslots_load_total++;						\
    dslots_load_filled++;						\
    fprintf (STREAM, "\tlw\t%s,0(%s)\n\taddu\t%s,%s,8\n",		\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM]);				\
									\
    if (! set_noreorder)						\
      fprintf (STREAM, "\t.set\treorder\n");				\
  }									\
while (0)

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* How to start an assembler comment.  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START "\t\t# "
#endif



/* Macros for mips-tfile.c to encapsulate stabs in ECOFF, and for
   and mips-tdump.c to print them out.

   These must match the corresponding definitions in gdb/mipsread.c.
   Unfortunately, gcc and gdb do not currently share any directories. */

#define CODE_MASK 0x8F300
#define MIPS_IS_STAB(sym) (((sym)->index & 0xFFF00) == CODE_MASK)
#define MIPS_MARK_STAB(code) ((code)+CODE_MASK)
#define MIPS_UNMARK_STAB(code) ((code)-CODE_MASK)


/* Default definitions for size_t and ptrdiff_t.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE	"unsigned int"
#endif

#ifndef PTRDIFF_TYPE
#define PTRDIFF_TYPE	"int"
#endif

