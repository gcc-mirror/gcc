/* Definitions of target machine for GNU compiler.  MIPS version.
   Copyright (C) 1989, 90-98, 1999 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky (lich@inria.inria.fr).
   Changed by Michael Meissner	(meissner@osf.org).
   64 bit r4000 support by Ian Lance Taylor (ian@cygnus.com) and
   Brendan Eich (brendan@microunity.com).

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


/* Standard GCC variables that we reference.  */

extern char    *asm_file_name;
extern char	call_used_regs[];
extern int	current_function_calls_alloca;
extern char    *language_string;
extern int	may_call_alloca;
extern char   **save_argv;
extern int	target_flags;
extern char    *version_string;

/* MIPS external variables defined in mips.c.  */

/* comparison type */
enum cmp_type {
  CMP_SI,				/* compare four byte integers */
  CMP_DI,				/* compare eight byte integers */
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
  PROCESSOR_R3900,
  PROCESSOR_R6000,
  PROCESSOR_R4000,
  PROCESSOR_R4100,
  PROCESSOR_R4300,
  PROCESSOR_R4600,
  PROCESSOR_R4650,
  PROCESSOR_R5000,
  PROCESSOR_R8000
};

/* Recast the cpu class to be the cpu attribute.  */
#define mips_cpu_attr ((enum attr_cpu)mips_cpu)

/* Which ABI to use.  These are constants because abi64.h must check their
   value at preprocessing time.

   ABI_32 (original 32, or o32), ABI_N32 (n32), ABI_64 (n64) are all
   defined by SGI.  ABI_O64 is o32 extended to work on a 64 bit machine. */

#define ABI_32  0
#define ABI_N32 1
#define ABI_64  2
#define ABI_EABI 3
#define ABI_O64  4

#ifndef MIPS_ABI_DEFAULT
/* We define this away so that there is no extra runtime cost if the target
   doesn't support multiple ABIs.  */
#define mips_abi ABI_32
#else
extern int mips_abi;
#endif

/* Whether to emit abicalls code sequences or not.  */

enum mips_abicalls_type {
  MIPS_ABICALLS_NO,
  MIPS_ABICALLS_YES
};

/* Recast the abicalls class to be the abicalls attribute.  */
#define mips_abicalls_attr ((enum attr_abicalls)mips_abicalls)

/* Which type of block move to do (whether or not the last store is
   split out so it can fill a branch delay slot).  */

enum block_move_type {
  BLOCK_MOVE_NORMAL,			/* generate complete block move */
  BLOCK_MOVE_NOT_LAST,			/* generate all but last store */
  BLOCK_MOVE_LAST			/* generate just the last store */
};

extern char mips_reg_names[][8];	/* register names (a0 vs. $4). */
extern char mips_print_operand_punct[];	/* print_operand punctuation chars */
extern const char *current_function_file; /* filename current function is in */
extern int num_source_filenames;	/* current .file # */
extern int inside_function;		/* != 0 if inside of a function */
extern int ignore_line_number;		/* != 0 if we are to ignore next .loc */
extern int file_in_function_warning;	/* warning given about .file in func */
extern int sdb_label_count;		/* block start/end next label # */
extern int sdb_begin_function_line;     /* Starting Line of current function */
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
extern struct rtx_def *branch_cmp[2];	/* operands for compare */
extern enum cmp_type branch_type;	/* what type of branch to use */
extern enum processor_type mips_cpu;	/* which cpu are we scheduling for */
extern enum mips_abicalls_type mips_abicalls;/* for svr4 abi pic calls */
extern int mips_isa;			/* architectural level */
extern int mips16;			/* whether generating mips16 code */
extern int mips16_hard_float;		/* mips16 without -msoft-float */
extern int mips_entry;			/* generate entry/exit for mips16 */
extern const char *mips_cpu_string;	/* for -mcpu=<xxx> */
extern const char *mips_isa_string;	/* for -mips{1,2,3,4} */
extern const char *mips_abi_string;	/* for -mabi={32,n32,64} */
extern const char *mips_entry_string;	/* for -mentry */
extern const char *mips_no_mips16_string;/* for -mno-mips16 */
extern const char *mips_explicit_type_size_string;/* for -mexplicit-type-size */
extern int mips_split_addresses;	/* perform high/lo_sum support */
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
extern struct rtx_def *embedded_pic_fnaddr_rtx;	/* function address */
extern int mips_string_length;		/* length of strings for mips16 */
extern struct rtx_def *mips16_gp_pseudo_rtx; /* psuedo reg holding $gp */

/* Functions within mips.c that we reference.  Some of these return
   type HOST_WIDE_INT, so define that here.  */

#include "hwint.h"

extern int		arith32_operand ();
extern int		arith_operand ();
extern int		cmp_op ();
#ifdef HOST_WIDE_INT
extern HOST_WIDE_INT	compute_frame_size ();
#endif
extern int		const_float_1_operand ();
extern void		expand_block_move ();
extern int		equality_op ();
extern void		final_prescan_insn ();
extern struct rtx_def *	function_arg ();
extern void		function_arg_advance ();
extern int		function_arg_partial_nregs ();
extern int		function_arg_pass_by_reference ();
extern void		function_epilogue ();
extern void		function_prologue ();
extern void		gen_conditional_branch ();
extern void		gen_conditional_move ();
extern struct rtx_def * gen_int_relational ();
extern void		init_cumulative_args ();
extern int		large_int ();
extern void		machine_dependent_reorg ();
extern int		mips_address_cost ();
extern void		mips_asm_file_end ();
extern void		mips_asm_file_start ();
extern int		mips_can_use_return_insn ();
extern int		mips_const_double_ok ();
extern void		mips_count_memory_refs ();
#ifdef HOST_WIDE_INT
extern HOST_WIDE_INT	mips_debugger_offset ();
#endif
extern void		mips_declare_object ();
extern int		mips_epilogue_delay_slots ();
extern void		mips_expand_epilogue ();
extern void		mips_expand_prologue ();
extern int		mips_check_split ();
extern char	       *mips_fill_delay_slot ();
extern const char       *mips_move_1word ();
extern const char       *mips_move_2words ();
extern void		mips_output_double ();
extern int		mips_output_external ();
extern void		mips_output_float ();
extern void		mips_output_filename ();
extern void		mips_output_lineno ();
extern const char       *output_block_move ();
extern void		override_options ();
extern int		pc_or_label_operand ();
extern void		print_operand_address ();
extern void		print_operand ();
extern void		print_options ();
extern int		reg_or_0_operand ();
extern int		true_reg_or_0_operand ();
extern int		simple_epilogue_p ();
extern int		simple_memory_operand ();
extern int		double_memory_operand ();
extern int		small_int ();
extern void		trace ();
extern int		uns_arith_operand ();
extern struct rtx_def *	embedded_pic_offset ();
extern void		mips_order_regs_for_local_alloc ();
extern struct rtx_def *	mips16_gp_pseudo_reg ();
extern struct rtx_def * mips16_gp_offset ();
extern int		mips16_gp_offset_p ();
extern int		mips16_constant ();
extern int		mips16_constant_after_function_p ();
extern int		build_mips16_call_stub ();

/* Recognition functions that return if a condition is true.  */
extern int		address_operand ();
extern int		call_insn_operand ();
extern int		const_double_operand ();
extern int		const_int_operand ();
extern int		consttable_operand ();
extern int		general_operand ();
extern int		immediate_operand ();
extern int		memory_address_p ();
extern int		memory_operand ();
extern int		nonimmediate_operand ();
extern int		nonmemory_operand ();
extern int		pic_address_needs_scratch ();
extern int		register_operand ();
extern int		scratch_operand ();
extern int		move_operand ();
extern int		movdi_operand ();
extern int		se_register_operand ();
extern int		se_reg_or_0_operand ();
extern int		se_uns_arith_operand ();
extern int		se_arith_operand ();
extern int		se_nonmemory_operand ();
extern int		se_nonimmediate_operand ();
extern int              extend_operator ();
extern int              highpart_shift_operator ();
extern int		m16_uimm3_b ();
extern int		m16_simm4_1 ();
extern int		m16_nsimm4_1 ();
extern int		m16_simm5_1 ();
extern int		m16_nsimm5_1 ();
extern int		m16_uimm5_4 ();
extern int		m16_nuimm5_4 ();
extern int		m16_simm8_1 ();
extern int		m16_nsimm8_1 ();
extern int		m16_uimm8_1 ();
extern int		m16_nuimm8_1 ();
extern int		m16_uimm8_m1_1 ();
extern int		m16_uimm8_4 ();
extern int		m16_nuimm8_4 ();
extern int		m16_simm8_8 ();
extern int		m16_nsimm8_8 ();
extern int		m16_usym8_4 ();
extern int		m16_usym5_4 ();

/* Functions to change what output section we are using.  */
extern void		data_section ();
extern void		rdata_section ();
extern void		readonly_data_section ();
extern void		sdata_section ();
extern void		text_section ();
extern void		mips_select_rtx_section ();
extern void		mips_select_section ();

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


/* Run-time compilation parameters selecting different hardware subsets.  */

/* Macros used in the machine description to test the flags.  */

					/* Bits for real switches */
#define MASK_INT64	0x00000001	/* ints are 64 bits */
#define MASK_LONG64	0x00000002	/* longs are 64 bits */
#define MASK_SPLIT_ADDR	0x00000004	/* Address splitting is enabled.  */
#define MASK_GPOPT	0x00000008	/* Optimize for global pointer */
#define MASK_GAS	0x00000010	/* Gas used instead of MIPS as */
#define MASK_NAME_REGS	0x00000020	/* Use MIPS s/w reg name convention */
#define MASK_STATS	0x00000040	/* print statistics to stderr */
#define MASK_MEMCPY	0x00000080	/* call memcpy instead of inline code*/
#define MASK_SOFT_FLOAT	0x00000100	/* software floating point */
#define MASK_FLOAT64	0x00000200	/* fp registers are 64 bits */
#define MASK_ABICALLS	0x00000400	/* emit .abicalls/.cprestore/.cpload */
#define MASK_HALF_PIC	0x00000800	/* Emit OSF-style pic refs to externs*/
#define MASK_LONG_CALLS	0x00001000	/* Always call through a register */
#define MASK_64BIT	0x00002000	/* Use 64 bit GP registers and insns */
#define MASK_EMBEDDED_PIC 0x00004000	/* Generate embedded PIC code */
#define MASK_EMBEDDED_DATA 0x00008000	/* Reduce RAM usage, not fast code */
#define MASK_BIG_ENDIAN	0x00010000	/* Generate big endian code */
#define MASK_SINGLE_FLOAT 0x00020000	/* Only single precision FPU.  */
#define MASK_MAD	0x00040000	/* Generate mad/madu as on 4650.  */
#define MASK_4300_MUL_FIX 0x00080000    /* Work-around early Vr4300 CPU bug */
#define MASK_MIPS3900	0x00100000	/* like -mips1 only 3900 */
#define MASK_MIPS16	0x01000000	/* Generate mips16 code */
#define MASK_NO_CHECK_ZERO_DIV 0x04000000	/* divide by zero checking */
#define MASK_CHECK_RANGE_DIV 0x08000000	/* divide result range checking */

					/* Dummy switches used only in spec's*/
#define MASK_MIPS_TFILE	0x00000000	/* flag for mips-tfile usage */

					/* Debug switches, not documented */
#define MASK_DEBUG	0		/* Eliminate version # in .s file */
#define MASK_DEBUG_A	0x40000000	/* don't allow <label>($reg) addrs */
#define MASK_DEBUG_B	0x20000000	/* GO_IF_LEGITIMATE_ADDRESS debug */
#define MASK_DEBUG_C	0x10000000	/* don't expand seq, etc. */
#define MASK_DEBUG_D	0		/* don't do define_split's */
#define MASK_DEBUG_E	0		/* function_arg debug */
#define MASK_DEBUG_F	0
#define MASK_DEBUG_G	0		/* don't support 64 bit arithmetic */
#define MASK_DEBUG_H	0               /* allow ints in FP registers */
#define MASK_DEBUG_I	0		/* unused */

					/* r4000 64 bit sizes */
#define TARGET_INT64		(target_flags & MASK_INT64)
#define TARGET_LONG64		(target_flags & MASK_LONG64)
#define TARGET_FLOAT64		(target_flags & MASK_FLOAT64)
#define TARGET_64BIT		(target_flags & MASK_64BIT)

					/* Mips vs. GNU linker */
#define TARGET_SPLIT_ADDRESSES	(target_flags & MASK_SPLIT_ADDR)

/* generate mips 3900 insns */
#define TARGET_MIPS3900         (target_flags & MASK_MIPS3900)

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

					/* software floating point */
#define TARGET_SOFT_FLOAT	(target_flags & MASK_SOFT_FLOAT)
#define TARGET_HARD_FLOAT	(! TARGET_SOFT_FLOAT)

					/* always call through a register */
#define TARGET_LONG_CALLS	(target_flags & MASK_LONG_CALLS)

					/* generate embedded PIC code;
					   requires gas.  */
#define TARGET_EMBEDDED_PIC	(target_flags & MASK_EMBEDDED_PIC)

					/* for embedded systems, optimize for
					   reduced RAM space instead of for
					   fastest code.  */
#define TARGET_EMBEDDED_DATA	(target_flags & MASK_EMBEDDED_DATA)

					/* generate big endian code.  */
#define TARGET_BIG_ENDIAN	(target_flags & MASK_BIG_ENDIAN)

#define TARGET_SINGLE_FLOAT	(target_flags & MASK_SINGLE_FLOAT)
#define TARGET_DOUBLE_FLOAT	(! TARGET_SINGLE_FLOAT)

#define TARGET_MAD		(target_flags & MASK_MAD)

#define TARGET_4300_MUL_FIX     (target_flags & MASK_4300_MUL_FIX)

#define TARGET_NO_CHECK_ZERO_DIV (target_flags & MASK_NO_CHECK_ZERO_DIV)
#define TARGET_CHECK_RANGE_DIV  (target_flags & MASK_CHECK_RANGE_DIV)

/* This is true if we must enable the assembly language file switching
   code.  */

#define TARGET_FILE_SWITCHING	(TARGET_GP_OPT && ! TARGET_GAS)

/* We must disable the function end stabs when doing the file switching trick,
   because the Lscope stabs end up in the wrong place, making it impossible
   to debug the resulting code.  */
#define NO_DBX_FUNCTION_END TARGET_FILE_SWITCHING

					/* Generate mips16 code */
#define TARGET_MIPS16		(target_flags & MASK_MIPS16)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES							\
{									\
  {"int64",		  MASK_INT64 | MASK_LONG64,			\
     "Use 64-bit int type"},						\
  {"long64",		  MASK_LONG64,					\
     "Use 64-bit long type"},						\
  {"long32",		 -(MASK_LONG64 | MASK_INT64),			\
     "Use 32-bit long type"},						\
  {"split-addresses",	  MASK_SPLIT_ADDR,				\
     "Optimize lui/addiu address loads"},				\
  {"no-split-addresses", -MASK_SPLIT_ADDR,				\
     "Don't optimize lui/addiu address loads"},				\
  {"mips-as",		 -MASK_GAS,					\
     "Use MIPS as"},							\
  {"gas",		  MASK_GAS,					\
     "Use GNU as"},							\
  {"rnames",		  MASK_NAME_REGS,				\
     "Use symbolic register names"},					\
  {"no-rnames",		 -MASK_NAME_REGS,				\
     "Don't use symbolic register names"},				\
  {"gpOPT",		  MASK_GPOPT,					\
     "Use GP relative sdata/sbss sections"},				\
  {"gpopt",		  MASK_GPOPT,					\
     "Use GP relative sdata/sbss sections"},				\
  {"no-gpOPT",		 -MASK_GPOPT,					\
     "Don't use GP relative sdata/sbss sections"},			\
  {"no-gpopt",		 -MASK_GPOPT,					\
     "Don't use GP relative sdata/sbss sections"},			\
  {"stats",		  MASK_STATS,					\
     "Output compiler statistics"},					\
  {"no-stats",		 -MASK_STATS,					\
     "Don't output compiler statistics"},				\
  {"memcpy",		  MASK_MEMCPY,					\
     "Don't optimize block moves"},					\
  {"no-memcpy",		 -MASK_MEMCPY,					\
     "Optimize block moves"},						\
  {"mips-tfile",	  MASK_MIPS_TFILE,				\
     "Use mips-tfile asm postpass"},					\
  {"no-mips-tfile",	 -MASK_MIPS_TFILE,				\
     "Don't use mips-tfile asm postpass"},				\
  {"soft-float",	  MASK_SOFT_FLOAT,				\
     "Use software floating point"},					\
  {"hard-float",	 -MASK_SOFT_FLOAT,				\
     "Use hardware floating point"},					\
  {"fp64",		  MASK_FLOAT64,					\
     "Use 64-bit FP registers"},					\
  {"fp32",		 -MASK_FLOAT64,					\
     "Use 32-bit FP registers"},					\
  {"gp64",		  MASK_64BIT,					\
     "Use 64-bit general registers"},					\
  {"gp32",		 -MASK_64BIT,					\
     "Use 32-bit general registers"},					\
  {"abicalls",		  MASK_ABICALLS,				\
     "Use Irix PIC"},							\
  {"no-abicalls",	 -MASK_ABICALLS,				\
     "Don't use Irix PIC"},						\
  {"half-pic",		  MASK_HALF_PIC,				\
     "Use OSF PIC"},							\
  {"no-half-pic",	 -MASK_HALF_PIC,				\
     "Don't use OSF PIC"},						\
  {"long-calls",	  MASK_LONG_CALLS,				\
     "Use indirect calls"},						\
  {"no-long-calls",	 -MASK_LONG_CALLS,				\
     "Don't use indirect calls"},					\
  {"embedded-pic",	  MASK_EMBEDDED_PIC,				\
     "Use embedded PIC"},						\
  {"no-embedded-pic",	 -MASK_EMBEDDED_PIC,				\
     "Don't use embedded PIC"},						\
  {"embedded-data",	  MASK_EMBEDDED_DATA,				\
     "Use ROM instead of RAM"},						\
  {"no-embedded-data",	 -MASK_EMBEDDED_DATA,				\
     "Don't use ROM instead of RAM"},					\
  {"eb",		  MASK_BIG_ENDIAN,				\
     "Use big-endian byte order"},					\
  {"el",		 -MASK_BIG_ENDIAN,				\
     "Use little-endian byte order"},					\
  {"single-float",	  MASK_SINGLE_FLOAT,				\
     "Use single (32-bit) FP only"},					\
  {"double-float",	 -MASK_SINGLE_FLOAT,				\
     "Don't use single (32-bit) FP only"},				\
  {"mad",		  MASK_MAD,					\
     "Use multiply accumulate"},					\
  {"no-mad",		 -MASK_MAD,					\
     "Don't use multiply accumulate"},					\
  {"fix4300",             MASK_4300_MUL_FIX,				\
     "Work around early 4300 hardware bug"},				\
  {"no-fix4300",         -MASK_4300_MUL_FIX,				\
     "Don't work around early 4300 hardware bug"},			\
  {"4650",		  MASK_MAD | MASK_SINGLE_FLOAT,			\
     "Optimize for 4650"},						\
  {"3900",		  MASK_MIPS3900,				\
     "Optimize for 3900"},						\
  {"check-zero-division",-MASK_NO_CHECK_ZERO_DIV,			\
     "Trap on integer divide by zero"},					\
  {"no-check-zero-division", MASK_NO_CHECK_ZERO_DIV,			\
     "Don't trap on integer divide by zero"},				\
  {"check-range-division",MASK_CHECK_RANGE_DIV,				\
     "Trap on integer divide overflow"},				\
  {"no-check-range-division",-MASK_CHECK_RANGE_DIV,			\
     "Don't trap on integer divide overflow"},				\
  {"debug",		  MASK_DEBUG,					\
     NULL},								\
  {"debuga",		  MASK_DEBUG_A,					\
     NULL},								\
  {"debugb",		  MASK_DEBUG_B,					\
     NULL},								\
  {"debugc",		  MASK_DEBUG_C,					\
     NULL},								\
  {"debugd",		  MASK_DEBUG_D,					\
     NULL},								\
  {"debuge",		  MASK_DEBUG_E,					\
     NULL},								\
  {"debugf",		  MASK_DEBUG_F,					\
     NULL},								\
  {"debugg",		  MASK_DEBUG_G,					\
     NULL},								\
  {"debugh",		  MASK_DEBUG_H,					\
     NULL},								\
  {"debugi",		  MASK_DEBUG_I,					\
     NULL},								\
  {"",			  (TARGET_DEFAULT				\
			   | TARGET_CPU_DEFAULT				\
			   | TARGET_ENDIAN_DEFAULT),			\
     NULL},								\
}     

/* Default target_flags if no switches are specified  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#ifndef TARGET_ENDIAN_DEFAULT
#ifndef DECSTATION
#define TARGET_ENDIAN_DEFAULT MASK_BIG_ENDIAN
#else
#define TARGET_ENDIAN_DEFAULT 0
#endif
#endif

#ifndef MULTILIB_DEFAULTS
#if TARGET_ENDIAN_DEFAULT == 0
#define MULTILIB_DEFAULTS { "EL", "mips1" }
#else
#define MULTILIB_DEFAULTS { "EB", "mips1" }
#endif
#endif

/* We must pass -EL to the linker by default for little endian embedded
   targets using linker scripts with a OUTPUT_FORMAT line.  Otherwise, the
   linker will default to using big-endian output files.  The OUTPUT_FORMAT
   line must be in the linker script, otherwise -EB/-EL will not work.  */

#ifndef LINKER_ENDIAN_SPEC
#if TARGET_ENDIAN_DEFAULT == 0
#define LINKER_ENDIAN_SPEC "%{!EB:%{!meb:-EL}}"
#else
#define LINKER_ENDIAN_SPEC ""
#endif
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
  SUBTARGET_TARGET_OPTIONS						\
  { "cpu=",	&mips_cpu_string,					\
      "Specify CPU for scheduling purposes"},				\
  { "ips",	&mips_isa_string,					\
      "Specify MIPS ISA"},						\
  { "entry",	&mips_entry_string,					\
      "Use mips16 entry/exit psuedo ops"},				\
  { "no-mips16", &mips_no_mips16_string,				\
      "Don't use MIPS16 instructions"},					\
  { "explicit-type-size", &mips_explicit_type_size_string,		\
      NULL},								\
}

/* This is meant to be redefined in the host dependent files.  */
#define SUBTARGET_TARGET_OPTIONS

#define GENERATE_BRANCHLIKELY  (!TARGET_MIPS16 && (TARGET_MIPS3900 || (mips_isa >= 2)))

/* Generate three-operand multiply instructions for both SImode and DImode.  */
#define GENERATE_MULT3         (TARGET_MIPS3900				\
				&& !TARGET_MIPS16)

/* Macros to decide whether certain features are available or not,
   depending on the instruction set architecture level.  */

#define BRANCH_LIKELY_P()	GENERATE_BRANCHLIKELY
#define HAVE_SQRT_P()		(mips_isa >= 2)

/* CC1_SPEC causes -mips3 and -mips4 to set -mfp64 and -mgp64; -mips1 or
   -mips2 sets -mfp32 and -mgp32.  This can be overridden by an explicit
   -mfp32, -mfp64, -mgp32 or -mgp64.  -mfp64 sets MASK_FLOAT64 in
   target_flags, and -mgp64 sets MASK_64BIT.

   Setting MASK_64BIT in target_flags will cause gcc to assume that
   registers are 64 bits wide.  int, long and void * will be 32 bit;
   this may be changed with -mint64 or -mlong64.

   The gen* programs link code that refers to MASK_64BIT.  They don't
   actually use the information in target_flags; they just refer to
   it.  */

/* Switch  Recognition by gcc.c.  Add -G xx support */

#ifdef SWITCH_TAKES_ARG
#undef SWITCH_TAKES_ARG
#endif

#define SWITCH_TAKES_ARG(CHAR)						\
  (DEFAULT_SWITCH_TAKES_ARG (CHAR) || (CHAR) == 'G')

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
	for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)	\
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
    else if (mips_isa < 4)						\
      {									\
	int regno;							\
									\
	/* We only have a single condition code register.  We		\
           implement this by hiding all the condition code registers,	\
           and generating RTL that refers directly to ST_REG_FIRST.  */	\
	for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)	\
	  fixed_regs[regno] = call_used_regs[regno] = 1;		\
      }									\
    /* In mips16 mode, we permit the $t temporary registers to be used	\
       for reload.  We prohibit the unused $s registers, since they	\
       are caller saved, and saving them via a mips16 register would	\
       probably waste more time than just reloading the value.  */	\
    if (TARGET_MIPS16)							\
      {									\
	fixed_regs[18] = call_used_regs[18] = 1;                        \
	fixed_regs[19] = call_used_regs[19] = 1;                        \
	fixed_regs[20] = call_used_regs[20] = 1;                        \
	fixed_regs[21] = call_used_regs[21] = 1;                        \
	fixed_regs[22] = call_used_regs[22] = 1;                        \
	fixed_regs[23] = call_used_regs[23] = 1;                        \
	fixed_regs[26] = call_used_regs[26] = 1;                        \
	fixed_regs[27] = call_used_regs[27] = 1;                        \
	fixed_regs[30] = call_used_regs[30] = 1;                        \
      }									\
    SUBTARGET_CONDITIONAL_REGISTER_USAGE				\
  }									\
while (0)

/* This is meant to be redefined in the host dependent files.  */
#define SUBTARGET_CONDITIONAL_REGISTER_USAGE

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

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
#define NM_FLAGS "-Bn"
#endif


/* Names to predefine in the preprocessor for this target machine.  */

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dmips -Dunix -Dhost_mips -DMIPSEB -DR3000 -DSYSTYPE_BSD43 \
-D_mips -D_unix -D_host_mips -D_MIPSEB -D_R3000 -D_SYSTYPE_BSD43 \
-Asystem(unix) -Asystem(bsd) -Acpu(mips) -Amachine(mips)"
#endif

/* Assembler specs.  */

/* MIPS_AS_ASM_SPEC is passed when using the MIPS assembler rather
   than gas.  */

#define MIPS_AS_ASM_SPEC "\
%{!.s:-nocpp} %{.s: %{cpp} %{nocpp}} \
%{pipe: %e-pipe is not supported.} \
%{K} %(subtarget_mips_as_asm_spec)"

/* SUBTARGET_MIPS_AS_ASM_SPEC is passed when using the MIPS assembler
   rather than gas.  It may be overridden by subtargets.  */

#ifndef SUBTARGET_MIPS_AS_ASM_SPEC
#define SUBTARGET_MIPS_AS_ASM_SPEC "%{v}"
#endif

/* GAS_ASM_SPEC is passed when using gas, rather than the MIPS
   assembler.  */

#define GAS_ASM_SPEC "%{mcpu=*} %{m4650} %{mmad:-m4650} %{m3900} %{v}"

/* TARGET_ASM_SPEC is used to select either MIPS_AS_ASM_SPEC or
   GAS_ASM_SPEC as the default, depending upon the value of
   TARGET_DEFAULT.  */

#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GAS) != 0
/* GAS */

#define TARGET_ASM_SPEC "\
%{mmips-as: %(mips_as_asm_spec)} \
%{!mmips-as: %(gas_asm_spec)}"

#else /* not GAS */

#define TARGET_ASM_SPEC "\
%{!mgas: %(mips_as_asm_spec)} \
%{mgas: %(gas_asm_spec)}"

#endif /* not GAS */

/* SUBTARGET_ASM_OPTIMIZING_SPEC handles passing optimization options
   to the assembler.  It may be overridden by subtargets.  */
#ifndef SUBTARGET_ASM_OPTIMIZING_SPEC
#define SUBTARGET_ASM_OPTIMIZING_SPEC "\
%{noasmopt:-O0} \
%{!noasmopt:%{O:-O2} %{O1:-O2} %{O2:-O2} %{O3:-O3}}"
#endif

/* SUBTARGET_ASM_DEBUGGING_SPEC handles passing debugging options to
   the assembler.  It may be overridden by subtargets.  */
#ifndef SUBTARGET_ASM_DEBUGGING_SPEC
#define SUBTARGET_ASM_DEBUGGING_SPEC "\
%{g} %{g0} %{g1} %{g2} %{g3} \
%{ggdb:-g} %{ggdb0:-g0} %{ggdb1:-g1} %{ggdb2:-g2} %{ggdb3:-g3} \
%{gstabs:-g} %{gstabs0:-g0} %{gstabs1:-g1} %{gstabs2:-g2} %{gstabs3:-g3} \
%{gstabs+:-g} %{gstabs+0:-g0} %{gstabs+1:-g1} %{gstabs+2:-g2} %{gstabs+3:-g3} \
%{gcoff:-g} %{gcoff0:-g0} %{gcoff1:-g1} %{gcoff2:-g2} %{gcoff3:-g3}"
#endif

/* SUBTARGET_ASM_SPEC is always passed to the assembler.  It may be
   overridden by subtargets.  */

#ifndef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC ""
#endif

/* ASM_SPEC is the set of arguments to pass to the assembler.  */

#define ASM_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{mips16:%{!mno-mips16:-mips16}} %{mno-mips16:-no-mips16} \
%(subtarget_asm_optimizing_spec) \
%(subtarget_asm_debugging_spec) \
%{membedded-pic} \
%{mabi=32:-32}%{mabi=o32:-32}%{mabi=n32:-n32}%{mabi=64:-64}%{mabi=n64:-64} \
%(target_asm_spec) \
%(subtarget_asm_spec)"

/* Specify to run a post-processor, mips-tfile after the assembler
   has run to stuff the mips debug information into the object file.
   This is needed because the $#!%^ MIPS assembler provides no way
   of specifying such information in the assembly file.  If we are
   cross compiling, disable mips-tfile unless the user specifies
   -mmips-tfile.  */

#ifndef ASM_FINAL_SPEC
#if ((TARGET_CPU_DEFAULT | TARGET_DEFAULT) & MASK_GAS) != 0
/* GAS */
#define ASM_FINAL_SPEC "\
%{mmips-as: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#else
/* not GAS */
#define ASM_FINAL_SPEC "\
%{!mgas: %{!mno-mips-tfile: \
	\n mips-tfile %{v*: -v} \
		%{K: -I %b.o~} \
		%{!K: %{save-temps: -I %b.o~}} \
		%{c:%W{o*}%{!o*:-o %b.o}}%{!c:-o %U.o} \
		%{.s:%i} %{!.s:%g.s}}}"

#endif
#endif	/* ASM_FINAL_SPEC */

/* Redefinition of libraries used.  Mips doesn't support normal
   UNIX style profiling via calling _mcount.  It does offer
   profiling that samples the PC, so do what we can... */

#ifndef LIB_SPEC
#define LIB_SPEC "%{pg:-lprof1} %{p:-lprof1} -lc"
#endif

/* Extra switches sometimes passed to the linker.  */
/* ??? The bestGnum will never be passed to the linker, because the gcc driver
  will interpret it as a -b option.  */

#ifndef LINK_SPEC
#define LINK_SPEC "\
%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3} %{mips4} \
%{bestGnum} %{shared} %{non_shared} \
%(linker_endian_spec)"
#endif	/* LINK_SPEC defined */

/* Specs for the compiler proper */

/* SUBTARGET_CC1_SPEC is passed to the compiler proper.  It may be
   overridden by subtargets.  */
#ifndef SUBTARGET_CC1_SPEC
#define SUBTARGET_CC1_SPEC ""
#endif

/* CC1_SPEC is the set of arguments to pass to the compiler proper.  */

#ifndef CC1_SPEC
#define CC1_SPEC "\
%{gline:%{!g:%{!g0:%{!g1:%{!g2: -g1}}}}} \
%{mips1:-mfp32 -mgp32} %{mips2:-mfp32 -mgp32}\
%{mips3:%{!msingle-float:%{!m4650:-mfp64}} -mgp64} \
%{mips4:%{!msingle-float:%{!m4650:-mfp64}} -mgp64} \
%{mfp64:%{msingle-float:%emay not use both -mfp64 and -msingle-float}} \
%{mfp64:%{m4650:%emay not use both -mfp64 and -m4650}} \
%{mint64|mlong64|mlong32:-mexplicit-type-size }\
%{m4650:-mcpu=r4650} \
%{m3900:-mips1 -mcpu=r3900 -mfp32 -mgp32} \
%{G*} %{EB:-meb} %{EL:-mel} %{EB:%{EL:%emay not use both -EB and -EL}} \
%{pic-none:   -mno-half-pic} \
%{pic-lib:    -mhalf-pic} \
%{pic-extern: -mhalf-pic} \
%{pic-calls:  -mhalf-pic} \
%{save-temps: } \
%(subtarget_cc1_spec) "
#endif

/* Preprocessor specs.  */

/* SUBTARGET_CPP_SIZE_SPEC defines SIZE_TYPE and PTRDIFF_TYPE.  It may
   be overridden by subtargets.  */

#ifndef SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC "\
%{mlong64:%{!mips1:%{!mips2:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int}}} \
%{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int}"
#endif

/* SUBTARGET_CPP_SPEC is passed to the preprocessor.  It may be
   overridden by subtargets.  */
#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC ""
#endif

/* If we're using 64bit longs, then we have to define __LONG_MAX__
   correctly.  Similarly for 64bit ints and __INT_MAX__.  */
#ifndef LONG_MAX_SPEC
#if ((TARGET_DEFAULT | TARGET_CPU_DEFAULT) & MASK_LONG64)
#define LONG_MAX_SPEC "%{!mlong32:-D__LONG_MAX__=9223372036854775807L}"
#else
#define LONG_MAX_SPEC "%{mlong64:-D__LONG_MAX__=9223372036854775807L}"
#endif
#endif

/* CPP_SPEC is the set of arguments to pass to the preprocessor.  */

#ifndef CPP_SPEC
#define CPP_SPEC "\
%{.cc:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.cxx:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.C:	-D__LANGUAGE_C_PLUS_PLUS -D_LANGUAGE_C_PLUS_PLUS} \
%{.m:	-D__LANGUAGE_OBJECTIVE_C -D_LANGUAGE_OBJECTIVE_C -D__LANGUAGE_C -D_LANGUAGE_C} \
%{.S:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{.s:	-D__LANGUAGE_ASSEMBLY -D_LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}} \
%{!.S: %{!.s: %{!.cc: %{!.cxx: %{!.C: %{!.m: -D__LANGUAGE_C -D_LANGUAGE_C %{!ansi:-DLANGUAGE_C}}}}}}} \
%(subtarget_cpp_size_spec) \
%{mips3:-U__mips -D__mips=3 -D__mips64} \
%{mips4:-U__mips -D__mips=4 -D__mips64} \
%{mgp32:-U__mips64} %{mgp64:-D__mips64} \
%{msingle-float:%{!msoft-float:-D__mips_single_float}} \
%{m4650:%{!msoft-float:-D__mips_single_float}} \
%{msoft-float:-D__mips_soft_float} \
%{mabi=eabi:-D__mips_eabi} \
%{mips16:%{!mno-mips16:-D__mips16}} \
%{EB:-UMIPSEL -U_MIPSEL -U__MIPSEL -U__MIPSEL__ -D_MIPSEB -D__MIPSEB -D__MIPSEB__ %{!ansi:-DMIPSEB}} \
%{EL:-UMIPSEB -U_MIPSEB -U__MIPSEB -U__MIPSEB__ -D_MIPSEL -D__MIPSEL -D__MIPSEL__ %{!ansi:-DMIPSEL}} \
%(long_max_spec) \
%(subtarget_cpp_spec) "
#endif

/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */

#define EXTRA_SPECS							\
  { "subtarget_cc1_spec", SUBTARGET_CC1_SPEC },				\
  { "subtarget_cpp_spec", SUBTARGET_CPP_SPEC },				\
  { "subtarget_cpp_size_spec", SUBTARGET_CPP_SIZE_SPEC },		\
  { "long_max_spec", LONG_MAX_SPEC },					\
  { "mips_as_asm_spec", MIPS_AS_ASM_SPEC },				\
  { "gas_asm_spec", GAS_ASM_SPEC },					\
  { "target_asm_spec", TARGET_ASM_SPEC },				\
  { "subtarget_mips_as_asm_spec", SUBTARGET_MIPS_AS_ASM_SPEC }, 	\
  { "subtarget_asm_optimizing_spec", SUBTARGET_ASM_OPTIMIZING_SPEC },	\
  { "subtarget_asm_debugging_spec", SUBTARGET_ASM_DEBUGGING_SPEC },	\
  { "subtarget_asm_spec", SUBTARGET_ASM_SPEC },				\
  { "linker_endian_spec", LINKER_ENDIAN_SPEC },				\
  SUBTARGET_EXTRA_SPECS

#ifndef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
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

#define MIPS_VERSION "[AL 1.1, MM 40]"

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
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG
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

/* Local compiler-generated symbols must have a prefix that the assembler
   understands.   By default, this is $, although some targets (e.g.,
   NetBSD-ELF) need to override this. */

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX	"$"
#endif

/* By default on the mips, external symbols do not have an underscore
   prepended, but some targets (e.g., NetBSD) require this. */

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX	""
#endif

/* Forward references to tags are allowed.  */
#define SDB_ALLOW_FORWARD_REFERENCES

/* Unknown tags are also allowed.  */
#define SDB_ALLOW_UNKNOWN_REFERENCES

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#define DBX_CONTIN_LENGTH 1500

/* How to renumber registers for dbx and gdb. */
#define DBX_REGISTER_NUMBER(REGNO) mips_dbx_regno[ (REGNO) ]

/* The mapping from gcc register number to DWARF 2 CFA column number.
   This mapping does not allow for tracking register 0, since SGI's broken
   dwarf reader thinks column 0 is used for the frame address, but since
   register 0 is fixed this is not a problem.  */
#define DWARF_FRAME_REGNUM(REG)				\
  (REG == GP_REG_FIRST + 31 ? DWARF_FRAME_RETURN_COLUMN : REG)

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN (FP_REG_LAST + 1)

/* Before the prologue, RA lives in r31.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx (REG, VOIDmode, GP_REG_FIRST + 31)

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
  fprintf (asm_out_text_file, "\t%s.def\t",		\
	   (TARGET_GAS) ? "" : "#");			\
  ASM_OUTPUT_LABELREF (asm_out_text_file, a); 		\
  fputc (';', asm_out_text_file);			\
} while (0)

#define PUT_SDB_PLAIN_DEF(a)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file, "\t%s.def\t.%s;",		\
	   (TARGET_GAS) ? "" : "#", (a));		\
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
	   "%sLb%d:\n\t%s.begin\t%sLb%d\t%d\n",		\
	   LOCAL_LABEL_PREFIX,				\
	   sdb_label_count,				\
	   (TARGET_GAS) ? "" : "#",			\
	   LOCAL_LABEL_PREFIX,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_BLOCK_END(LINE)				\
do {							\
  extern FILE *asm_out_text_file;			\
  fprintf (asm_out_text_file,				\
	   "%sLe%d:\n\t%s.bend\t%sLe%d\t%d\n",		\
	   LOCAL_LABEL_PREFIX,				\
	   sdb_label_count,				\
	   (TARGET_GAS) ? "" : "#",			\
	   LOCAL_LABEL_PREFIX,				\
	   sdb_label_count,				\
	   (LINE));					\
  sdb_label_count++;					\
} while (0)

#define PUT_SDB_FUNCTION_START(LINE)

#define PUT_SDB_FUNCTION_END(LINE)            \
do {                                                  \
  extern FILE *asm_out_text_file;             \
  ASM_OUTPUT_SOURCE_LINE (asm_out_text_file, LINE + sdb_begin_function_line); \
} while (0)

#define PUT_SDB_EPILOGUE_END(NAME)

#define PUT_SDB_SRC_FILE(FILENAME) \
do {							\
  extern FILE *asm_out_text_file;			\
  output_file_directive (asm_out_text_file, (FILENAME)); \
} while (0)

#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), ".%dfake", (NUMBER));

/* Correct the offset of automatic variables and arguments.  Note that
   the MIPS debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the MIPS 3.0 linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  */

#define DEBUGGER_AUTO_OFFSET(X)  \
  mips_debugger_offset (X, (HOST_WIDE_INT) 0)
#define DEBUGGER_ARG_OFFSET(OFFSET, X)  \
  mips_debugger_offset (X, (HOST_WIDE_INT) OFFSET)

/* Tell collect that the object format is ECOFF */
#ifndef OBJECT_FORMAT_ROSE
#define OBJECT_FORMAT_COFF	/* Object file looks like COFF */
#define EXTENDED_COFF		/* ECOFF, not normal coff */
#endif

#if 0 /* These definitions normally have no effect because
	 MIPS systems define USE_COLLECT2, so
	 assemble_constructor does nothing anyway.  */

/* Don't use the default definitions, because we don't have gld.
   Also, we don't want stabs when generating ECOFF output.
   Instead we depend on collect to handle these.  */

#define ASM_OUTPUT_CONSTRUCTOR(file, name)
#define ASM_OUTPUT_DESTRUCTOR(file, name)

#endif /* 0 */

/* Target machine storage layout */

/* Define in order to support both big and little endian float formats
   in the same gcc binary.  */
#define REAL_ARITHMETIC

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
*/
#define BITS_BIG_ENDIAN 0

/* Define this if most significant byte of a word is the lowest numbered. */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this if most significant word of a multiword number is the lowest. */
#define WORDS_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* Define this to set the endianness to use in libgcc2.c, which can
   not depend on target_flags.  */
#if !defined(MIPSEL) && !defined(__MIPSEL__)
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#endif

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD (TARGET_64BIT ? 64 : 32)
#define MAX_BITS_PER_WORD 64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#define MIN_UNITS_PER_WORD 4

/* For MIPS, width of a floating point register.  */
#define UNITS_PER_FPREG (TARGET_FLOAT64 ? 8 : 4)

/* A C expression for the size in bits of the type `int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE (TARGET_INT64 ? 64 : 32)
#define MAX_INT_TYPE_SIZE 64

/* Tell the preprocessor the maximum size of wchar_t.  */
#ifndef MAX_WCHAR_TYPE_SIZE
#ifndef WCHAR_TYPE_SIZE
#define MAX_WCHAR_TYPE_SIZE MAX_INT_TYPE_SIZE
#endif
#endif

/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE (TARGET_LONG64 ? 64 : 32)
#define MAX_LONG_TYPE_SIZE 64

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
#ifndef POINTER_SIZE
#define POINTER_SIZE (Pmode == DImode ? 64 : 32)
#endif

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY (Pmode == DImode ? 64 : 32)

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_64BIT ? 64 : 32)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
/* 8 is observed right on a DECstation and on riscos 4.02.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 64

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

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none. 

   When in 64 bit mode, mips_move_1word will sign extend SImode and CCmode
   moves.  All other referces are zero extended.  */
#define LOAD_EXTEND_OP(MODE) \
  (TARGET_64BIT && ((MODE) == SImode || (MODE) == CCmode) \
   ? SIGN_EXTEND : ZERO_EXTEND)

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.

   We promote any value smaller than SImode up to SImode.  We don't
   want to promote to DImode when in 64 bit mode, because that would
   prevent us from using the faster SImode multiply and divide
   instructions.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < 4)		\
    (MODE) = SImode;

/* Define this if function arguments should also be promoted using the above
   procedure.  */

#define PROMOTE_FUNCTION_ARGS

/* Likewise, if the function return value is promoted.  */

#define PROMOTE_FUNCTION_RETURN

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   On the Mips, we have 32 integer registers, 32 floating point
   registers, 8 condition code registers, and the special registers
   hi, lo, hilo, and rap.  The 8 condition code registers are only
   used if mips_isa >= 4.  The hilo register is only used in 64 bit
   mode.  It represents a 64 bit value stored as two 32 bit values in
   the hi and lo registers; this is the result of the mult
   instruction.  rap is a pointer to the stack where the return
   address reg ($31) was stored.  This is needed for C++ exception
   handling.  */

#define FIRST_PSEUDO_REGISTER 76

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the MIPS, see conventions, page D-2  */

#define FIXED_REGISTERS							\
{									\
  1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,			\
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1					\
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
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1					\
}


/* Internal macros to classify a register number as to whether it's a
   general purpose register, a floating point register, a
   multiply/divide register, or a status register.  */

#define GP_REG_FIRST 0
#define GP_REG_LAST  31
#define GP_REG_NUM   (GP_REG_LAST - GP_REG_FIRST + 1)
#define GP_DBX_FIRST 0

#define FP_REG_FIRST 32
#define FP_REG_LAST  63
#define FP_REG_NUM   (FP_REG_LAST - FP_REG_FIRST + 1)
#define FP_DBX_FIRST ((write_symbols == DBX_DEBUG) ? 38 : 32)

#define MD_REG_FIRST 64
#define MD_REG_LAST  66
#define MD_REG_NUM   (MD_REG_LAST - MD_REG_FIRST + 1)

#define ST_REG_FIRST 67
#define ST_REG_LAST  74
#define ST_REG_NUM   (ST_REG_LAST - ST_REG_FIRST + 1)

#define RAP_REG_NUM   75

#define AT_REGNUM	(GP_REG_FIRST + 1)
#define HI_REGNUM	(MD_REG_FIRST + 0)
#define LO_REGNUM	(MD_REG_FIRST + 1)
#define HILO_REGNUM	(MD_REG_FIRST + 2)

/* FPSW_REGNUM is the single condition code used if mips_isa < 4.  If
   mips_isa >= 4, it should not be used, and an arbitrary ST_REG
   should be used instead.  */
#define FPSW_REGNUM	ST_REG_FIRST

#define GP_REG_P(REGNO) ((unsigned) ((REGNO) - GP_REG_FIRST) < GP_REG_NUM)
#define M16_REG_P(REGNO) \
  (((REGNO) >= 2 && (REGNO) <= 7) || (REGNO) == 16 || (REGNO) == 17)
#define FP_REG_P(REGNO) ((unsigned) ((REGNO) - FP_REG_FIRST) < FP_REG_NUM)
#define MD_REG_P(REGNO) ((unsigned) ((REGNO) - MD_REG_FIRST) < MD_REG_NUM)
#define ST_REG_P(REGNO) ((unsigned) ((REGNO) - ST_REG_FIRST) < ST_REG_NUM)

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the MIPS, all general registers are one word long.  Except on
   the R4000 with the FR bit set, the floating point uses register
   pairs, with the second register not being allocable.  */

#define HARD_REGNO_NREGS(REGNO, MODE)					\
  (! FP_REG_P (REGNO)							\
	? ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD) \
	: ((GET_MODE_SIZE (MODE) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG))

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  In 32 bit mode, require that DImode and DFmode be in even
   registers.  For DImode, this makes some of the insns easier to
   write, since you don't have to worry about a DImode value in
   registers 3 & 4, producing a result in 4 & 5.

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

/* Offset from the stack pointer to the first available location.  Use
   the default value zero.  */
/* #define STACK_POINTER_OFFSET 0 */

/* Base register for access to local variables of the function.  We
   pretend that the frame pointer is $1, and then eliminate it to
   HARD_FRAME_POINTER_REGNUM.  We can get away with this because $1 is
   a fixed register, and will not be used for anything else.  */
#define FRAME_POINTER_REGNUM (GP_REG_FIRST + 1)

/* $30 is not available on the mips16, so we use $17 as the frame
   pointer.  */
#define HARD_FRAME_POINTER_REGNUM \
  (TARGET_MIPS16 ? GP_REG_FIRST + 17 : GP_REG_FIRST + 30)

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED (current_function_calls_alloca)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM GP_REG_FIRST

/* Fake register that holds the address on the stack of the
   current function's return address.  */
#define RETURN_ADDRESS_POINTER_REGNUM RAP_REG_NUM

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM (GP_REG_FIRST + 2)

/* If the structure value address is passed in a register, then
   `STRUCT_VALUE_REGNUM' should be the number of that register.  */
/* #define STRUCT_VALUE_REGNUM (GP_REG_FIRST + 4) */

/* If the structure value address is not passed in a register, define
   `STRUCT_VALUE' as an expression returning an RTX for the place
   where the address is passed.  If it returns 0, the address is
   passed as an "invisible" first argument.  */
#define STRUCT_VALUE 0

/* Mips registers used in prologue/epilogue code when the stack frame
   is larger than 32K bytes.  These registers must come from the
   scratch register set, and not used for passing and returning
   arguments and any other information used in the calling sequence
   (such as pic).  Must start at 12, since t0/t3 are parameter passing
   registers in the 64 bit ABI.  */

#define MIPS_TEMP1_REGNUM (GP_REG_FIRST + 12)
#define MIPS_TEMP2_REGNUM (GP_REG_FIRST + 13)

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

#define PIC_FUNCTION_ADDR_REGNUM (GP_REG_FIRST + 25)

/* Initialize embedded_pic_fnaddr_rtx before RTL generation for
   each function.  We used to do this in FINALIZE_PIC, but FINALIZE_PIC
   isn't always called for static inline functions.  */
#define INIT_EXPANDERS			\
do {					\
  embedded_pic_fnaddr_rtx = NULL;	\
  mips16_gp_pseudo_rtx = NULL;		\
} while (0)

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
  M16_NA_REGS,			/* mips16 regs not used to pass args */
  M16_REGS,			/* mips16 directly accessible registers */
  T_REG,			/* mips16 T register ($24) */
  M16_T_REGS,			/* mips16 registers plus T register */
  GR_REGS,			/* integer registers */
  FP_REGS,			/* floating point registers */
  HI_REG,			/* hi register */
  LO_REG,			/* lo register */
  HILO_REG,			/* hilo register pair for 64 bit mode mult */
  MD_REGS,			/* multiply/divide registers (hi/lo) */
  HI_AND_GR_REGS,		/* union classes */
  LO_AND_GR_REGS,
  HILO_AND_GR_REGS,
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
  "M16_NA_REGS",							\
  "M16_REGS",								\
  "T_REG",								\
  "M16_T_REGS",								\
  "GR_REGS",								\
  "FP_REGS",								\
  "HI_REG",								\
  "LO_REG",								\
  "HILO_REG",								\
  "MD_REGS",								\
  "HI_AND_GR_REGS",							\
  "LO_AND_GR_REGS",							\
  "HILO_AND_GR_REGS",							\
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
  { 0x0003000c, 0x00000000, 0x00000000 },	/* mips16 nonarg regs */\
  { 0x000300fc, 0x00000000, 0x00000000 },	/* mips16 registers */	\
  { 0x01000000, 0x00000000, 0x00000000 },	/* mips16 T register */	\
  { 0x010300fc, 0x00000000, 0x00000000 },	/* mips16 and T regs */ \
  { 0xffffffff, 0x00000000, 0x00000000 },	/* integer registers */	\
  { 0x00000000, 0xffffffff, 0x00000000 },	/* floating registers*/	\
  { 0x00000000, 0x00000000, 0x00000001 },	/* hi register */	\
  { 0x00000000, 0x00000000, 0x00000002 },	/* lo register */	\
  { 0x00000000, 0x00000000, 0x00000004 },	/* hilo register */	\
  { 0x00000000, 0x00000000, 0x00000003 },	/* mul/div registers */	\
  { 0xffffffff, 0x00000000, 0x00000001 },	/* union classes */     \
  { 0xffffffff, 0x00000000, 0x00000002 },				\
  { 0xffffffff, 0x00000000, 0x00000004 },				\
  { 0x00000000, 0x00000000, 0x000007f8 },	/* status registers */	\
  { 0xffffffff, 0xffffffff, 0x000007ff }	/* all registers */	\
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

#define BASE_REG_CLASS  (TARGET_MIPS16 ? M16_REGS : GR_REGS)

/* A macro whose definition is the name of the class to which a
   valid index register must belong.  An index register is one used
   in an address where its value is either multiplied by a scale
   factor or added to another register (as well as added to a
   displacement).  */

#define INDEX_REG_CLASS NO_REGS

/* When SMALL_REGISTER_CLASSES is nonzero, the compiler allows
   registers explicitly used in the rtl to be used as spill registers
   but prevents the compiler from extending the lifetime of these
   registers. */

#define SMALL_REGISTER_CLASSES (TARGET_MIPS16)

/* This macro is used later on in the file.  */
#define GR_REG_CLASS_P(CLASS)						\
  ((CLASS) == GR_REGS || (CLASS) == M16_REGS || (CLASS) == T_REG	\
   || (CLASS) == M16_T_REGS || (CLASS) == M16_NA_REGS)

/* REG_ALLOC_ORDER is to order in which to allocate registers.  This
   is the default value (allocate the registers in numeric order).  We
   define it just so that we can override it for the mips16 target in
   ORDER_REGS_FOR_LOCAL_ALLOC.  */

#define REG_ALLOC_ORDER							\
{  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,	\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,	\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63,	\
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75			\
}

/* ORDER_REGS_FOR_LOCAL_ALLOC is a macro which permits reg_alloc_order
   to be rearranged based on a particular function.  On the mips16, we
   want to allocate $24 (T_REG) before other registers for
   instructions for which it is possible.  */

#define ORDER_REGS_FOR_LOCAL_ALLOC mips_order_regs_for_local_alloc ()

/* REGISTER AND CONSTANT CLASSES */

/* Get reg_class from a letter such as appears in the machine
   description.

   DEFINED REGISTER CLASSES:

   'd'  General (aka integer) registers
        Normally this is GR_REGS, but in mips16 mode this is M16_REGS
   'y'  General registers (in both mips16 and non mips16 mode)
   'e'	mips16 non argument registers (M16_NA_REGS)
   't'  mips16 temporary register ($24)
   'f'	Floating point registers
   'h'	Hi register
   'l'	Lo register
   'x'	Multiply/divide registers
   'a'	HILO_REG
   'z'	FP Status register
   'b'	All registers */

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

   `N'	is used for negative 16 bit constants other than -65536.

   `O'	is a 15 bit signed integer.

   `P'	is used for positive 16 bit constants.  */

#define SMALL_INT(X) ((unsigned HOST_WIDE_INT) (INTVAL (X) + 0x8000) < 0x10000)
#define SMALL_INT_UNSIGNED(X) ((unsigned HOST_WIDE_INT) (INTVAL (X)) < 0x10000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
  ((C) == 'I' ? ((unsigned HOST_WIDE_INT) ((VALUE) + 0x8000) < 0x10000)	\
   : (C) == 'J' ? ((VALUE) == 0)					\
   : (C) == 'K' ? ((unsigned HOST_WIDE_INT) (VALUE) < 0x10000)		\
   : (C) == 'L' ? (((VALUE) & 0x0000ffff) == 0				\
		   && (((VALUE) & ~2147483647) == 0			\
		       || ((VALUE) & ~2147483647) == ~2147483647))	\
   : (C) == 'M' ? ((((VALUE) & ~0x0000ffff) != 0)			\
		   && (((VALUE) & ~0x0000ffff) != ~0x0000ffff)		\
		   && (((VALUE) & 0x0000ffff) != 0			\
		       || (((VALUE) & ~2147483647) != 0			\
			   && ((VALUE) & ~2147483647) != ~2147483647)))	\
   : (C) == 'N' ? ((unsigned HOST_WIDE_INT) ((VALUE) + 0xffff) < 0xffff) \
   : (C) == 'O' ? ((unsigned HOST_WIDE_INT) ((VALUE) + 0x4000) < 0x8000) \
   : (C) == 'P' ? ((VALUE) != 0 && (((VALUE) & ~0x0000ffff) == 0))	\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

/* For Mips

  'G'	: Floating point 0 */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'G'								\
   && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))

/* Letters in the range `Q' through `U' may be defined in a
   machine-dependent fashion to stand for arbitrary operand types. 
   The machine description macro `EXTRA_CONSTRAINT' is passed the
   operand as its first argument and the constraint letter as its
   second operand.

   `Q'	is for mips16 GP relative constants
   `R'	is for memory references which take 1 word for the instruction.
   `S'	is for references to extern items which are PIC for OSF/rose.
   `T'	is for memory addresses that can be used to load two words.  */

#define EXTRA_CONSTRAINT(OP,CODE)					\
  (((CODE) == 'T')	  ? double_memory_operand (OP, GET_MODE (OP))	\
   : ((CODE) == 'Q')	  ? (GET_CODE (OP) == CONST			\
			     && mips16_gp_offset_p (OP))		\
   : (GET_CODE (OP) != MEM) ? FALSE					\
   : ((CODE) == 'R')	  ? simple_memory_operand (OP, GET_MODE (OP))	\
   : ((CODE) == 'S')	  ? (HALF_PIC_P () && CONSTANT_P (OP)		\
			     && HALF_PIC_ADDRESS_P (OP))		\
   : FALSE)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)					\
  ((CLASS) != ALL_REGS							\
   ? (! TARGET_MIPS16							\
      ? (CLASS)								\
      : ((CLASS) != GR_REGS						\
	 ? (CLASS)							\
	 : M16_REGS))							\
   : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT			\
       || GET_MODE_CLASS (GET_MODE (X)) == MODE_COMPLEX_FLOAT)		\
      ? (TARGET_SOFT_FLOAT						\
	 ? (TARGET_MIPS16 ? M16_REGS : GR_REGS)				\
	 : FP_REGS)							\
      : ((GET_MODE_CLASS (GET_MODE (X)) == MODE_INT			\
	  || GET_MODE (X) == VOIDmode)					\
	 ? (TARGET_MIPS16 ? M16_REGS : GR_REGS)				\
	 : (CLASS))))

/* Certain machines have the property that some registers cannot be
   copied to some other registers without using memory.  Define this
   macro on those machines to be a C expression that is non-zero if
   objects of mode MODE in registers of CLASS1 can only be copied to
   registers of class CLASS2 by storing a register of CLASS1 into
   memory and loading that memory location into a register of CLASS2.

   Do not define this macro if its value would always be zero.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE)			\
  ((!TARGET_DEBUG_H_MODE						\
    && GET_MODE_CLASS (MODE) == MODE_INT				\
    && ((CLASS1 == FP_REGS && GR_REG_CLASS_P (CLASS2))			\
	|| (GR_REG_CLASS_P (CLASS1) && CLASS2 == FP_REGS)))		\
   || (TARGET_FLOAT64 && !TARGET_64BIT && (MODE) == DFmode		\
       && ((GR_REG_CLASS_P (CLASS1) && CLASS2 == FP_REGS)		\
	   || (GR_REG_CLASS_P (CLASS2) && CLASS1 == FP_REGS))))

/* The HI and LO registers can only be reloaded via the general
   registers.  Condition code registers can only be loaded to the
   general registers, and from the floating point registers.  */

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  mips_secondary_reload_class (CLASS, MODE, X, 1)
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)			\
  mips_secondary_reload_class (CLASS, MODE, X, 0)

/* Not declared above, with the other functions, because enum
   reg_class is not declared yet.  */
extern enum reg_class	mips_secondary_reload_class ();

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */

#define CLASS_UNITS(mode, size)						\
  ((GET_MODE_SIZE (mode) + (size) - 1) / (size))

#define CLASS_MAX_NREGS(CLASS, MODE)					\
  ((CLASS) == FP_REGS							\
   ? (TARGET_FLOAT64							\
      ? CLASS_UNITS (MODE, 8)						\
      : 2 * CLASS_UNITS (MODE, 8))					\
   : CLASS_UNITS (MODE, UNITS_PER_WORD))

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
#define STARTING_FRAME_OFFSET						\
  (current_function_outgoing_args_size					\
   + (TARGET_ABICALLS ? MIPS_STACK_ALIGN (UNITS_PER_WORD) : 0))

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

/* The return address for the current frame is in r31 is this is a leaf
   function.  Otherwise, it is on the stack.  It is at a variable offset
   from sp/fp/ap, so we define a fake hard register rap which is a
   poiner to the return address on the stack.  This always gets eliminated
   during reload to be either the frame pointer or the stack pointer plus
   an offset.  */

/* ??? This definition fails for leaf functions.  There is currently no
   general solution for this problem.  */

/* ??? There appears to be no way to get the return address of any previous
   frame except by disassembling instructions in the prologue/epilogue.
   So currently we support only the current frame.  */

#define RETURN_ADDR_RTX(count, frame)			\
  ((count == 0)						\
   ? gen_rtx (MEM, Pmode, gen_rtx (REG, Pmode, RETURN_ADDRESS_POINTER_REGNUM))\
   : (rtx) 0)

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
  long insns_len;		/* length of insns; mips16 only */
};

extern struct mips_frame_info current_frame_info;

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
   pointer is specified first since that is the preferred elimination.

   The eliminations to $17 are only used on the mips16.  See the
   definition of HARD_FRAME_POINTER_REGNUM.  */

#define ELIMINABLE_REGS							\
{{ ARG_POINTER_REGNUM,   STACK_POINTER_REGNUM},				\
 { ARG_POINTER_REGNUM,   GP_REG_FIRST + 30},				\
 { ARG_POINTER_REGNUM,   GP_REG_FIRST + 17},				\
 { RETURN_ADDRESS_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
 { RETURN_ADDRESS_POINTER_REGNUM, GP_REG_FIRST + 30},			\
 { RETURN_ADDRESS_POINTER_REGNUM, GP_REG_FIRST + 17},			\
 { RETURN_ADDRESS_POINTER_REGNUM, GP_REG_FIRST + 31},			\
 { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},				\
 { FRAME_POINTER_REGNUM, GP_REG_FIRST + 30},				\
 { FRAME_POINTER_REGNUM, GP_REG_FIRST + 17}}

/* A C expression that returns non-zero if the compiler is allowed to
   try to replace register number FROM-REG with register number
   TO-REG.  This macro need only be defined if `ELIMINABLE_REGS' is
   defined, and will usually be the constant 1, since most of the
   cases preventing register elimination are things that the compiler
   already knows about.

   When not in mips16 and mips64, we can always eliminate to the
   frame pointer.  We can eliminate to the stack pointer unless
   a frame pointer is needed.  In mips16 mode, we need a frame
   pointer for a large frame; otherwise, reload may be unable
   to compute the address of a local variable, since there is
   no way to add a large constant to the stack pointer
   without using a temporary register.

   In mips16, for some instructions (eg lwu), we can't eliminate the
   frame pointer for the stack pointer.  These instructions are
   only generated in TARGET_64BIT mode.
   */

#define CAN_ELIMINATE(FROM, TO)						\
  (((FROM) == RETURN_ADDRESS_POINTER_REGNUM && (! leaf_function_p ()	\
   || (TO == GP_REG_FIRST + 31 && leaf_function_p)))   			\
  || ((FROM) != RETURN_ADDRESS_POINTER_REGNUM				\
   && ((TO) == HARD_FRAME_POINTER_REGNUM 				\
   || ((TO) == STACK_POINTER_REGNUM && ! frame_pointer_needed		\
       && ! (TARGET_MIPS16 && TARGET_64BIT)                             \
       && (! TARGET_MIPS16						\
	   || compute_frame_size (get_frame_size ()) < 32768)))))

/* This macro is similar to `INITIAL_FRAME_POINTER_OFFSET'.  It
   specifies the initial difference between the specified pair of
   registers.  This macro must be defined if `ELIMINABLE_REGS' is
   defined.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			 \
{  compute_frame_size (get_frame_size ());				 \
  if (TARGET_MIPS16 && (FROM) == FRAME_POINTER_REGNUM			 \
      && (TO) == HARD_FRAME_POINTER_REGNUM)				 \
    (OFFSET) = - current_function_outgoing_args_size;			 \
  else if ((FROM) == FRAME_POINTER_REGNUM)				 \
    (OFFSET) = 0;							 \
  else if (TARGET_MIPS16 && (FROM) == ARG_POINTER_REGNUM		 \
	   && (TO) == HARD_FRAME_POINTER_REGNUM)			 \
    (OFFSET) = (current_frame_info.total_size				 \
		- current_function_outgoing_args_size			 \
		- ((mips_abi != ABI_32 					 \
		    && mips_abi != ABI_O64				 \
		    && mips_abi != ABI_EABI)				 \
		   ? current_function_pretend_args_size			 \
		   : 0));						 \
  else if ((FROM) == ARG_POINTER_REGNUM)				 \
    (OFFSET) = (current_frame_info.total_size				 \
		- ((mips_abi != ABI_32 					 \
		    && mips_abi != ABI_O64				 \
		    && mips_abi != ABI_EABI)				 \
		   ? current_function_pretend_args_size			 \
		   : 0));						 \
  /* Some ABIs store 64 bits to the stack, but Pmode is 32 bits,	 \
     so we must add 4 bytes to the offset to get the right value.  */	 \
  else if ((FROM) == RETURN_ADDRESS_POINTER_REGNUM)			 \
  {									 \
   if (leaf_function_p ()) 						 \
      (OFFSET) = 0;				 			 \
   else (OFFSET) = current_frame_info.gp_sp_offset			 \
	       + ((UNITS_PER_WORD - (POINTER_SIZE / BITS_PER_UNIT))	 \
		  * (BYTES_BIG_ENDIAN != 0));				 \
  }									 \
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
   returning a structure or a union, to account for its address being
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

#define REG_PARM_STACK_SPACE(FNDECL)	\
  ((MAX_ARGS_IN_REGISTERS*UNITS_PER_WORD) - FIRST_PARM_OFFSET (FNDECL))

/* Define this if it is the responsibility of the caller to
   allocate the area reserved for arguments passed in registers. 
   If `ACCUMULATE_OUTGOING_ARGS' is also defined, the only effect
   of this macro is to determine whether the space is included in 
   `current_function_outgoing_args_size'.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* Align stack frames on 64 bits (Double Word ).  */
#ifndef STACK_BOUNDARY
#define STACK_BOUNDARY 64
#endif

/* Make sure 4 words are always allocated on the stack.  */

#ifndef STACK_ARGS_ADJUST
#define STACK_ARGS_ADJUST(SIZE)						\
{									\
  if (SIZE.constant < 4 * UNITS_PER_WORD)				\
    SIZE.constant = 4 * UNITS_PER_WORD;					\
}
#endif


/* A C expression that should indicate the number of bytes of its
   own arguments that a function pops on returning, or 0
   if the function pops no arguments and the caller must therefore
   pop them all after the function returns.

   FUNDECL is the declaration node of the function (as a tree).

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

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0


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
   assuming the value has mode MODE.  Because we define
   PROMOTE_FUNCTION_RETURN, we must promote the mode just as
   PROMOTE_MODE does.  */

#define LIBCALL_VALUE(MODE)						\
  gen_rtx (REG,								\
	   ((GET_MODE_CLASS (MODE) != MODE_INT				\
	     || GET_MODE_SIZE (MODE) >= 4)				\
	    ? (MODE)							\
	    : SImode),							\
	   ((GET_MODE_CLASS (MODE) == MODE_FLOAT			\
	     && (! TARGET_SINGLE_FLOAT					\
		 || GET_MODE_SIZE (MODE) <= 4))				\
	    ? FP_RETURN							\
	    : GP_RETURN))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) LIBCALL_VALUE (TYPE_MODE (VALTYPE))


/* 1 if N is a possible register number for a function value.
   On the MIPS, R2 R3 and F0 F2 are the only register thus used.
   Currently, R2 and F0 are only implemented  here (C has no complex type)  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == GP_RETURN || (N) == FP_RETURN)

/* 1 if N is a possible register number for function argument passing.
   We have no FP argument registers when soft-float.  When FP registers
   are 32 bits, we can't directly reference the odd numbered ones.  */

#define FUNCTION_ARG_REGNO_P(N)					\
  (((N) >= GP_ARG_FIRST && (N) <= GP_ARG_LAST)			\
   || ((! TARGET_SOFT_FLOAT					\
       && ((N) >= FP_ARG_FIRST && (N) <= FP_ARG_LAST)		\
       && (TARGET_FLOAT64 || (0 == (N) % 2)))			\
       && ! fixed_regs[N]))

/* A C expression which can inhibit the returning of certain function
   values in registers, based on the type of value.  A nonzero value says
   to return the function value in memory, just as large structures are
   always returned.  Here TYPE will be a C expression of type
   `tree', representing the data type of the value.

   Note that values of mode `BLKmode' must be explicitly
   handled by this macro.  Also, the option `-fpcc-struct-return'
   takes effect regardless of this macro.  On most systems, it is
   possible to leave the macro undefined; this causes a default
   definition to be used, whose value is the constant 1 for BLKmode
   values, and 0 otherwise.

   GCC normally converts 1 byte structures into chars, 2 byte
   structs into shorts, and 4 byte structs into ints, and returns
   them this way.  Defining the following macro overrides this,
   to give us MIPS cc compatibility.  */

#define RETURN_IN_MEMORY(TYPE)	\
  (TYPE_MODE (TYPE) == BLKmode)

/* A code distinguishing the floating point format of the target
   machine.  There are three defined values: IEEE_FLOAT_FORMAT,
   VAX_FLOAT_FORMAT, and UNKNOWN_FLOAT_FORMAT.  */

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the mips16, we need to keep track of which floating point
   arguments were passed in general registers, but would have been
   passed in the FP regs if this were a 32 bit function, so that we
   can move them to the FP regs if we wind up calling a 32 bit
   function.  We record this information in fp_code, encoded in base
   four.  A zero digit means no floating point argument, a one digit
   means an SFmode argument, and a two digit means a DFmode argument,
   and a three digit is not used.  The low order digit is the first
   argument.  Thus 6 == 1 * 4 + 2 means a DFmode argument followed by
   an SFmode argument.  ??? A more sophisticated approach will be
   needed if MIPS_ABI != ABI_32.  */

typedef struct mips_args {
  int gp_reg_found;		/* whether a gp register was found yet */
  int arg_number;		/* argument number */
  int arg_words;		/* # total words the arguments take */
  int fp_arg_words;		/* # words for FP args (MIPS_EABI only) */
  int last_arg_fp;		/* nonzero if last arg was FP (EABI only) */
  int fp_code;			/* Mode of FP arguments (mips16) */
  int num_adjusts;		/* number of adjustments made */
				/* Adjustments made to args pass in regs.  */
				/* ??? The size is doubled to work around a 
				   bug in the code that sets the adjustments
				   in function_arg.  */
  struct rtx_def *adjust[MAX_ARGS_IN_REGISTERS*2];
} CUMULATIVE_ARGS;

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

*/

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT)		\
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
	? ((TYPE_ALIGN(TYPE) <= (unsigned)PARM_BOUNDARY)		\
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

/* Tell prologue and epilogue if register REGNO should be saved / restored.  */

#define MUST_SAVE_REGISTER(regno) \
 ((regs_ever_live[regno] && !call_used_regs[regno])			\
  || (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)	\
  || (regno == (GP_REG_FIRST + 31) && regs_ever_live[GP_REG_FIRST + 31]))

/* ALIGN FRAMES on double word boundaries */
#ifndef MIPS_STACK_ALIGN
#define MIPS_STACK_ALIGN(LOC) (((LOC) + 7) & ~7)
#endif


/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)				\
{									\
  if (TARGET_MIPS16)							\
    sorry ("mips16 function profiling");				\
  fprintf (FILE, "\t.set\tnoreorder\n");				\
  fprintf (FILE, "\t.set\tnoat\n");					\
  fprintf (FILE, "\tmove\t%s,%s\t\t# save current return address\n",	\
	   reg_names[GP_REG_FIRST + 1], reg_names[GP_REG_FIRST + 31]);	\
  fprintf (FILE, "\tjal\t_mcount\n");					\
  fprintf (FILE,							\
	   "\t%s\t%s,%s,%d\t\t# _mcount pops 2 words from  stack\n",	\
	   TARGET_64BIT ? "dsubu" : "subu",				\
	   reg_names[STACK_POINTER_REGNUM],				\
	   reg_names[STACK_POINTER_REGNUM],				\
	   Pmode == DImode ? 16 : 8);					\
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
  if (Pmode == DImode)							\
    {									\
      fprintf (STREAM, "\t.word\t0xdfe30014\t\t# ld     $3,20($31)\n");	\
      fprintf (STREAM, "\t.word\t0xdfe2001c\t\t# ld     $2,28($31)\n");	\
    }									\
  else									\
    {									\
      fprintf (STREAM, "\t.word\t0x8fe30014\t\t# lw     $3,20($31)\n");	\
      fprintf (STREAM, "\t.word\t0x8fe20018\t\t# lw     $2,24($31)\n");	\
    }									\
  fprintf (STREAM, "\t.word\t0x0060c821\t\t# move   $25,$3 (abicalls)\n"); \
  fprintf (STREAM, "\t.word\t0x00600008\t\t# jr     $3\n");		\
  fprintf (STREAM, "\t.word\t0x0020f821\t\t# move   $31,$1\n");		\
  if (Pmode == DImode)							\
    {									\
      fprintf (STREAM, "\t.dword\t0x00000000\t\t# <function address>\n"); \
      fprintf (STREAM, "\t.dword\t0x00000000\t\t# <static chain value>\n"); \
    }									\
  else									\
    {									\
      fprintf (STREAM, "\t.word\t0x00000000\t\t# <function address>\n"); \
      fprintf (STREAM, "\t.word\t0x00000000\t\t# <static chain value>\n"); \
    }									\
}

/* A C expression for the size in bytes of the trampoline, as an
   integer.  */

#define TRAMPOLINE_SIZE (32 + (Pmode == DImode ? 16 : 8))

/* Alignment required for trampolines, in bits.  */

#define TRAMPOLINE_ALIGNMENT (Pmode == DImode ? 64 : 32)

/* INITIALIZE_TRAMPOLINE calls this library function to flush
   program and data caches.  */

#ifndef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "_flush_cache"
#endif

/* A C statement to initialize the variable parts of a trampoline. 
   ADDR is an RTX for the address of the trampoline; FNADDR is an
   RTX for the address of the nested function; STATIC_CHAIN is an
   RTX for the static chain value that should be passed to the
   function when it is called.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FUNC, CHAIN)			    \
{									    \
  rtx addr = ADDR;							    \
  if (Pmode == DImode)							    \
    {									    \
      emit_move_insn (gen_rtx (MEM, DImode, plus_constant (addr, 32)), FUNC); \
      emit_move_insn (gen_rtx (MEM, DImode, plus_constant (addr, 40)), CHAIN);\
    }									    \
  else									    \
    {									    \
      emit_move_insn (gen_rtx (MEM, SImode, plus_constant (addr, 32)), FUNC); \
      emit_move_insn (gen_rtx (MEM, SImode, plus_constant (addr, 36)), CHAIN);\
    }									    \
									    \
  /* Flush both caches.  We need to flush the data cache in case	    \
     the system has a write-back cache.  */				    \
  /* ??? Should check the return value for errors.  */			    \
  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, CACHE_FLUSH_FUNC),	    \
		     0, VOIDmode, 3, addr, Pmode,			    \
		     GEN_INT (TRAMPOLINE_SIZE), TYPE_MODE (integer_type_node),\
		     GEN_INT (3), TYPE_MODE (integer_type_node));	    \
}

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT 0 */
/* #define HAVE_POST_DECREMENT 0 */

/* #define HAVE_PRE_DECREMENT 0 */
/* #define HAVE_PRE_INCREMENT 0 */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define BASE_REG_P(regno, mode)					\
  (TARGET_MIPS16						\
   ? (M16_REG_P (regno)						\
      || (regno) == FRAME_POINTER_REGNUM			\
      || (regno) == ARG_POINTER_REGNUM				\
      || ((regno) == STACK_POINTER_REGNUM			\
	  && (GET_MODE_SIZE (mode) == 4				\
	      || GET_MODE_SIZE (mode) == 8)))			\
   : GP_REG_P (regno))

#define GP_REG_OR_PSEUDO_STRICT_P(regno, mode)				    \
  BASE_REG_P((regno < FIRST_PSEUDO_REGISTER) ? regno : reg_renumber[regno], \
	     (mode))

#define GP_REG_OR_PSEUDO_NONSTRICT_P(regno, mode) \
  (((regno) >= FIRST_PSEUDO_REGISTER) || (BASE_REG_P ((regno), (mode))))

#define REGNO_OK_FOR_INDEX_P(regno)	0
#define REGNO_MODE_OK_FOR_BASE_P(regno, mode) \
  GP_REG_OR_PSEUDO_STRICT_P ((regno), (mode))

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
#define REG_OK_FOR_INDEX_P(X) 0
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  GP_REG_OR_PSEUDO_NONSTRICT_P (REGNO (X), (MODE))

#else

#define REG_OK_STRICT_P 1
#define REG_OK_FOR_INDEX_P(X) 0
#define REG_MODE_OK_FOR_BASE_P(X, MODE) \
  REGNO_MODE_OK_FOR_BASE_P (REGNO (X), (MODE))

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
  /* The mips16 can only use the stack pointer as a base register when	\
     loading SImode or DImode values.  */				\
  if (GET_CODE (xinsn) == REG && REG_MODE_OK_FOR_BASE_P (xinsn, MODE))	\
    goto ADDR;								\
									\
  if (CONSTANT_ADDRESS_P (xinsn)					\
      && ! (mips_split_addresses && mips_check_split (xinsn, MODE))	\
      && (! TARGET_MIPS16 || mips16_constant (xinsn, MODE, 1, 0)))	\
    goto ADDR;								\
									\
  if (GET_CODE (xinsn) == LO_SUM && mips_split_addresses)		\
    {									\
      register rtx xlow0 = XEXP (xinsn, 0);				\
      register rtx xlow1 = XEXP (xinsn, 1);				\
									\
      if (GET_CODE (xlow0) == REG					\
	  && REG_MODE_OK_FOR_BASE_P (xlow0, MODE)			\
	  && mips_check_split (xlow1, MODE))				\
	goto ADDR;							\
    }									\
									\
  if (GET_CODE (xinsn) == PLUS)						\
    {									\
      register rtx xplus0 = XEXP (xinsn, 0);				\
      register rtx xplus1 = XEXP (xinsn, 1);				\
      register enum rtx_code code0 = GET_CODE (xplus0);			\
      register enum rtx_code code1 = GET_CODE (xplus1);			\
									\
      /* The mips16 can only use the stack pointer as a base register	\
         when loading SImode or DImode values.  */			\
      if (code0 == REG && REG_MODE_OK_FOR_BASE_P (xplus0, MODE))	\
	{								\
	  if (code1 == CONST_INT					\
	      && INTVAL (xplus1) >= -32768				\
	      && INTVAL (xplus1) + GET_MODE_SIZE (MODE) - 1 <= 32767)	\
	    goto ADDR;							\
									\
	  /* On the mips16, we represent GP relative offsets in RTL.	\
             These are 16 bit signed values, and can serve as register	\
             offsets.  */						\
	  if (TARGET_MIPS16						\
	      && mips16_gp_offset_p (xplus1))				\
	    goto ADDR;							\
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
	  /* Also accept CONST_INT addresses here, so no else.  */	\
	  /* Reject combining an embedded PIC text segment reference	\
	     with a register.  That requires an additional		\
	     instruction.  */						\
          /* ??? Reject combining an address with a register for the MIPS  \
	     64 bit ABI, because the SGI assembler can not handle this.  */ \
	  if (!TARGET_DEBUG_A_MODE					\
	      && (mips_abi == ABI_32					\
		  || mips_abi == ABI_O64				\
		  || mips_abi == ABI_EABI)				\
	      && CONSTANT_ADDRESS_P (xplus1)				\
	      && ! mips_split_addresses					\
	      && (!TARGET_EMBEDDED_PIC					\
		  || code1 != CONST					\
		  || GET_CODE (XEXP (xplus1, 0)) != MINUS)		\
	      && !TARGET_MIPS16)					\
	    goto ADDR;							\
	}								\
    }									\
									\
  if (TARGET_DEBUG_B_MODE)						\
    GO_PRINTF ("Not a legitimate address\n");				\
}


/* A C expression that is 1 if the RTX X is a constant which is a
   valid address.  This is defined to be the same as `CONSTANT_P (X)',
   but rejecting CONST_DOUBLE.  */
/* When pic, we must reject addresses of the form symbol+large int.
   This is because an instruction `sw $4,s+70000' needs to be converted
   by the assembler to `lw $at,s($gp);sw $4,70000($at)'.  Normally the
   assembler would use $at as a temp to load in the large offset.  In this
   case $at is already in use.  We convert such problem addresses to
   `la $5,s;sw $4,70000($5)' via LEGITIMIZE_ADDRESS.  */
/* ??? SGI Irix 6 assembler fails for CONST address, so reject them.  */
#define CONSTANT_ADDRESS_P(X)						\
  ((GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
    || GET_CODE (X) == CONST_INT || GET_CODE (X) == HIGH		\
    || (GET_CODE (X) == CONST						\
	&& ! (flag_pic && pic_address_needs_scratch (X))		\
	&& (mips_abi == ABI_32						\
	    || mips_abi == ABI_O64					\
	    || mips_abi == ABI_EABI)))					\
   && (!HALF_PIC_P () || !HALF_PIC_ADDRESS_P (X)))

/* Define this, so that when PIC, reload won't try to reload invalid
   addresses which require two reload registers.  */

#define LEGITIMATE_PIC_OPERAND_P(X)  (! pic_address_needs_scratch (X))

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   At present, GAS doesn't understand li.[sd], so don't allow it
   to be generated at present.  Also, the MIPS assembler does not
   grok li.d Infinity.  */

/* ??? SGI Irix 6 assembler fails for CONST address, so reject them.  */
#define LEGITIMATE_CONSTANT_P(X)					\
  ((GET_CODE (X) != CONST_DOUBLE					\
    || mips_const_double_ok (X, GET_MODE (X)))				\
   && ! (GET_CODE (X) == CONST						\
	 && mips_abi != ABI_32 						\
	 && mips_abi != ABI_O64 					\
         && mips_abi != ABI_EABI)					\
   && (! TARGET_MIPS16 || mips16_constant (X, GET_MODE (X), 0, 0)))

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

   This is for CSE to find several similar references, and only use one Z.

   When PIC, convert addresses of the form memory (symbol+large int) to
   memory (reg+large int).  */
   

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
  if (mips_split_addresses && mips_check_split (X, MODE))		\
    {									\
      /* ??? Is this ever executed?  */					\
      X = gen_rtx (LO_SUM, Pmode,					\
		   copy_to_mode_reg (Pmode, gen_rtx (HIGH, Pmode, X)), X); \
      goto WIN;								\
    }									\
									\
  if (GET_CODE (xinsn) == CONST						\
      && ((flag_pic && pic_address_needs_scratch (xinsn))		\
	  /* ??? SGI's Irix 6 assembler can't handle CONST.  */		\
	  || (mips_abi != ABI_32 					\
	      && mips_abi != ABI_O64					\
	      && mips_abi != ABI_EABI)))				\
    {									\
      rtx ptr_reg = gen_reg_rtx (Pmode);				\
      rtx constant = XEXP (XEXP (xinsn, 0), 1);				\
									\
      emit_move_insn (ptr_reg, XEXP (XEXP (xinsn, 0), 0));		\
									\
      X = gen_rtx (PLUS, Pmode, ptr_reg, constant);			\
      if (SMALL_INT (constant))						\
	goto WIN;							\
      /* Otherwise we fall through so the code below will fix the	\
	 constant.  */							\
      xinsn = X;							\
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
      if (code0 == REG && REG_MODE_OK_FOR_BASE_P (xplus0, MODE)		\
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
   `PRINT_OPERAND_ADDRESS'.

   When optimizing for the $gp pointer, SYMBOL_REF_FLAG is set for all
   small objects.

   When generating embedded PIC code, SYMBOL_REF_FLAG is set for
   symbols which are not in the .text section.

   When generating mips16 code, SYMBOL_REF_FLAG is set for string
   constants which are put in the .text section.  We also record the
   total length of all such strings; this total is used to decide
   whether we need to split the constant table, and need not be
   precisely correct. 

   When not mips16 code nor embedded PIC, if a symbol is in a
   gp addresable section, SYMBOL_REF_FLAG is set prevent gcc from
   splitting the reference so that gas can generate a gp relative
   reference.

   When TARGET_EMBEDDED_DATA is set, we assume that all const
   variables will be stored in ROM, which is too far from %gp to use
   %gprel addressing.  Note that (1) we include "extern const"
   variables in this, which mips_select_section doesn't, and (2) we
   can't always tell if they're really const (they might be const C++
   objects with non-const constructors), so we err on the side of
   caution and won't use %gprel anyway (otherwise we'd have to defer
   this decision to the linker/loader).  The handling of extern consts
   is why the DECL_INITIAL macros differ from mips_select_section.

   If you are changing this macro, you should look at
   mips_select_section and see if it needs a similar change.  */

#ifndef UNIQUE_SECTION_P
#define UNIQUE_SECTION_P(DECL) (0)
#endif

#define ENCODE_SECTION_INFO(DECL)					\
do									\
  {									\
    if (TARGET_MIPS16)							\
      {									\
	if (TREE_CODE (DECL) == STRING_CST				\
	    && ! flag_writable_strings					\
	    /* If this string is from a function, and the function will	\
	       go in a gnu linkonce section, then we can't directly	\
	       access the string.  This gets an assembler error		\
	       "unsupported PC relative reference to different section".\
	       If we modify SELECT_SECTION to put it in function_section\
	       instead of text_section, it still fails because		\
	       DECL_SECTION_NAME isn't set until assemble_start_function.\
	       If we fix that, it still fails because strings are shared\
	       among multiple functions, and we have cross section	\
	       references again.  We force it to work by putting string	\
	       addresses in the constant pool and indirecting.  */	\
	    && (! current_function_decl					\
		|| ! UNIQUE_SECTION_P (current_function_decl)))		\
	  {								\
	    SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (DECL), 0)) = 1;	\
	    mips_string_length += TREE_STRING_LENGTH (DECL);		\
	  }								\
      }									\
									\
    if (TARGET_EMBEDDED_DATA						\
	&& (TREE_CODE (DECL) == VAR_DECL				\
	    && TREE_READONLY (DECL) && !TREE_SIDE_EFFECTS (DECL))	\
	    && (!DECL_INITIAL (DECL)					\
		|| TREE_CONSTANT (DECL_INITIAL (DECL))))		\
      {									\
	SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 0;		\
      }									\
									\
    else if (TARGET_EMBEDDED_PIC)					\
      {									\
        if (TREE_CODE (DECL) == VAR_DECL)				\
	  SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;		\
        else if (TREE_CODE (DECL) == FUNCTION_DECL)			\
	  SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 0;		\
	else if (TREE_CODE (DECL) == STRING_CST				\
		 && ! flag_writable_strings)				\
	  SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (DECL), 0)) = 0;		\
        else								\
	  SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (DECL), 0)) = 1;		\
      }									\
									\
    else if (TREE_CODE (DECL) == VAR_DECL				\
             && DECL_SECTION_NAME (DECL) != NULL_TREE                   \
             && (0 == strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (DECL)), \
                              ".sdata")                                 \
                || 0 == strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (DECL)),\
                              ".sbss")))                                \
      {									\
        SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;		\
      }									\
									\
    /* We can not perform GP optimizations on variables which are in	\
       specific sections, except for .sdata and .sbss which are		\
       handled above.  */						\
    else if (TARGET_GP_OPT && TREE_CODE (DECL) == VAR_DECL		\
	     && DECL_SECTION_NAME (DECL) == NULL_TREE)			\
      {									\
	int size = int_size_in_bytes (TREE_TYPE (DECL));		\
									\
	if (size > 0 && size <= mips_section_threshold)			\
	  SYMBOL_REF_FLAG (XEXP (DECL_RTL (DECL), 0)) = 1;		\
      }									\
									\
    else if (HALF_PIC_P ())						\
      {									\
        HALF_PIC_ENCODE (DECL);						\
      }									\
  }									\
while (0)

/* The mips16 wants the constant pool to be after the function,
   because the PC relative load instructions use unsigned offsets.  */

#define CONSTANT_POOL_BEFORE_FUNCTION (! TARGET_MIPS16)

#define ASM_OUTPUT_POOL_EPILOGUE(FILE, FNNAME, FNDECL, SIZE)	\
  mips_string_length = 0;

#if 0
/* In mips16 mode, put most string constants after the function.  */
#define CONSTANT_AFTER_FUNCTION_P(tree)				\
  (TARGET_MIPS16 && mips16_constant_after_function_p (tree))
#endif

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.
   ??? Using HImode in mips16 mode can cause overflow.  However, the
   overflow is no more likely than the overflow in a branch
   instruction.  Large functions can currently break in both ways.  */
#define CASE_VECTOR_MODE \
  (TARGET_MIPS16 ? HImode : Pmode == DImode ? DImode : SImode)

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
#define CASE_VECTOR_PC_RELATIVE (TARGET_MIPS16)

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
#define MOVE_MAX (TARGET_64BIT ? 8 : 4)
#define MAX_MOVE_MAX 8

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

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits. */
#define SHIFT_COUNT_TRUNCATED 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
/* In 64 bit mode, 32 bit instructions require that register values be properly
   sign-extended to 64 bits.  As a result, a truncate is not a no-op if it
   converts a value >32 bits to a value <32 bits.  */
/* ??? This results in inefficient code for 64 bit to 32 conversions.
   Something needs to be done about this.  Perhaps not use any 32 bit
   instructions?  Perhaps use PROMOTE_MODE?  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) \
  (TARGET_64BIT ? ((INPREC) <= 32 || (OUTPREC) > 32) : 1)

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.

   For MIPS we make pointers are the smaller of longs and gp-registers. */

#ifndef Pmode
#define Pmode ((TARGET_LONG64 && TARGET_64BIT) ? DImode : SImode)
#endif

/* A function address in a call instruction
   is a word address (for indexing purposes)
   so give the MEM rtx a words's mode.  */

#define FUNCTION_MODE (Pmode == DImode ? DImode : SImode)

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
    if (! TARGET_MIPS16)						\
      {									\
	/* Always return 0, since we don't have different sized		\
	   instructions, hence different costs according to Richard	\
	   Kenner */							\
	return 0;							\
      }									\
    if ((OUTER_CODE) == SET)						\
      {									\
	if (INTVAL (X) >= 0 && INTVAL (X) < 0x100)			\
	  return 0;							\
	else if ((INTVAL (X) >= 0 && INTVAL (X) < 0x10000)		\
		 || (INTVAL (X) < 0 && INTVAL (X) > -0x100))		\
	  return COSTS_N_INSNS (1);					\
	else								\
	  return COSTS_N_INSNS (2);					\
      }									\
    /* A PLUS could be an address.  We don't want to force an address	\
       to use a register, so accept any signed 16 bit value without	\
       complaint.  */							\
    if ((OUTER_CODE) == PLUS						\
	&& INTVAL (X) >= -0x8000 && INTVAL (X) < 0x8000)		\
      return 0;								\
    /* A number between 1 and 8 inclusive is efficient for a shift.	\
       Otherwise, we will need an extended instruction.  */		\
    if ((OUTER_CODE) == ASHIFT || (OUTER_CODE) == ASHIFTRT		\
	|| (OUTER_CODE) == LSHIFTRT)					\
      {									\
	if (INTVAL (X) >= 1 && INTVAL (X) <= 8)				\
	  return 0;							\
	return COSTS_N_INSNS (1);					\
      }									\
    /* We can use cmpi for an xor with an unsigned 16 bit value.  */	\
    if ((OUTER_CODE) == XOR						\
	&& INTVAL (X) >= 0 && INTVAL (X) < 0x10000)			\
      return 0;								\
    /* We may be able to use slt or sltu for a comparison with a	\
       signed 16 bit value.  (The boundary conditions aren't quite	\
       right, but this is just a heuristic anyhow.)  */			\
    if (((OUTER_CODE) == LT || (OUTER_CODE) == LE			\
	 || (OUTER_CODE) == GE || (OUTER_CODE) == GT			\
	 || (OUTER_CODE) == LTU || (OUTER_CODE) == LEU			\
	 || (OUTER_CODE) == GEU || (OUTER_CODE) == GTU)			\
	&& INTVAL (X) >= -0x8000 && INTVAL (X) < 0x8000)		\
      return 0;								\
    /* Equality comparisons with 0 are cheap.  */			\
    if (((OUTER_CODE) == EQ || (OUTER_CODE) == NE)			\
	&& INTVAL (X) == 0)						\
      return 0;								\
									\
    /* Otherwise, work out the cost to load the value into a		\
       register.  */							\
    if (INTVAL (X) >= 0 && INTVAL (X) < 0x100)				\
      return COSTS_N_INSNS (1);						\
    else if ((INTVAL (X) >= 0 && INTVAL (X) < 0x10000)			\
	     || (INTVAL (X) < 0 && INTVAL (X) > -0x100))		\
      return COSTS_N_INSNS (2);						\
    else								\
      return COSTS_N_INSNS (3);						\
									\
  case LABEL_REF:							\
    return COSTS_N_INSNS (2);						\
									\
  case CONST:								\
    {									\
      rtx offset = const0_rtx;						\
      rtx symref = eliminate_constant_term (XEXP (X, 0), &offset);	\
									\
      if (TARGET_MIPS16 && mips16_gp_offset_p (X))			\
	{								\
	  /* Treat this like a signed 16 bit CONST_INT.  */		\
	  if ((OUTER_CODE) == PLUS)					\
	    return 0;							\
	  else if ((OUTER_CODE) == SET)					\
	    return COSTS_N_INSNS (1);					\
	  else								\
	    return COSTS_N_INSNS (2);					\
	}								\
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
    {									\
      rtx high, low;							\
      if (TARGET_MIPS16)						\
	return COSTS_N_INSNS (4);					\
      split_double (X, &high, &low);					\
      return COSTS_N_INSNS ((high == CONST0_RTX (GET_MODE (high))	\
			     || low == CONST0_RTX (GET_MODE (low)))	\
			    ? 2 : 4);					\
    }

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

/* ??? Fix this to be right for the R8000.  */
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
    return COSTS_N_INSNS ((GET_MODE (X) == DImode && !TARGET_64BIT) ? 2 : 1); \
									\
  case AND:								\
  case IOR:								\
  case XOR:								\
    if (GET_MODE (X) == DImode && !TARGET_64BIT)			\
      return COSTS_N_INSNS (2);						\
									\
    break;								\
									\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
    if (GET_MODE (X) == DImode && !TARGET_64BIT)			\
      return COSTS_N_INSNS ((GET_CODE (XEXP (X, 1)) == CONST_INT) ? 4 : 12); \
									\
    break;								\
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
	{								\
	  if (mips_cpu == PROCESSOR_R3000				\
              || mips_cpu == PROCESSOR_R3900)				\
	    return COSTS_N_INSNS (2);					\
	  else if (mips_cpu == PROCESSOR_R6000)				\
	    return COSTS_N_INSNS (3);					\
	  else								\
	    return COSTS_N_INSNS (6);					\
	}								\
									\
      if (xmode == DImode && !TARGET_64BIT)				\
	return COSTS_N_INSNS (4);					\
									\
      break;								\
    }									\
									\
  case NEG:								\
    if (GET_MODE (X) == DImode && !TARGET_64BIT)			\
      return 4;								\
									\
    break;								\
									\
  case MULT:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode)						\
	{								\
	  if (mips_cpu == PROCESSOR_R3000				\
	      || mips_cpu == PROCESSOR_R3900				\
	      || mips_cpu == PROCESSOR_R5000)				\
	    return COSTS_N_INSNS (4);					\
	  else if (mips_cpu == PROCESSOR_R6000)				\
	    return COSTS_N_INSNS (5);					\
	  else								\
	    return COSTS_N_INSNS (7);					\
	}								\
									\
      if (xmode == DFmode)						\
	{								\
	  if (mips_cpu == PROCESSOR_R3000				\
	      || mips_cpu == PROCESSOR_R3900				\
	      || mips_cpu == PROCESSOR_R5000)				\
	    return COSTS_N_INSNS (5);					\
	  else if (mips_cpu == PROCESSOR_R6000)				\
	    return COSTS_N_INSNS (6);					\
	  else								\
	    return COSTS_N_INSNS (8);					\
	}								\
									\
      if (mips_cpu == PROCESSOR_R3000)					\
	return COSTS_N_INSNS (12);					\
      else if (mips_cpu == PROCESSOR_R3900)				\
	return COSTS_N_INSNS (2);					\
      else if (mips_cpu == PROCESSOR_R6000)				\
	return COSTS_N_INSNS (17);					\
      else if (mips_cpu == PROCESSOR_R5000)				\
	return COSTS_N_INSNS (5);					\
      else								\
	return COSTS_N_INSNS (10);					\
    }									\
									\
  case DIV:								\
  case MOD:								\
    {									\
      enum machine_mode xmode = GET_MODE (X);				\
      if (xmode == SFmode)						\
	{								\
	  if (mips_cpu == PROCESSOR_R3000				\
              || mips_cpu == PROCESSOR_R3900)				\
	    return COSTS_N_INSNS (12);					\
	  else if (mips_cpu == PROCESSOR_R6000)				\
	    return COSTS_N_INSNS (15);					\
	  else								\
	    return COSTS_N_INSNS (23);					\
	}								\
									\
      if (xmode == DFmode)						\
	{								\
	  if (mips_cpu == PROCESSOR_R3000				\
              || mips_cpu == PROCESSOR_R3900)				\
	    return COSTS_N_INSNS (19);					\
	  else if (mips_cpu == PROCESSOR_R6000)				\
	    return COSTS_N_INSNS (16);					\
	  else								\
	    return COSTS_N_INSNS (36);					\
	}								\
    }									\
    /* fall through */							\
									\
  case UDIV:								\
  case UMOD:								\
    if (mips_cpu == PROCESSOR_R3000					\
        || mips_cpu == PROCESSOR_R3900)					\
      return COSTS_N_INSNS (35);					\
    else if (mips_cpu == PROCESSOR_R6000)				\
      return COSTS_N_INSNS (38);					\
    else if (mips_cpu == PROCESSOR_R5000)				\
      return COSTS_N_INSNS (36);					\
    else								\
      return COSTS_N_INSNS (69);					\
									\
  case SIGN_EXTEND:							\
    /* A sign extend from SImode to DImode in 64 bit mode is often	\
       zero instructions, because the result can often be used		\
       directly by another instruction; we'll call it one.  */		\
    if (TARGET_64BIT && GET_MODE (X) == DImode				\
	&& GET_MODE (XEXP (X, 0)) == SImode)				\
      return COSTS_N_INSNS (1);						\
    else								\
      return COSTS_N_INSNS (2);						\
									\
  case ZERO_EXTEND:							\
    if (TARGET_64BIT && GET_MODE (X) == DImode				\
	&& GET_MODE (XEXP (X, 0)) == SImode)				\
      return COSTS_N_INSNS (2);						\
    else								\
      return COSTS_N_INSNS (1);

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
   not allow such copying.

   ??? We make make the cost of moving from HI/LO/HILO/MD into general
   registers the same as for one of moving general registers to
   HI/LO/HILO/MD for TARGET_MIPS16 in order to prevent allocating a
   pseudo to HI/LO/HILO/MD.  This might hurt optimizations though, it
   isn't clear if it is wise.  And it might not work in all cases.  We
   could solve the DImode LO reg problem by using a multiply, just like
   reload_{in,out}si.  We could solve the SImode/HImode HI reg problem
   by using divide instructions.  divu puts the remainder in the HI
   reg, so doing a divide by -1 will move the value in the HI reg for
   all values except -1.  We could handle that case by using a signed
   divide, e.g.  -1 / 2 (or maybe 1 / -2?).  We'd have to emit a
   compare/branch to test the input value to see which instruction we
   need to use.  This gets pretty messy, but it is feasible. */

#define REGISTER_MOVE_COST(FROM, TO)	\
  ((FROM) == M16_REGS && GR_REG_CLASS_P (TO) ? 2			\
   : (FROM) == M16_NA_REGS && GR_REG_CLASS_P (TO) ? 2			\
   : GR_REG_CLASS_P (FROM) && (TO) == M16_REGS ? 2			\
   : GR_REG_CLASS_P (FROM) && (TO) == M16_NA_REGS ? 2			\
   : GR_REG_CLASS_P (FROM) && GR_REG_CLASS_P (TO) ? (TARGET_MIPS16 ? 4 : 2) \
   : (FROM) == FP_REGS && (TO) == FP_REGS ? 2				\
   : GR_REG_CLASS_P (FROM) && (TO) == FP_REGS ? 4			\
   : (FROM) == FP_REGS && GR_REG_CLASS_P (TO) ? 4			\
   : (((FROM) == HI_REG || (FROM) == LO_REG				\
       || (FROM) == MD_REGS || (FROM) == HILO_REG)			\
      && ((TO) == M16_REGS || (TO) == M16_NA_REGS)) ? 6			\
   : (((FROM) == HI_REG || (FROM) == LO_REG				\
       || (FROM) == MD_REGS || (FROM) == HILO_REG)			\
      && GR_REG_CLASS_P (TO)) ? (TARGET_MIPS16 ? 12 : 6)		\
   : (((TO) == HI_REG || (TO) == LO_REG					\
       || (TO) == MD_REGS || (TO) == HILO_REG)				\
      && GR_REG_CLASS_P (FROM)) ? (TARGET_MIPS16 ? 12 : 6)		\
   : (FROM) == ST_REGS && GR_REG_CLASS_P (TO) ? 4			\
   : (FROM) == FP_REGS && (TO) == ST_REGS ? 8				\
   : 12)

/* ??? Fix this to be right for the R8000.  */
#define MEMORY_MOVE_COST(MODE,CLASS,TO_P) \
  (((mips_cpu == PROCESSOR_R4000 || mips_cpu == PROCESSOR_R6000) ? 6 : 4) \
   + memory_move_secondary_cost ((MODE), (CLASS), (TO_P)))

/* Define if copies to/from condition code registers should be avoided.

   This is needed for the MIPS because reload_outcc is not complete;
   it needs to handle cases where the source is a general or another
   condition code register.  */
#define AVOID_CCMODE_COPIES

/* A C expression for the cost of a branch instruction.  A value of
   1 is the default; other values are interpreted relative to that.  */

/* ??? Fix this to be right for the R8000.  */
#define BRANCH_COST							\
  ((! TARGET_MIPS16							\
    && (mips_cpu == PROCESSOR_R4000 || mips_cpu == PROCESSOR_R6000))	\
   ? 2 : 1)

/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  On the MIPS, ignore the cost of anti- and
   output-dependencies.  */

#define ADJUST_COST(INSN,LINK,DEP_INSN,COST)				\
  if (REG_NOTE_KIND (LINK) != 0)					\
    (COST) = 0; /* Anti or output dependence.  */

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
  {"reg_or_0_operand",		{ REG, CONST_INT, CONST_DOUBLE, SUBREG }}, \
  {"true_reg_or_0_operand",	{ REG, CONST_INT, CONST_DOUBLE, SUBREG }}, \
  {"small_int",			{ CONST_INT }},				\
  {"large_int",			{ CONST_INT }},				\
  {"mips_const_double_ok",	{ CONST_DOUBLE }},			\
  {"const_float_1_operand",	{ CONST_DOUBLE }},			\
  {"simple_memory_operand",	{ MEM, SUBREG }},			\
  {"equality_op",		{ EQ, NE }},				\
  {"cmp_op",			{ EQ, NE, GT, GE, GTU, GEU, LT, LE,	\
				  LTU, LEU }},				\
  {"pc_or_label_operand",	{ PC, LABEL_REF }},			\
  {"call_insn_operand",		{ CONST_INT, CONST, SYMBOL_REF, REG}},	\
  {"move_operand", 		{ CONST_INT, CONST_DOUBLE, CONST,	\
				  SYMBOL_REF, LABEL_REF, SUBREG,	\
				  REG, MEM}},				\
  {"movdi_operand",		{ CONST_INT, CONST_DOUBLE, CONST,	\
				  SYMBOL_REF, LABEL_REF, SUBREG, REG,	\
				  MEM, SIGN_EXTEND }},			\
  {"se_register_operand",	{ SUBREG, REG, SIGN_EXTEND }},		\
  {"se_reg_or_0_operand",	{ REG, CONST_INT, CONST_DOUBLE, SUBREG,	\
				  SIGN_EXTEND }},			\
  {"se_uns_arith_operand",	{ REG, CONST_INT, SUBREG,		\
				  SIGN_EXTEND }},			\
  {"se_arith_operand",		{ REG, CONST_INT, SUBREG,		\
				  SIGN_EXTEND }},			\
  {"se_nonmemory_operand",	{ CONST_INT, CONST_DOUBLE, CONST,	\
				  SYMBOL_REF, LABEL_REF, SUBREG,	\
				  REG, SIGN_EXTEND }},			\
  {"se_nonimmediate_operand",   { SUBREG, REG, MEM, SIGN_EXTEND }},	\
  {"consttable_operand",	{ LABEL_REF, SYMBOL_REF, CONST_INT,	\
				  CONST_DOUBLE, CONST }},		\
  {"extend_operator",           { SIGN_EXTEND, ZERO_EXTEND }},          \
  {"highpart_shift_operator",   { ASHIFTRT, LSHIFTRT, ROTATERT, ROTATE }},



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
  &mips_reg_names[67][0],						\
  &mips_reg_names[68][0],						\
  &mips_reg_names[69][0],						\
  &mips_reg_names[70][0],						\
  &mips_reg_names[71][0],						\
  &mips_reg_names[72][0],						\
  &mips_reg_names[73][0],						\
  &mips_reg_names[74][0],						\
  &mips_reg_names[75][0],						\
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
  "hi",   "lo",   "accum","$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",	\
  "$fcc5","$fcc6","$fcc7","$rap"					\
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
  { "$fp",	30 + GP_REG_FIRST }					\
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

/* This is defined so that it can be overridden in iris6.h.  */
#define ASM_OUTPUT_FILENAME(STREAM, NUM_SOURCE_FILENAMES, NAME) \
do								\
  {								\
    fprintf (STREAM, "\t.file\t%d ", NUM_SOURCE_FILENAMES);	\
    output_quoted_string (STREAM, NAME);			\
    fputs ("\n", STREAM);					\
  }								\
while (0)

/* This is how to output a note the debugger telling it the line number
   to which the following sequence of instructions corresponds.
   Silicon graphics puts a label after each .loc.  */

#ifndef LABEL_AFTER_LOC
#define LABEL_AFTER_LOC(STREAM)
#endif

#define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE)				\
  mips_output_lineno (STREAM, LINE)

/* The MIPS implementation uses some labels for its own purpose.  The
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
  mips_declare_object (STREAM, NAME, "\n\t.comm\t", ",%u\n", (SIZE))

/* This says how to define a local common symbol (ie, not visible to
   linker).  */

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)			\
  mips_declare_object (STREAM, NAME, "\n\t.lcomm\t", ",%u\n", (SIZE))


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
  if (TARGET_GP_OPT && ! TARGET_MIPS16)					\
    {									\
      STREAM = asm_out_text_file;					\
      /* ??? text_section gets called too soon.  If the previous	\
	 function is in a special section and we're not, we have	\
	 to switch back to the text section.  We can't call		\
	 text_section again as gcc thinks we're already there.  */	\
      /* ??? See varasm.c.  There are other things that get output	\
	 too early, like alignment (before we've switched STREAM).  */	\
      if (DECL_SECTION_NAME (DECL) == NULL_TREE)			\
	fprintf (STREAM, "%s\n", TEXT_SECTION_ASM_OP);			\
    }									\
									\
  HALF_PIC_DECLARE (NAME);						\
}

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM,PREFIX,NUM)			\
  fprintf (STREAM, "%s%s%d:\n", LOCAL_LABEL_PREFIX, PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
  sprintf ((LABEL), "*%s%s%ld", (LOCAL_LABEL_PREFIX), (PREFIX), (long)(NUM))

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

/* Likewise for 64 bit, `char' and `short' constants.  */

#define ASM_OUTPUT_DOUBLE_INT(STREAM,VALUE)				\
do {									\
  if (TARGET_64BIT)							\
    {									\
      fprintf (STREAM, "\t.dword\t");					\
      if (HOST_BITS_PER_WIDE_INT < 64 || GET_CODE (VALUE) != CONST_INT)	\
	/* We can't use 'X' for negative numbers, because then we won't	\
	   get the right value for the upper 32 bits.  */		\
        output_addr_const (STREAM, VALUE);				\
      else								\
	/* We must use 'X', because otherwise LONG_MIN will print as	\
	   a number that the Irix 6 assembler won't accept.  */		\
        print_operand (STREAM, VALUE, 'X');				\
      fprintf (STREAM, "\n");						\
    }									\
  else									\
    {									\
      assemble_integer (operand_subword ((VALUE), 0, 0, DImode),	\
			UNITS_PER_WORD, 1);				\
      assemble_integer (operand_subword ((VALUE), 1, 0, DImode),	\
			UNITS_PER_WORD, 1);				\
    }									\
} while (0)

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
  fprintf (STREAM, "\t%s\t%sL%d\n",					\
	   Pmode == DImode ? ".dword" : ".word",			\
	   LOCAL_LABEL_PREFIX,						\
	   VALUE)

/* This is how to output an element of a case-vector that is relative.
   This is used for pc-relative code (e.g. when TARGET_ABICALLS or
   TARGET_EMBEDDED_PIC).  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
do {									\
  if (TARGET_MIPS16)							\
    fprintf (STREAM, "\t.half\t%sL%d-%sL%d\n",				\
	     LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL);	\
  else if (TARGET_EMBEDDED_PIC)						\
    fprintf (STREAM, "\t%s\t%sL%d-%sLS%d\n",				\
	     Pmode == DImode ? ".dword" : ".word",			\
	     LOCAL_LABEL_PREFIX, VALUE, LOCAL_LABEL_PREFIX, REL);	\
  else if (mips_abi == ABI_32 || mips_abi == ABI_O64)			\
    fprintf (STREAM, "\t%s\t%sL%d\n",					\
	     Pmode == DImode ? ".gpdword" : ".gpword",			\
	     LOCAL_LABEL_PREFIX, VALUE);				\
  else									\
    fprintf (STREAM, "\t%s\t%sL%d\n",					\
	     Pmode == DImode ? ".dword" : ".word",			\
	     LOCAL_LABEL_PREFIX, VALUE);				\
} while (0)

/* When generating embedded PIC or mips16 code we want to put the jump
   table in the .text section.  In all other cases, we want to put the
   jump table in the .rdata section.  Unfortunately, we can't use
   JUMP_TABLES_IN_TEXT_SECTION, because it is not conditional.
   Instead, we use ASM_OUTPUT_CASE_LABEL to switch back to the .text
   section if appropriate.  */
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, INSN)			\
do {									\
  if (TARGET_EMBEDDED_PIC || TARGET_MIPS16)				\
    function_section (current_function_decl);				\
  ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM);			\
} while (0)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(STREAM,LOG)					\
  fprintf (STREAM, "\t.align\t%d\n", (LOG))

/* This is how to output an assembler line to advance the location
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

/* Default to -G 8 */
#ifndef MIPS_DEFAULT_GVALUE
#define MIPS_DEFAULT_GVALUE 8
#endif

/* Define the strings to put out for each section in the object file.  */
#define TEXT_SECTION_ASM_OP	"\t.text"	/* instructions */
#define DATA_SECTION_ASM_OP	"\t.data"	/* large data */
#define SDATA_SECTION_ASM_OP	"\t.sdata"	/* small data */
#define RDATA_SECTION_ASM_OP	"\t.rdata"	/* read-only data */
#define READONLY_DATA_SECTION	rdata_section
#define SMALL_DATA_SECTION	sdata_section

/* What other sections we support other than the normal .data/.text.  */

#define EXTRA_SECTIONS in_sdata, in_rdata

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

#define SELECT_RTX_SECTION(MODE,RTX)	mips_select_rtx_section (MODE, RTX)

#define SELECT_SECTION(DECL, RELOC)	mips_select_section (DECL, RELOC)


/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)			\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),			\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)				\
do									\
  {									\
    fprintf (STREAM, "\t%s\t%s,%s,8\n\t%s\t%s,0(%s)\n",			\
	     TARGET_64BIT ? "dsubu" : "subu",				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     reg_names[STACK_POINTER_REGNUM],				\
	     TARGET_64BIT ? "sd" : "sw",				\
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
    fprintf (STREAM, "\t%s\t%s,0(%s)\n\t%s\t%s,%s,8\n",			\
	     TARGET_64BIT ? "ld" : "lw",				\
	     reg_names[REGNO],						\
	     reg_names[STACK_POINTER_REGNUM],				\
	     TARGET_64BIT ? "daddu" : "addu",				\
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

/* How to start an assembler comment.
   The leading space is important (the mips native assembler requires it).  */
#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START " #"
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
#define NO_BUILTIN_SIZE_TYPE
#define SIZE_TYPE (Pmode == DImode ? "long unsigned int" : "unsigned int")
#endif

#ifndef PTRDIFF_TYPE
#define NO_BUILTIN_PTRDIFF_TYPE
#define PTRDIFF_TYPE (Pmode == DImode ? "long int" : "int")
#endif

/* See mips_expand_prologue's use of loadgp for when this should be
   true.  */

#define DONT_ACCESS_GBLS_AFTER_EPILOGUE (TARGET_ABICALLS 		\
					 && mips_abi != ABI_32		\
					 && mips_abi != ABI_O64)

/* In mips16 mode, we need to look through the function to check for
   PC relative loads that are out of range.  */
#define MACHINE_DEPENDENT_REORG(X) machine_dependent_reorg (X)

/* We need to use a special set of functions to handle hard floating
   point code in mips16 mode.  */

#ifndef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS
#endif

#define INIT_TARGET_OPTABS						\
do									\
  {									\
    if (! TARGET_MIPS16 || ! mips16_hard_float)				\
      INIT_SUBTARGET_OPTABS;						\
    else								\
      {									\
	add_optab->handlers[(int) SFmode].libfunc =			\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_addsf3");		\
	sub_optab->handlers[(int) SFmode].libfunc =			\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_subsf3");		\
	smul_optab->handlers[(int) SFmode].libfunc =			\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_mulsf3");		\
	flodiv_optab->handlers[(int) SFmode].libfunc =			\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_divsf3");		\
									\
	eqsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_eqsf2");	\
	nesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_nesf2");	\
	gtsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_gtsf2");	\
	gesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_gesf2");	\
	ltsf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_ltsf2");	\
	lesf2_libfunc = gen_rtx (SYMBOL_REF, Pmode, "__mips16_lesf2");	\
									\
	floatsisf_libfunc =						\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_floatsisf");		\
	fixsfsi_libfunc =						\
	  gen_rtx (SYMBOL_REF, Pmode, "__mips16_fixsfsi");		\
									\
	if (TARGET_DOUBLE_FLOAT)					\
	  {								\
	    add_optab->handlers[(int) DFmode].libfunc =			\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_adddf3");		\
	    sub_optab->handlers[(int) DFmode].libfunc =			\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_subdf3");		\
	    smul_optab->handlers[(int) DFmode].libfunc =		\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_muldf3");		\
	    flodiv_optab->handlers[(int) DFmode].libfunc =		\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_divdf3");		\
									\
	    extendsfdf2_libfunc =					\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_extendsfdf2");	\
	    truncdfsf2_libfunc =					\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_truncdfsf2");	\
									\
	    eqdf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_eqdf2");		\
	    nedf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_nedf2");		\
	    gtdf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_gtdf2");		\
	    gedf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_gedf2");		\
	    ltdf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_ltdf2");		\
	    ledf2_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_ledf2");		\
									\
	    floatsidf_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_floatsidf");	\
	    fixdfsi_libfunc =						\
	      gen_rtx (SYMBOL_REF, Pmode, "__mips16_fixdfsi");		\
	  }								\
      }									\
  }									\
while (0)
