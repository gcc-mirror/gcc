/* Subroutines for insn-output.c for MIPS
   Copyright (C) 1989, 1990, 1991, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2005 Free Software Foundation, Inc.
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by Michael Meissner, meissner@osf.org.
   64 bit r4000 support by Ian Lance Taylor, ian@cygnus.com, and
   Brendan Eich, brendan@microunity.com.

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

/* ??? The TARGET_FP_CALL_32 macros are intended to simulate a 32 bit
   calling convention in 64 bit mode.  It doesn't work though, and should
   be replaced with something better designed.  */

#include "config.h"
#include "system.h"
#include <signal.h>
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
#include "output.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "flags.h"
#include "reload.h"
#include "output.h"
#include "tm_p.h"
#include "ggc.h"
#include "gstab.h"
#include "hashtab.h"
#include "debug.h"
#include "target.h"
#include "target-def.h"

#ifdef __GNU_STAB__
#define STAB_CODE_TYPE enum __stab_debug_code
#else
#define STAB_CODE_TYPE int
#endif

extern tree   lookup_name PARAMS ((tree));

/* Enumeration for all of the relational tests, so that we can build
   arrays indexed by the test type, and not worry about the order
   of EQ, NE, etc.  */

enum internal_test {
    ITEST_EQ,
    ITEST_NE,
    ITEST_GT,
    ITEST_GE,
    ITEST_LT,
    ITEST_LE,
    ITEST_GTU,
    ITEST_GEU,
    ITEST_LTU,
    ITEST_LEU,
    ITEST_MAX
  };


struct constant;
struct mips_arg_info;
static enum internal_test map_test_to_internal_test	PARAMS ((enum rtx_code));
static void get_float_compare_codes PARAMS ((enum rtx_code, enum rtx_code *,
					     enum rtx_code *));
static int mips16_simple_memory_operand		PARAMS ((rtx, rtx,
							enum machine_mode));
static int m16_check_op				PARAMS ((rtx, int, int, int));
static void block_move_loop			PARAMS ((rtx, rtx,
							 unsigned int,
							 int,
							 rtx, rtx));
static void block_move_call			PARAMS ((rtx, rtx, rtx));
static void mips_arg_info		PARAMS ((const CUMULATIVE_ARGS *,
						 enum machine_mode,
						 tree, int,
						 struct mips_arg_info *));
static rtx mips_add_large_offset_to_sp		PARAMS ((HOST_WIDE_INT));
static void mips_annotate_frame_insn		PARAMS ((rtx, rtx));
static rtx mips_frame_set			PARAMS ((enum machine_mode,
							 int, int));
static void mips_emit_frame_related_store	PARAMS ((rtx, rtx,
							 HOST_WIDE_INT));
static void save_restore_insns			PARAMS ((int, rtx, long));
static void mips16_output_gp_offset		PARAMS ((FILE *, rtx));
static void mips16_fp_args			PARAMS ((FILE *, int, int));
static void build_mips16_function_stub		PARAMS ((FILE *));
static void mips16_optimize_gp			PARAMS ((rtx));
static rtx add_constant				PARAMS ((struct constant **,
							rtx,
							enum machine_mode));
static void dump_constants			PARAMS ((struct constant *,
							rtx));
static rtx mips_find_symbol			PARAMS ((rtx));
static void abort_with_insn			PARAMS ((rtx, const char *))
  ATTRIBUTE_NORETURN;
static int symbolic_expression_p                PARAMS ((rtx));
static bool mips_assemble_integer	  PARAMS ((rtx, unsigned int, int));
static void mips_output_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));
static void mips_output_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
static void mips_set_architecture    PARAMS ((const struct mips_cpu_info *));
static void mips_set_tune	     PARAMS ((const struct mips_cpu_info *));
static bool mips_strict_matching_cpu_name_p	PARAMS ((const char *,
							 const char *));
static bool mips_matching_cpu_name_p		PARAMS ((const char *,
							 const char *));
static const struct mips_cpu_info *mips_parse_cpu   PARAMS ((const char *,
							      const char *));
static const struct mips_cpu_info *mips_cpu_info_from_isa PARAMS ((int));
static void copy_file_data			PARAMS ((FILE *, FILE *));
#ifdef TARGET_IRIX6
static void iris6_asm_named_section_1		PARAMS ((const char *,
							 unsigned int,
							 unsigned int));
static void iris6_asm_named_section		PARAMS ((const char *,
							 unsigned int));
static int iris_section_align_entry_eq		PARAMS ((const PTR, const PTR));
static hashval_t iris_section_align_entry_hash	PARAMS ((const PTR));
static int iris6_section_align_1		PARAMS ((void **, void *));
#endif
static int mips_adjust_cost			PARAMS ((rtx, rtx, rtx, int));
static int mips_issue_rate			PARAMS ((void));

static struct machine_function * mips_init_machine_status PARAMS ((void));
static void mips_select_section PARAMS ((tree, int, unsigned HOST_WIDE_INT))
	ATTRIBUTE_UNUSED;
static void mips_unique_section			PARAMS ((tree, int))
	ATTRIBUTE_UNUSED;
static void mips_select_rtx_section PARAMS ((enum machine_mode, rtx,
					     unsigned HOST_WIDE_INT));
static int mips_use_dfa_pipeline_interface      PARAMS ((void));
static void mips_encode_section_info		PARAMS ((tree, int));

/* Structure to be filled in by compute_frame_size with register
   save masks, and offsets for the current function.  */

struct mips_frame_info GTY(())
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

struct machine_function GTY(()) {
  /* Pseudo-reg holding the address of the current function when
     generating embedded PIC code.  Created by LEGITIMIZE_ADDRESS,
     used by mips_finalize_pic if it was created.  */
  rtx embedded_pic_fnaddr_rtx;

  /* Pseudo-reg holding the value of $28 in a mips16 function which
     refers to GP relative global variables.  */
  rtx mips16_gp_pseudo_rtx;

  /* Current frame information, calculated by compute_frame_size.  */
  struct mips_frame_info frame;

  /* Length of instructions in function; mips16 only.  */
  long insns_len;
};

/* Information about a single argument.  */
struct mips_arg_info
{
  /* True if the argument is a record or union type.  */
  bool struct_p;

  /* True if the argument is passed in a floating-point register, or
     would have been if we hadn't run out of registers.  */
  bool fpr_p;

  /* The argument's size, in bytes.  */
  unsigned int num_bytes;

  /* The number of words passed in registers, rounded up.  */
  unsigned int reg_words;

  /* The offset of the first register from GP_ARG_FIRST or FP_ARG_FIRST,
     or MAX_ARGS_IN_REGISTERS if the argument is passed entirely
     on the stack.  */
  unsigned int reg_offset;

  /* The number of words that must be passed on the stack, rounded up.  */
  unsigned int stack_words;

  /* The offset from the start of the stack overflow area of the argument's
     first stack word.  Only meaningful when STACK_WORDS is nonzero.  */
  unsigned int stack_offset;
};

/* Global variables for machine-dependent things.  */

/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
int mips_section_threshold = -1;

/* Count the number of .file directives, so that .loc is up to date.  */
int num_source_filenames = 0;

/* Count the number of sdb related labels are generated (to find block
   start and end boundaries).  */
int sdb_label_count = 0;

/* Next label # for each statement for Silicon Graphics IRIS systems.  */
int sym_lineno = 0;

/* Nonzero if inside of a function, because the stupid MIPS asm can't
   handle .files inside of functions.  */
int inside_function = 0;

/* Files to separate the text and the data output, so that all of the data
   can be emitted before the text, which will mean that the assembler will
   generate smaller code, based on the global pointer.  */
FILE *asm_out_data_file;
FILE *asm_out_text_file;

/* Linked list of all externals that are to be emitted when optimizing
   for the global pointer if they haven't been declared by the end of
   the program with an appropriate .comm or initialization.  */

struct extern_list
{
  struct extern_list *next;	/* next external */
  const char *name;		/* name of the external */
  int size;			/* size in bytes */
} *extern_head = 0;

/* Name of the file containing the current function.  */
const char *current_function_file = "";

/* Warning given that Mips ECOFF can't support changing files
   within a function.  */
int file_in_function_warning = FALSE;

/* Whether to suppress issuing .loc's because the user attempted
   to change the filename within a function.  */
int ignore_line_number = FALSE;

/* Number of nested .set noreorder, noat, nomacro, and volatile requests.  */
int set_noreorder;
int set_noat;
int set_nomacro;
int set_volatile;

/* The next branch instruction is a branch likely, not branch normal.  */
int mips_branch_likely;

/* Count of delay slots and how many are filled.  */
int dslots_load_total;
int dslots_load_filled;
int dslots_jump_total;
int dslots_jump_filled;

/* # of nops needed by previous insn */
int dslots_number_nops;

/* Number of 1/2/3 word references to data items (ie, not jal's).  */
int num_refs[3];

/* registers to check for load delay */
rtx mips_load_reg, mips_load_reg2, mips_load_reg3, mips_load_reg4;

/* Cached operands, and operator to compare for use in set/branch/trap
   on condition codes.  */
rtx branch_cmp[2];

/* what type of branch to use */
enum cmp_type branch_type;

/* The target cpu for code generation.  */
enum processor_type mips_arch;
const struct mips_cpu_info *mips_arch_info;

/* The target cpu for optimization and scheduling.  */
enum processor_type mips_tune;
const struct mips_cpu_info *mips_tune_info;

/* which instruction set architecture to use.  */
int mips_isa;

/* which abi to use.  */
int mips_abi;

/* Strings to hold which cpu and instruction set architecture to use.  */
const char *mips_arch_string;   /* for -march=<xxx> */
const char *mips_tune_string;   /* for -mtune=<xxx> */
const char *mips_isa_string;	/* for -mips{1,2,3,4} */
const char *mips_abi_string;	/* for -mabi={32,n32,64,eabi} */

/* Whether we are generating mips16 code.  This is a synonym for
   TARGET_MIPS16, and exists for use as an attribute.  */
int mips16;

/* This variable is set by -mno-mips16.  We only care whether
   -mno-mips16 appears or not, and using a string in this fashion is
   just a way to avoid using up another bit in target_flags.  */
const char *mips_no_mips16_string;

/* Whether we are generating mips16 hard float code.  In mips16 mode
   we always set TARGET_SOFT_FLOAT; this variable is nonzero if
   -msoft-float was not specified by the user, which means that we
   should arrange to call mips32 hard floating point code.  */
int mips16_hard_float;

/* This variable is set by -mentry.  We only care whether -mentry
   appears or not, and using a string in this fashion is just a way to
   avoid using up another bit in target_flags.  */
const char *mips_entry_string;

const char *mips_cache_flush_func = CACHE_FLUSH_FUNC;

/* Whether we should entry and exit pseudo-ops in mips16 mode.  */
int mips_entry;

/* If TRUE, we split addresses into their high and low parts in the RTL.  */
int mips_split_addresses;

/* Generating calls to position independent functions?  */
enum mips_abicalls_type mips_abicalls;

/* Mode used for saving/restoring general purpose registers.  */
static enum machine_mode gpr_mode;

/* Array giving truth value on whether or not a given hard register
   can support a given mode.  */
char mips_hard_regno_mode_ok[(int)MAX_MACHINE_MODE][FIRST_PSEUDO_REGISTER];

/* The length of all strings seen when compiling for the mips16.  This
   is used to tell how many strings are in the constant pool, so that
   we can see if we may have an overflow.  This is reset each time the
   constant pool is output.  */
int mips_string_length;

/* When generating mips16 code, a list of all strings that are to be
   output after the current function.  */

static GTY(()) rtx mips16_strings;

/* In mips16 mode, we build a list of all the string constants we see
   in a particular function.  */

struct string_constant
{
  struct string_constant *next;
  const char *label;
};

static struct string_constant *string_constants;

/* List of all MIPS punctuation characters used by print_operand.  */
char mips_print_operand_punct[256];

/* Map GCC register number to debugger register number.  */
int mips_dbx_regno[FIRST_PSEUDO_REGISTER];

/* Buffer to use to enclose a load/store operation with %{ %} to
   turn on .set volatile.  */
static char volatile_buffer[60];

/* Hardware names for the registers.  If -mrnames is used, this
   will be overwritten with mips_sw_reg_names.  */

char mips_reg_names[][8] =
{
 "$0",   "$1",   "$2",   "$3",   "$4",   "$5",   "$6",   "$7",
 "$8",   "$9",   "$10",  "$11",  "$12",  "$13",  "$14",  "$15",
 "$16",  "$17",  "$18",  "$19",  "$20",  "$21",  "$22",  "$23",
 "$24",  "$25",  "$26",  "$27",  "$28",  "$sp",  "$fp",  "$31",
 "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
 "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
 "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
 "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",
 "hi",   "lo",   "accum","$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",
 "$fcc5","$fcc6","$fcc7","$rap", "",     "",     "",     "",
 "$c0r0", "$c0r1", "$c0r2", "$c0r3", "$c0r4", "$c0r5", "$c0r6", "$c0r7",
 "$c0r8", "$c0r9", "$c0r10","$c0r11","$c0r12","$c0r13","$c0r14","$c0r15",
 "$c0r16","$c0r17","$c0r18","$c0r19","$c0r20","$c0r21","$c0r22","$c0r23",
 "$c0r24","$c0r25","$c0r26","$c0r27","$c0r28","$c0r29","$c0r30","$c0r31",
 "$c2r0", "$c2r1", "$c2r2", "$c2r3", "$c2r4", "$c2r5", "$c2r6", "$c2r7",
 "$c2r8", "$c2r9", "$c2r10","$c2r11","$c2r12","$c2r13","$c2r14","$c2r15",
 "$c2r16","$c2r17","$c2r18","$c2r19","$c2r20","$c2r21","$c2r22","$c2r23",
 "$c2r24","$c2r25","$c2r26","$c2r27","$c2r28","$c2r29","$c2r30","$c2r31",
 "$c3r0", "$c3r1", "$c3r2", "$c3r3", "$c3r4", "$c3r5", "$c3r6", "$c3r7",
 "$c3r8", "$c3r9", "$c3r10","$c3r11","$c3r12","$c3r13","$c3r14","$c3r15",
 "$c3r16","$c3r17","$c3r18","$c3r19","$c3r20","$c3r21","$c3r22","$c3r23",
 "$c3r24","$c3r25","$c3r26","$c3r27","$c3r28","$c3r29","$c3r30","$c3r31"
};

/* Mips software names for the registers, used to overwrite the
   mips_reg_names array.  */

char mips_sw_reg_names[][8] =
{
  "$zero","$at",  "$v0",  "$v1",  "$a0",  "$a1",  "$a2",  "$a3",
  "$t0",  "$t1",  "$t2",  "$t3",  "$t4",  "$t5",  "$t6",  "$t7",
  "$s0",  "$s1",  "$s2",  "$s3",  "$s4",  "$s5",  "$s6",  "$s7",
  "$t8",  "$t9",  "$k0",  "$k1",  "$gp",  "$sp",  "$fp",  "$ra",
  "$f0",  "$f1",  "$f2",  "$f3",  "$f4",  "$f5",  "$f6",  "$f7",
  "$f8",  "$f9",  "$f10", "$f11", "$f12", "$f13", "$f14", "$f15",
  "$f16", "$f17", "$f18", "$f19", "$f20", "$f21", "$f22", "$f23",
  "$f24", "$f25", "$f26", "$f27", "$f28", "$f29", "$f30", "$f31",
  "hi",   "lo",   "accum","$fcc0","$fcc1","$fcc2","$fcc3","$fcc4",
  "$fcc5","$fcc6","$fcc7","$rap", "",     "",     "",     "",
  "$c0r0", "$c0r1", "$c0r2", "$c0r3", "$c0r4", "$c0r5", "$c0r6", "$c0r7",
  "$c0r8", "$c0r9", "$c0r10","$c0r11","$c0r12","$c0r13","$c0r14","$c0r15",
  "$c0r16","$c0r17","$c0r18","$c0r19","$c0r20","$c0r21","$c0r22","$c0r23",
  "$c0r24","$c0r25","$c0r26","$c0r27","$c0r28","$c0r29","$c0r30","$c0r31",
  "$c2r0", "$c2r1", "$c2r2", "$c2r3", "$c2r4", "$c2r5", "$c2r6", "$c2r7",
  "$c2r8", "$c2r9", "$c2r10","$c2r11","$c2r12","$c2r13","$c2r14","$c2r15",
  "$c2r16","$c2r17","$c2r18","$c2r19","$c2r20","$c2r21","$c2r22","$c2r23",
  "$c2r24","$c2r25","$c2r26","$c2r27","$c2r28","$c2r29","$c2r30","$c2r31",
  "$c3r0", "$c3r1", "$c3r2", "$c3r3", "$c3r4", "$c3r5", "$c3r6", "$c3r7",
  "$c3r8", "$c3r9", "$c3r10","$c3r11","$c3r12","$c3r13","$c3r14","$c3r15",
  "$c3r16","$c3r17","$c3r18","$c3r19","$c3r20","$c3r21","$c3r22","$c3r23",
  "$c3r24","$c3r25","$c3r26","$c3r27","$c3r28","$c3r29","$c3r30","$c3r31"
};

/* Map hard register number to register class */
const enum reg_class mips_regno_to_class[] =
{
  GR_REGS,	GR_REGS,	M16_NA_REGS,	M16_NA_REGS,
  M16_REGS,	M16_REGS,	M16_REGS,	M16_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  M16_NA_REGS,	M16_NA_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  T_REG,	GR_REGS,	GR_REGS,	GR_REGS,
  GR_REGS,	GR_REGS,	GR_REGS,	GR_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  FP_REGS,	FP_REGS,	FP_REGS,	FP_REGS,
  HI_REG,	LO_REG,		HILO_REG,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	ST_REGS,
  ST_REGS,	ST_REGS,	ST_REGS,	GR_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP0_REGS,	COP0_REGS,	COP0_REGS,	COP0_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP2_REGS,	COP2_REGS,	COP2_REGS,	COP2_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS,
  COP3_REGS,	COP3_REGS,	COP3_REGS,	COP3_REGS
};

/* Map register constraint character to register class.  */
enum reg_class mips_char_to_class[256] =
{
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
  NO_REGS,	NO_REGS,	NO_REGS,	NO_REGS,
};

/* A table describing all the processors gcc knows about.  Names are
   matched in the order listed.  The first mention of an ISA level is
   taken as the canonical name for that ISA.

   To ease comparison, please keep this table in the same order as
   gas's mips_cpu_info_table[].  */
const struct mips_cpu_info mips_cpu_info_table[] = {
  /* Entries for generic ISAs */
  { "mips1", PROCESSOR_R3000, 1 },
  { "mips2", PROCESSOR_R6000, 2 },
  { "mips3", PROCESSOR_R4000, 3 },
  { "mips4", PROCESSOR_R8000, 4 },
  { "mips32", PROCESSOR_R4KC, 32 },
  { "mips64", PROCESSOR_R5KC, 64 },

  /* MIPS I */
  { "r3000", PROCESSOR_R3000, 1 },
  { "r2000", PROCESSOR_R3000, 1 }, /* = r3000 */
  { "r3900", PROCESSOR_R3900, 1 },

  /* MIPS II */
  { "r6000", PROCESSOR_R6000, 2 },

  /* MIPS III */
  { "r4000", PROCESSOR_R4000, 3 },
  { "vr4100", PROCESSOR_R4100, 3 },
  { "vr4111", PROCESSOR_R4111, 3 },
  { "vr4120", PROCESSOR_R4120, 3 },
  { "vr4300", PROCESSOR_R4300, 3 },
  { "r4400", PROCESSOR_R4000, 3 }, /* = r4000 */
  { "r4600", PROCESSOR_R4600, 3 },
  { "orion", PROCESSOR_R4600, 3 }, /* = r4600 */
  { "r4650", PROCESSOR_R4650, 3 },

  /* MIPS IV */
  { "r8000", PROCESSOR_R8000, 4 },
  { "vr5000", PROCESSOR_R5000, 4 },
  { "vr5400", PROCESSOR_R5400, 4 },
  { "vr5500", PROCESSOR_R5500, 4 },


  /* MIPS 32 */
  { "4kc", PROCESSOR_R4KC, 32 },
  { "4kp", PROCESSOR_R4KC, 32 }, /* = 4kc */

  /* MIPS 64 */
  { "5kc", PROCESSOR_R5KC, 64 },
  { "20kc", PROCESSOR_R20KC, 64 },
  { "sr71000", PROCESSOR_SR71000, 64 },

  /* Broadcom SB-1 CPU core */
  { "sb1", PROCESSOR_SB1, 64 },

  /* End marker */
  { 0, 0, 0 }
};

/* Nonzero if -march should decide the default value of MASK_SOFT_FLOAT.  */
#ifndef MIPS_MARCH_CONTROLS_SOFT_FLOAT
#define MIPS_MARCH_CONTROLS_SOFT_FLOAT 0
#endif

/* Initialize the GCC target structure.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"
#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER mips_assemble_integer

#if TARGET_IRIX5 && !TARGET_IRIX6
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.align 0\n\t.half\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.align 0\n\t.word\t"
/* The IRIX 6 O32 assembler gives an error for `align 0; .dword', contrary
   to the documentation, so disable it.  */
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP NULL
#endif

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE mips_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE mips_output_function_epilogue
#undef TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION mips_select_rtx_section

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST mips_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE mips_issue_rate
#undef TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE
#define TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE mips_use_dfa_pipeline_interface

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO mips_encode_section_info

struct gcc_target targetm = TARGET_INITIALIZER;

/* Return truth value of whether OP can be used as an operands
   where a register or 16 bit unsigned integer is needed.  */

int
uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT_UNSIGNED (op))
    return 1;

  return register_operand (op, mode);
}

/* Return truth value of whether OP can be used as an operands
   where a 16 bit integer is needed  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT (op))
    return 1;

  /* On the mips16, a GP relative value is a signed 16 bit offset.  */
  if (TARGET_MIPS16 && GET_CODE (op) == CONST && mips16_gp_offset_p (op))
    return 1;

  return register_operand (op, mode);
}

/* Return truth value of whether OP can be used as an operand in a two
   address arithmetic insn (such as set 123456,%o4) of mode MODE.  */

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return 1;

  return register_operand (op, mode);
}

/* Return truth value of whether OP is an integer which fits in 16 bits.  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return truth value of whether OP is a 32 bit integer which is too big to
   be loaded with one instruction.  */

int
large_int (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  HOST_WIDE_INT value;

  if (GET_CODE (op) != CONST_INT)
    return 0;

  value = INTVAL (op);

  /* ior reg,$r0,value */
  if ((value & ~ ((HOST_WIDE_INT) 0x0000ffff)) == 0)
    return 0;

  /* subu reg,$r0,value */
  if (((unsigned HOST_WIDE_INT) (value + 32768)) <= 32767)
    return 0;

  /* lui reg,value>>16 */
  if ((value & 0x0000ffff) == 0)
    return 0;

  return 1;
}

/* Return truth value of whether OP is a register or the constant 0.
   In mips16 mode, we only accept a register, since the mips16 does
   not have $0.  */

int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
      if (TARGET_MIPS16)
	return 0;
      return INTVAL (op) == 0;

    case CONST_DOUBLE:
      if (TARGET_MIPS16)
	return 0;
      return op == CONST0_RTX (mode);

    case REG:
    case SUBREG:
      return register_operand (op, mode);

    default:
      break;
    }

  return 0;
}

/* Return truth value of whether OP is a register or the constant 0,
   even in mips16 mode.  */

int
true_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return INTVAL (op) == 0;

    case CONST_DOUBLE:
      return op == CONST0_RTX (mode);

    case REG:
    case SUBREG:
      return register_operand (op, mode);

    default:
      break;
    }

  return 0;
}

/* Return truth value if a CONST_DOUBLE is ok to be a legitimate constant.  */

int
mips_const_double_ok (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != CONST_DOUBLE)
    return 0;

  if (mode == VOIDmode)
    return 1;

  /* We've no zero register in mips16 mode.  */
  if (TARGET_MIPS16)
    return 0;

  if (mode != SFmode && mode != DFmode)
    return 0;

  if (op == CONST0_RTX (mode))
    return 1;

  return 0;
}

/* Accept the floating point constant 1 in the appropriate mode.  */

int
const_float_1_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  REAL_VALUE_TYPE d;

  if (GET_CODE (op) != CONST_DOUBLE
      || mode != GET_MODE (op)
      || (mode != DFmode && mode != SFmode))
    return 0;

  REAL_VALUE_FROM_CONST_DOUBLE (d, op);

  return REAL_VALUES_EQUAL (d, dconst1);
}

/* Return true if a memory load or store of REG plus OFFSET in MODE
   can be represented in a single word on the mips16.  */

static int
mips16_simple_memory_operand (reg, offset, mode)
     rtx reg;
     rtx offset;
     enum machine_mode mode;
{
  unsigned int size;
  int off;

  if (mode == BLKmode)
    {
      /* We can't tell, because we don't know how the value will
         eventually be accessed.  Returning 0 here does no great
         harm; it just prevents some possible instruction scheduling.  */
      return 0;
    }

  size = GET_MODE_SIZE (mode);

  if (INTVAL (offset) % size != 0)
    return 0;
  if (REGNO (reg) == STACK_POINTER_REGNUM && GET_MODE_SIZE (mode) == 4)
    off = 0x100;
  else
    off = 0x20;
  if (INTVAL (offset) >= 0 && INTVAL (offset) < (HOST_WIDE_INT)(off * size))
    return 1;
  return 0;
}

/* Return truth value if a memory operand fits in a single instruction
   (ie, register + small offset).  */

int
simple_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr, plus0, plus1;

  /* Eliminate non-memory operations */
  if (GET_CODE (op) != MEM)
    return 0;

  /* dword operations really put out 2 instructions, so eliminate them.  */
  /* ??? This isn't strictly correct.  It is OK to accept multiword modes
     here, since the length attributes are being set correctly, but only
     if the address is offsettable.  LO_SUM is not offsettable.  */
  if (GET_MODE_SIZE (GET_MODE (op)) > (unsigned) UNITS_PER_WORD)
    return 0;

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
    case LO_SUM:
      return 1;

    case CONST_INT:
      if (TARGET_MIPS16)
	return 0;
      return SMALL_INT (addr);

    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);
      if (GET_CODE (plus0) == REG
	  && GET_CODE (plus1) == CONST_INT && SMALL_INT (plus1)
	  && (! TARGET_MIPS16
	      || mips16_simple_memory_operand (plus0, plus1, mode)))
	return 1;

      else if (GET_CODE (plus1) == REG
	       && GET_CODE (plus0) == CONST_INT && SMALL_INT (plus0)
	       && (! TARGET_MIPS16
		   || mips16_simple_memory_operand (plus1, plus0, mode)))
	return 1;

      else
	return 0;

#if 0
      /* We used to allow small symbol refs here (ie, stuff in .sdata
	 or .sbss), but this causes some bugs in G++.  Also, it won't
	 interfere if the MIPS linker rewrites the store instruction
	 because the function is PIC.  */

    case LABEL_REF:		/* never gp relative */
      break;

    case CONST:
      /* If -G 0, we can never have a GP relative memory operation.
	 Also, save some time if not optimizing.  */
      if (!TARGET_GP_OPT)
	return 0;

      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (XEXP (addr, 0), &offset);
	if (GET_CODE (op) != SYMBOL_REF)
	  return 0;

	/* let's be paranoid....  */
	if (! SMALL_INT (offset))
	  return 0;
      }

      /* fall through */

    case SYMBOL_REF:
      return SYMBOL_REF_FLAG (addr);
#endif

      /* This SYMBOL_REF case is for the mips16.  If the above case is
         reenabled, this one should be merged in.  */
    case SYMBOL_REF:
      /* References to the constant pool on the mips16 use a small
         offset if the function is small.  The only time we care about
         getting this right is during delayed branch scheduling, so
         don't need to check until then.  The machine_dependent_reorg
         function will set the total length of the instructions used
         in the function (cfun->machine->insns_len).  If that is small
         enough, we know for sure that this is a small offset.  It
         would be better if we could take into account the location of
         the instruction within the function, but we can't, because we
         don't know where we are.  */
      if (TARGET_MIPS16
	  && CONSTANT_POOL_ADDRESS_P (addr)
	  && cfun->machine->insns_len > 0)
	{
	  long size;

	  size = cfun->machine->insns_len + get_pool_size ();
	  if (GET_MODE_SIZE (mode) == 4)
	    return size < 4 * 0x100;
	  else if (GET_MODE_SIZE (mode) == 8)
	    return size < 8 * 0x20;
	  else
	    return 0;
	}

      return 0;

    default:
      break;
    }

  return 0;
}

/* Return nonzero for a memory address that can be used to load or store
   a doubleword.  */

int
double_memory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM
      || ! memory_operand (op, mode))
    {
      /* During reload, we accept a pseudo register if it has an
	 appropriate memory address.  If we don't do this, we will
	 wind up reloading into a register, and then reloading that
	 register from memory, when we could just reload directly from
	 memory.  */
      if (reload_in_progress
	  && GET_CODE (op) == REG
	  && REGNO (op) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[REGNO (op)] < 0
	  && reg_equiv_mem[REGNO (op)] != 0
	  && double_memory_operand (reg_equiv_mem[REGNO (op)], mode))
	return 1;

      /* All reloaded addresses are valid in TARGET_64BIT mode.  This is
	 the same test performed for 'm' in find_reloads.  */

      if (reload_in_progress
	  && TARGET_64BIT
	  && (GET_CODE (op) == MEM
	      || (GET_CODE (op) == REG
		  && REGNO (op) >= FIRST_PSEUDO_REGISTER
		  && reg_renumber[REGNO (op)] < 0)))
	return 1;

      if (reload_in_progress
	  && TARGET_MIPS16
	  && GET_CODE (op) == MEM)
	{
	  rtx addr;

	  addr = XEXP (op, 0);

	  /* During reload on the mips16, we accept a large offset
	     from the frame pointer or the stack pointer.  This large
	     address will get reloaded anyhow.  */
	  if (GET_CODE (addr) == PLUS
	      && GET_CODE (XEXP (addr, 0)) == REG
	      && (REGNO (XEXP (addr, 0)) == (unsigned) HARD_FRAME_POINTER_REGNUM
		  || REGNO (XEXP (addr, 0)) == STACK_POINTER_REGNUM)
	      && ((GET_CODE (XEXP (addr, 1)) == CONST_INT
		   && ! SMALL_INT (XEXP (addr, 1)))
		  || (GET_CODE (XEXP (addr, 1)) == SYMBOL_REF
		      && CONSTANT_POOL_ADDRESS_P (XEXP (addr, 1)))))
	    return 1;

	  /* Similarly, we accept a case where the memory address is
             itself on the stack, and will be reloaded.  */
	  if (GET_CODE (addr) == MEM)
	    {
	      rtx maddr;

	      maddr = XEXP (addr, 0);
	      if (GET_CODE (maddr) == PLUS
		  && GET_CODE (XEXP (maddr, 0)) == REG
		  && (REGNO (XEXP (maddr, 0)) == (unsigned) HARD_FRAME_POINTER_REGNUM
		      || REGNO (XEXP (maddr, 0)) == STACK_POINTER_REGNUM)
		  && ((GET_CODE (XEXP (maddr, 1)) == CONST_INT
		       && ! SMALL_INT (XEXP (maddr, 1)))
		      || (GET_CODE (XEXP (maddr, 1)) == SYMBOL_REF
			  && CONSTANT_POOL_ADDRESS_P (XEXP (maddr, 1)))))
		return 1;
	    }

	  /* We also accept the same case when we have a 16 bit signed
	     offset mixed in as well.  The large address will get
	     reloaded, and the 16 bit offset will be OK.  */
	  if (GET_CODE (addr) == PLUS
	      && GET_CODE (XEXP (addr, 0)) == MEM
	      && GET_CODE (XEXP (addr, 1)) == CONST_INT
	      && SMALL_INT (XEXP (addr, 1)))
	    {
	      addr = XEXP (XEXP (addr, 0), 0);
	      if (GET_CODE (addr) == PLUS
		  && GET_CODE (XEXP (addr, 0)) == REG
		  && (REGNO (XEXP (addr, 0)) == (unsigned) HARD_FRAME_POINTER_REGNUM
		      || REGNO (XEXP (addr, 0)) == STACK_POINTER_REGNUM)
		  && ((GET_CODE (XEXP (addr, 1)) == CONST_INT
		       && ! SMALL_INT (XEXP (addr, 1)))
		      || (GET_CODE (XEXP (addr, 1)) == SYMBOL_REF
			  && CONSTANT_POOL_ADDRESS_P (XEXP (addr, 1)))))
		return 1;
	    }
	}

      return 0;
    }

  if (TARGET_64BIT)
    {
      /* In this case we can use an instruction like sd.  */
      return 1;
    }

  /* Make sure that 4 added to the address is a valid memory address.
     This essentially just checks for overflow in an added constant.  */

  if (CONSTANT_ADDRESS_P (XEXP (op, 0)))
    return 1;

  op = adjust_address_nv (op, GET_MODE_CLASS (mode) == MODE_INT
			  ? SImode : SFmode, 4);
  return memory_address_p (GET_MODE (op), XEXP (op, 0));
}

/* Return nonzero if the code of this rtx pattern is EQ or NE.  */

int
equality_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  return GET_CODE (op) == EQ || GET_CODE (op) == NE;
}

/* Return nonzero if the code is a relational operations (EQ, LE, etc.) */

int
cmp_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  return GET_RTX_CLASS (GET_CODE (op)) == '<';
}

/* Return nonzero if the code is a relational operation suitable for a
   conditional trap instructuion (only EQ, NE, LT, LTU, GE, GEU).
   We need this in the insn that expands `trap_if' in order to prevent
   combine from erroneously altering the condition.  */

int
trap_cmp_op (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != GET_MODE (op))
    return 0;

  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case LT:
    case LTU:
    case GE:
    case GEU:
      return 1;

    default:
      return 0;
    }
}

/* Return nonzero if the operand is either the PC or a label_ref.  */

int
pc_or_label_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (op == pc_rtx)
    return 1;

  if (GET_CODE (op) == LABEL_REF)
    return 1;

  return 0;
}

/* Test for a valid operand for a call instruction.
   Don't allow the arg pointer register or virtual regs
   since they may change into reg + const, which the patterns
   can't handle yet.  */

int
call_insn_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (CONSTANT_ADDRESS_P (op)
	  || (GET_CODE (op) == REG && op != arg_pointer_rtx
	      && ! (REGNO (op) >= FIRST_PSEUDO_REGISTER
		    && REGNO (op) <= LAST_VIRTUAL_REGISTER)));
}

/* Return nonzero if OPERAND is valid as a source operand for a move
   instruction.  */

int
move_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Accept any general operand after reload has started; doing so
     avoids losing if reload does an in-place replacement of a register
     with a SYMBOL_REF or CONST.  */
  return (general_operand (op, mode)
	  && (! (mips_split_addresses && mips_check_split (op, mode))
	      || reload_in_progress || reload_completed)
	  && ! (TARGET_MIPS16
		&& GET_CODE (op) == SYMBOL_REF
		&& ! mips16_constant (op, mode, 1, 0)));
}

/* Return nonzero if OPERAND is valid as a source operand for movdi.
   This accepts not only general_operand, but also sign extended
   move_operands.  Note that we need to accept sign extended constants
   in case a sign extended register which is used in an expression,
   and is equivalent to a constant, is spilled.  We need to accept
   sign-extended memory in order to reload registers from stack slots,
   and so that we generate efficient code for extendsidi2.  */

int
movdi_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && move_operand (XEXP (op, 0), SImode))
    return 1;

  return (general_operand (op, mode)
	  && ! (TARGET_MIPS16
		&& GET_CODE (op) == SYMBOL_REF
		&& ! mips16_constant (op, mode, 1, 0)));
}

/* Like register_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return register_operand (op, mode);
}

/* Like reg_or_0_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return reg_or_0_operand (op, mode);
}

/* Like uns_arith_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return uns_arith_operand (op, mode);
}

/* Like arith_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return arith_operand (op, mode);
}

/* Like nonmemory_operand, but when in 64 bit mode also accept a sign
   extend of a 32 bit register, since the value is known to be already
   sign extended.  */

int
se_nonmemory_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (TARGET_64BIT
      && mode == DImode
      && GET_CODE (op) == SIGN_EXTEND
      && GET_MODE (op) == DImode
      && GET_MODE (XEXP (op, 0)) == SImode
      && register_operand (XEXP (op, 0), SImode))
    return 1;

  return nonmemory_operand (op, mode);
}

/* Accept any operand that can appear in a mips16 constant table
   instruction.  We can't use any of the standard operand functions
   because for these instructions we accept values that are not
   accepted by LEGITIMATE_CONSTANT, such as arbitrary SYMBOL_REFs.  */

int
consttable_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return CONSTANT_P (op);
}

/* Coprocessor operand; return true if rtx is a REG and refers to a
   coprocessor.  */

int
coprocessor_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == REG
	  && COP0_REG_FIRST <= REGNO (op)
	  && REGNO (op) <= COP3_REG_LAST);
}

int
coprocessor2_operand (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return (GET_CODE (op) == REG
	  && COP2_REG_FIRST <= REGNO (op)
	  && REGNO (op) <= COP2_REG_LAST);
}

/* Returns 1 if OP is a symbolic operand, i.e. a symbol_ref or a label_ref,
   possibly with an offset.  */

int
symbolic_operand (op, mode)
      register rtx op;
      enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;
  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;
  if (GET_CODE (op) == CONST
      && GET_CODE (XEXP (op,0)) == PLUS
      && GET_CODE (XEXP (XEXP (op,0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (op,0), 1)) == CONST_INT)
    return 1;
  return 0;
}

/* Return nonzero if we split the address into high and low parts.  */

/* ??? We should also handle reg+array somewhere.  We get four
   instructions currently, lui %hi/addui %lo/addui reg/lw.  Better is
   lui %hi/addui reg/lw %lo.  Fixing GO_IF_LEGITIMATE_ADDRESS to accept
   (plus (reg) (symbol_ref)) doesn't work because the SYMBOL_REF is broken
   out of the address, then we have 4 instructions to combine.  Perhaps
   add a 3->2 define_split for combine.  */

/* ??? We could also split a CONST_INT here if it is a large_int().
   However, it doesn't seem to be very useful to have %hi(constant).
   We would be better off by doing the masking ourselves and then putting
   the explicit high part of the constant in the RTL.  This will give better
   optimization.  Also, %hi(constant) needs assembler changes to work.
   There is already a define_split that does this.  */

int
mips_check_split (address, mode)
     rtx address;
     enum machine_mode mode;
{
  /* ??? This is the same check used in simple_memory_operand.
     We use it here because LO_SUM is not offsettable.  */
  if (GET_MODE_SIZE (mode) > (unsigned) UNITS_PER_WORD)
    return 0;

  if ((GET_CODE (address) == SYMBOL_REF && ! SYMBOL_REF_FLAG (address))
      || (GET_CODE (address) == CONST
	  && GET_CODE (XEXP (XEXP (address, 0), 0)) == SYMBOL_REF
	  && ! SYMBOL_REF_FLAG (XEXP (XEXP (address, 0), 0)))
      || GET_CODE (address) == LABEL_REF)
    return 1;

  return 0;
}

/* This function is used to implement REG_MODE_OK_FOR_BASE_P.  */

int
mips_reg_mode_ok_for_base_p (reg, mode, strict)
     rtx reg;
     enum machine_mode mode;
     int strict;
{
  return (strict
	  ? REGNO_MODE_OK_FOR_BASE_P (REGNO (reg), mode)
	  : GP_REG_OR_PSEUDO_NONSTRICT_P (REGNO (reg), mode));
}

/* This function is used to implement GO_IF_LEGITIMATE_ADDRESS.  It
   returns a nonzero value if XINSN is a legitimate address for a
   memory operand of the indicated MODE.  STRICT is nonzero if this
   function is called during reload.  */

int
mips_legitimate_address_p (mode, xinsn, strict)
     enum machine_mode mode;
     rtx xinsn;
     int strict;
{
  if (TARGET_DEBUG_B_MODE)
    {
      GO_PRINTF2 ("\n========== GO_IF_LEGITIMATE_ADDRESS, %sstrict\n",
		  strict ? "" : "not ");
      GO_DEBUG_RTX (xinsn);
    }

  /* Check for constant before stripping off SUBREG, so that we don't
     accept (subreg (const_int)) which will fail to reload.  */
  if (CONSTANT_ADDRESS_P (xinsn)
      && ! (mips_split_addresses && mips_check_split (xinsn, mode))
      && (! TARGET_MIPS16 || mips16_constant (xinsn, mode, 1, 0)))
    return 1;

  while (GET_CODE (xinsn) == SUBREG)
    xinsn = SUBREG_REG (xinsn);

  /* The mips16 can only use the stack pointer as a base register when
     loading SImode or DImode values.  */
  if (GET_CODE (xinsn) == REG
      && mips_reg_mode_ok_for_base_p (xinsn, mode, strict))
    return 1;

  if (GET_CODE (xinsn) == LO_SUM && mips_split_addresses)
    {
      register rtx xlow0 = XEXP (xinsn, 0);
      register rtx xlow1 = XEXP (xinsn, 1);

      while (GET_CODE (xlow0) == SUBREG)
	xlow0 = SUBREG_REG (xlow0);
      if (GET_CODE (xlow0) == REG
	  && mips_reg_mode_ok_for_base_p (xlow0, mode, strict)
	  && mips_check_split (xlow1, mode))
	return 1;
    }

  if (GET_CODE (xinsn) == PLUS)
    {
      register rtx xplus0 = XEXP (xinsn, 0);
      register rtx xplus1 = XEXP (xinsn, 1);
      register enum rtx_code code0;
      register enum rtx_code code1;

      while (GET_CODE (xplus0) == SUBREG)
	xplus0 = SUBREG_REG (xplus0);
      code0 = GET_CODE (xplus0);

      while (GET_CODE (xplus1) == SUBREG)
	xplus1 = SUBREG_REG (xplus1);
      code1 = GET_CODE (xplus1);

      /* The mips16 can only use the stack pointer as a base register
         when loading SImode or DImode values.  */
      if (code0 == REG
	  && mips_reg_mode_ok_for_base_p (xplus0, mode, strict))
	{
	  if (code1 == CONST_INT && SMALL_INT (xplus1))
	    return 1;

	  /* On the mips16, we represent GP relative offsets in RTL.
             These are 16 bit signed values, and can serve as register
             offsets.  */
	  if (TARGET_MIPS16
	      && mips16_gp_offset_p (xplus1))
	    return 1;

	  /* For some code sequences, you actually get better code by
	     pretending that the MIPS supports an address mode of a
	     constant address + a register, even though the real
	     machine doesn't support it.  This is because the
	     assembler can use $r1 to load just the high 16 bits, add
	     in the register, and fold the low 16 bits into the memory
	     reference, whereas the compiler generates a 4 instruction
	     sequence.  On the other hand, CSE is not as effective.
	     It would be a win to generate the lui directly, but the
	     MIPS assembler does not have syntax to generate the
	     appropriate relocation.  */

	  /* Also accept CONST_INT addresses here, so no else.  */
	  /* Reject combining an embedded PIC text segment reference
	     with a register.  That requires an additional
	     instruction.  */
          /* ??? Reject combining an address with a register for the MIPS
	     64 bit ABI, because the SGI assembler can not handle this.  */
	  if (!TARGET_DEBUG_A_MODE
	      && (mips_abi == ABI_32
		  || mips_abi == ABI_O64
		  || mips_abi == ABI_EABI)
	      && CONSTANT_ADDRESS_P (xplus1)
	      && ! mips_split_addresses
	      && (!TARGET_EMBEDDED_PIC
		  || code1 != CONST
		  || GET_CODE (XEXP (xplus1, 0)) != MINUS)
	      /* When assembling for machines with 64 bit registers,
	         the assembler will sign-extend the constant "foo"
		 in "la x, foo(x)" yielding the wrong result for:
	         (set (blah:DI) (plus x y)).  */
	      && (!TARGET_64BIT
		  || (code1 == CONST_INT
		      && trunc_int_for_mode (INTVAL (xplus1),
					     SImode) == INTVAL (xplus1)))
	      && !TARGET_MIPS16)
	    return 1;
	}
    }

  if (TARGET_DEBUG_B_MODE)
    GO_PRINTF ("Not a legitimate address\n");

  /* The address was not legitimate.  */
  return 0;
}


/* We need a lot of little routines to check constant values on the
   mips16.  These are used to figure out how long the instruction will
   be.  It would be much better to do this using constraints, but
   there aren't nearly enough letters available.  */

static int
m16_check_op (op, low, high, mask)
     rtx op;
     int low;
     int high;
     int mask;
{
  return (GET_CODE (op) == CONST_INT
	  && INTVAL (op) >= low
	  && INTVAL (op) <= high
	  && (INTVAL (op) & mask) == 0);
}

int
m16_uimm3_b (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, 0x1, 0x8, 0);
}

int
m16_simm4_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x8, 0x7, 0);
}

int
m16_nsimm4_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x7, 0x8, 0);
}

int
m16_simm5_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x10, 0xf, 0);
}

int
m16_nsimm5_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0xf, 0x10, 0);
}

int
m16_uimm5_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, (- 0x10) << 2, 0xf << 2, 3);
}

int
m16_nuimm5_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, (- 0xf) << 2, 0x10 << 2, 3);
}

int
m16_simm8_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x80, 0x7f, 0);
}

int
m16_nsimm8_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x7f, 0x80, 0);
}

int
m16_uimm8_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, 0x0, 0xff, 0);
}

int
m16_nuimm8_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0xff, 0x0, 0);
}

int
m16_uimm8_m1_1 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, - 0x1, 0xfe, 0);
}

int
m16_uimm8_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, 0x0, 0xff << 2, 3);
}

int
m16_nuimm8_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, (- 0xff) << 2, 0x0, 3);
}

int
m16_simm8_8 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, (- 0x80) << 3, 0x7f << 3, 7);
}

int
m16_nsimm8_8 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  return m16_check_op (op, (- 0x7f) << 3, 0x80 << 3, 7);
}

/* References to the string table on the mips16 only use a small
   offset if the function is small.  See the comment in the SYMBOL_REF
   case in simple_memory_operand.  We can't check for LABEL_REF here,
   because the offset is always large if the label is before the
   referencing instruction.  */

int
m16_usym8_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF
      && SYMBOL_REF_FLAG (op)
      && cfun->machine->insns_len > 0
      && XSTR (op, 0)[0] == '*'
      && strncmp (XSTR (op, 0) + 1, LOCAL_LABEL_PREFIX,
		  sizeof LOCAL_LABEL_PREFIX - 1) == 0
      && (cfun->machine->insns_len + get_pool_size () + mips_string_length
	  < 4 * 0x100))
    {
      struct string_constant *l;

      /* Make sure this symbol is on thelist of string constants to be
         output for this function.  It is possible that it has already
         been output, in which case this requires a large offset.  */
      for (l = string_constants; l != NULL; l = l->next)
	if (strcmp (l->label, XSTR (op, 0)) == 0)
	  return 1;
    }

  return 0;
}

int
m16_usym5_4 (op, mode)
     rtx op;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  if (GET_CODE (op) == SYMBOL_REF
      && SYMBOL_REF_FLAG (op)
      && cfun->machine->insns_len > 0
      && XSTR (op, 0)[0] == '*'
      && strncmp (XSTR (op, 0) + 1, LOCAL_LABEL_PREFIX,
		  sizeof LOCAL_LABEL_PREFIX - 1) == 0
      && (cfun->machine->insns_len + get_pool_size () + mips_string_length
	  < 4 * 0x20))
    {
      struct string_constant *l;

      /* Make sure this symbol is on thelist of string constants to be
         output for this function.  It is possible that it has already
         been output, in which case this requires a large offset.  */
      for (l = string_constants; l != NULL; l = l->next)
	if (strcmp (l->label, XSTR (op, 0)) == 0)
	  return 1;
    }

  return 0;
}

/* Returns an operand string for the given instruction's delay slot,
   after updating filled delay slot statistics.

   We assume that operands[0] is the target register that is set.

   In order to check the next insn, most of this functionality is moved
   to FINAL_PRESCAN_INSN, and we just set the global variables that
   it needs.  */

/* ??? This function no longer does anything useful, because final_prescan_insn
   now will never emit a nop.  */

const char *
mips_fill_delay_slot (ret, type, operands, cur_insn)
     const char *ret;		/* normal string to return */
     enum delay_type type;	/* type of delay */
     rtx operands[];		/* operands to use */
     rtx cur_insn;		/* current insn */
{
  register rtx set_reg;
  register enum machine_mode mode;
  register rtx next_insn = cur_insn ? NEXT_INSN (cur_insn) : NULL_RTX;
  register int num_nops;

  if (type == DELAY_LOAD || type == DELAY_FCMP)
    num_nops = 1;

  else if (type == DELAY_HILO)
    num_nops = 2;

  else
    num_nops = 0;

  /* Make sure that we don't put nop's after labels.  */
  next_insn = NEXT_INSN (cur_insn);
  while (next_insn != 0 && GET_CODE (next_insn) == NOTE)
    next_insn = NEXT_INSN (next_insn);

  dslots_load_total += num_nops;
  if (TARGET_DEBUG_F_MODE
      || !optimize
      || type == DELAY_NONE
      || operands == 0
      || cur_insn == 0
      || next_insn == 0
      || GET_CODE (next_insn) == CODE_LABEL
      || (set_reg = operands[0]) == 0)
    {
      dslots_number_nops = 0;
      mips_load_reg  = 0;
      mips_load_reg2 = 0;
      mips_load_reg3 = 0;
      mips_load_reg4 = 0;
      return ret;
    }

  set_reg = operands[0];
  if (set_reg == 0)
    return ret;

  while (GET_CODE (set_reg) == SUBREG)
    set_reg = SUBREG_REG (set_reg);

  mode = GET_MODE (set_reg);
  dslots_number_nops = num_nops;
  mips_load_reg = set_reg;
  if (GET_MODE_SIZE (mode)
      > (unsigned) (FP_REG_P (REGNO (set_reg)) ? UNITS_PER_FPREG : UNITS_PER_WORD))
    mips_load_reg2 = gen_rtx_REG (SImode, REGNO (set_reg) + 1);
  else
    mips_load_reg2 = 0;

  if (type == DELAY_HILO)
    {
      mips_load_reg3 = gen_rtx_REG (SImode, MD_REG_FIRST);
      mips_load_reg4 = gen_rtx_REG (SImode, MD_REG_FIRST+1);
    }
  else
    {
      mips_load_reg3 = 0;
      mips_load_reg4 = 0;
    }

  return ret;
}


/* Determine whether a memory reference takes one (based off of the GP
   pointer), two (normal), or three (label + reg) instructions, and bump the
   appropriate counter for -mstats.  */

void
mips_count_memory_refs (op, num)
     rtx op;
     int num;
{
  int additional = 0;
  int n_words = 0;
  rtx addr, plus0, plus1;
  enum rtx_code code0, code1;
  int looping;

  if (TARGET_DEBUG_B_MODE)
    {
      fprintf (stderr, "\n========== mips_count_memory_refs:\n");
      debug_rtx (op);
    }

  /* Skip MEM if passed, otherwise handle movsi of address.  */
  addr = (GET_CODE (op) != MEM) ? op : XEXP (op, 0);

  /* Loop, going through the address RTL.  */
  do
    {
      looping = FALSE;
      switch (GET_CODE (addr))
	{
	case REG:
	case CONST_INT:
	case LO_SUM:
	  break;

	case PLUS:
	  plus0 = XEXP (addr, 0);
	  plus1 = XEXP (addr, 1);
	  code0 = GET_CODE (plus0);
	  code1 = GET_CODE (plus1);

	  if (code0 == REG)
	    {
	      additional++;
	      addr = plus1;
	      looping = 1;
	      continue;
	    }

	  if (code0 == CONST_INT)
	    {
	      addr = plus1;
	      looping = 1;
	      continue;
	    }

	  if (code1 == REG)
	    {
	      additional++;
	      addr = plus0;
	      looping = 1;
	      continue;
	    }

	  if (code1 == CONST_INT)
	    {
	      addr = plus0;
	      looping = 1;
	      continue;
	    }

	  if (code0 == SYMBOL_REF || code0 == LABEL_REF || code0 == CONST)
	    {
	      addr = plus0;
	      looping = 1;
	      continue;
	    }

	  if (code1 == SYMBOL_REF || code1 == LABEL_REF || code1 == CONST)
	    {
	      addr = plus1;
	      looping = 1;
	      continue;
	    }

	  break;

	case LABEL_REF:
	  n_words = 2;		/* always 2 words */
	  break;

	case CONST:
	  addr = XEXP (addr, 0);
	  looping = 1;
	  continue;

	case SYMBOL_REF:
	  n_words = SYMBOL_REF_FLAG (addr) ? 1 : 2;
	  break;

	default:
	  break;
	}
    }
  while (looping);

  if (n_words == 0)
    return;

  n_words += additional;
  if (n_words > 3)
    n_words = 3;

  num_refs[n_words-1] += num;
}


/* Return a pseudo that points to the address of the current function.
   The first time it is called for a function, an initializer for the
   pseudo is emitted in the beginning of the function.  */

rtx
embedded_pic_fnaddr_reg ()
{
  if (cfun->machine->embedded_pic_fnaddr_rtx == NULL)
    {
      rtx seq;

      cfun->machine->embedded_pic_fnaddr_rtx = gen_reg_rtx (Pmode);

      /* Output code at function start to initialize the pseudo-reg.  */
      /* ??? We used to do this in FINALIZE_PIC, but that does not work for
	 inline functions, because it is called after RTL for the function
	 has been copied.  The pseudo-reg in embedded_pic_fnaddr_rtx however
	 does not get copied, and ends up not matching the rest of the RTL.
	 This solution works, but means that we get unnecessary code to
	 initialize this value every time a function is inlined into another
	 function.  */
      start_sequence ();
      emit_insn (gen_get_fnaddr (cfun->machine->embedded_pic_fnaddr_rtx,
				 XEXP (DECL_RTL (current_function_decl), 0)));
      seq = get_insns ();
      end_sequence ();
      push_topmost_sequence ();
      emit_insn_after (seq, get_insns ());
      pop_topmost_sequence ();
    }

  return cfun->machine->embedded_pic_fnaddr_rtx;
}

/* Return RTL for the offset from the current function to the argument.
   X is the symbol whose offset from the current function we want.  */

rtx
embedded_pic_offset (x)
     rtx x;
{
  /* Make sure it is emitted.  */
  embedded_pic_fnaddr_reg ();

  return
    gen_rtx_CONST (Pmode,
		   gen_rtx_MINUS (Pmode, x,
				  XEXP (DECL_RTL (current_function_decl), 0)));
}

/* Return the appropriate instructions to move one operand to another.  */

const char *
mips_move_1word (operands, insn, unsignedp)
     rtx operands[];
     rtx insn;
     int unsignedp;
{
  const char *ret = 0;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  enum rtx_code code0 = GET_CODE (op0);
  enum rtx_code code1 = GET_CODE (op1);
  enum machine_mode mode = GET_MODE (op0);
  int subreg_offset0 = 0;
  int subreg_offset1 = 0;
  enum delay_type delay = DELAY_NONE;

  while (code0 == SUBREG)
    {
      subreg_offset0 += subreg_regno_offset (REGNO (SUBREG_REG (op0)),
					     GET_MODE (SUBREG_REG (op0)),
					     SUBREG_BYTE (op0),
					     GET_MODE (op0));
      op0 = SUBREG_REG (op0);
      code0 = GET_CODE (op0);
    }

  while (code1 == SUBREG)
    {
      subreg_offset1 += subreg_regno_offset (REGNO (SUBREG_REG (op1)),
					     GET_MODE (SUBREG_REG (op1)),
					     SUBREG_BYTE (op1),
					     GET_MODE (op1));
      op1 = SUBREG_REG (op1);
      code1 = GET_CODE (op1);
    }

  /* For our purposes, a condition code mode is the same as SImode.  */
  if (mode == CCmode)
    mode = SImode;

  if (code0 == REG)
    {
      int regno0 = REGNO (op0) + subreg_offset0;

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_offset1;

	  /* Just in case, don't do anything for assigning a register
	     to itself, unless we are filling a delay slot.  */
	  if (regno0 == regno1 && set_nomacro == 0)
	    ret = "";

	  else if (GP_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		ret = "move\t%0,%1";

	      else if (MD_REG_P (regno1))
		{
		  delay = DELAY_HILO;
		  if (regno1 != HILO_REGNUM)
		    ret = "mf%1\t%0";
		  else
		    ret = "mflo\t%0";
		}

	      else if (ST_REG_P (regno1) && ISA_HAS_8CC)
		ret = "li\t%0,1\n\tmovf\t%0,%.,%1";

	      else
		{
		  delay = DELAY_LOAD;
		  if (FP_REG_P (regno1))
		    ret = "mfc1\t%0,%1";
		  else if (ALL_COP_REG_P (regno1))
		    {
		      static char retval[] = "mfc_\t%0,%1";

		      retval[3] = COPNUM_AS_CHAR_FROM_REGNUM (regno1);
		      ret = retval;
		    }
		  else if (regno1 == FPSW_REGNUM && ! ISA_HAS_8CC)
		    ret = "cfc1\t%0,$31";
		}
	    }

	  else if (FP_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%1,%0";
		}

	      if (FP_REG_P (regno1))
		ret = "mov.s\t%0,%1";
	    }

	  else if (MD_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_HILO;
		  if (regno0 != HILO_REGNUM && ! TARGET_MIPS16)
		    ret = "mt%0\t%1";
		}
	    }

	  else if (regno0 == FPSW_REGNUM && ! ISA_HAS_8CC)
	    {
	      if (GP_REG_P (regno1))
		{
		  delay = DELAY_LOAD;
		  ret = "ctc1\t%0,$31";
		}
	    }
	  else if (ALL_COP_REG_P (regno0))
	    {
	      if (GP_REG_P (regno1))
		{
		  static char retval[] = "mtc_\t%1,%0";
		  char cop = COPNUM_AS_CHAR_FROM_REGNUM (regno0);

		  if (cop == '0')
		    abort_with_insn (insn,
				     "mtc0 not supported; it disturbs virtual address translation");
		  delay = DELAY_LOAD;
		  retval[3] = cop;
		  ret = retval;
		}
	    }
	}

      else if (code1 == MEM)
	{
	  delay = DELAY_LOAD;

	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 1);

	  if (GP_REG_P (regno0))
	    {
	      /* For loads, use the mode of the memory item, instead of the
		 target, so zero/sign extend can use this code as well.  */
	      switch (GET_MODE (op1))
		{
		default:
		  break;
		case SFmode:
		  ret = "lw\t%0,%1";
		  break;
		case SImode:
		case CCmode:
		  ret = ((unsignedp && TARGET_64BIT)
			 ? "lwu\t%0,%1"
			 : "lw\t%0,%1");
		  break;
		case HImode:
		  ret = (unsignedp) ? "lhu\t%0,%1" : "lh\t%0,%1";
		  break;
		case QImode:
		  ret = (unsignedp) ? "lbu\t%0,%1" : "lb\t%0,%1";
		  break;
		}
	    }

	  else if (FP_REG_P (regno0) && (mode == SImode || mode == SFmode))
	    ret = "l.s\t%0,%1";

	  else if (ALL_COP_REG_P (regno0))
	    {
	      static char retval[] = "lwc_\t%0,%1";
	      char cop = COPNUM_AS_CHAR_FROM_REGNUM (regno0);

	      if (cop == '0')
		abort_with_insn (insn,
				 "loads from memory to COP0 are illegal");
	      delay = DELAY_LOAD;
	      retval[3] = cop;
	      ret = retval;
	    }

	  if (ret != (char *)0 && MEM_VOLATILE_P (op1))
	    {
	      size_t i = strlen (ret);
	      if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
		abort ();

	      sprintf (volatile_buffer, "%%{%s%%}", ret);
	      ret = volatile_buffer;
	    }
	}

      else if (code1 == CONST_INT
	       || (code1 == CONST_DOUBLE
		   && GET_MODE (op1) == VOIDmode))
	{
	  if (code1 == CONST_DOUBLE)
	    {
	      /* This can happen when storing constants into long long
                 bitfields.  Just store the least significant word of
                 the value.  */
	      operands[1] = op1 = GEN_INT (CONST_DOUBLE_LOW (op1));
	    }

	  if (INTVAL (op1) == 0 && ! TARGET_MIPS16)
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%z1";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%z1,%0";
		}

	      else if (MD_REG_P (regno0))
		{
		  delay = DELAY_HILO;
		  ret = "mt%0\t%.";
		}
	    }

	  else if (GP_REG_P (regno0))
	    {
	      /* Don't use X format, because that will give out of
		 range numbers for 64 bit host and 32 bit target.  */
	      if (! TARGET_MIPS16)
		ret = "li\t%0,%1\t\t\t# %X1";
	      else
		{
		  if (INTVAL (op1) >= 0 && INTVAL (op1) <= 0xffff)
		    ret = "li\t%0,%1";
		  else if (INTVAL (op1) < 0 && INTVAL (op1) >= -0xffff)
		    ret = "li\t%0,%n1\n\tneg\t%0";
		}
	    }
	}

      else if (code1 == CONST_DOUBLE && mode == SFmode)
	{
	  if (op1 == CONST0_RTX (SFmode))
	    {
	      if (GP_REG_P (regno0))
		ret = "move\t%0,%.";

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = "mtc1\t%.,%0";
		}
	    }

	  else
	    {
	      delay = DELAY_LOAD;
	      ret = "li.s\t%0,%1";
	    }
	}

      else if (code1 == LABEL_REF)
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 1);

	  ret = "la\t%0,%a1";
	}

      else if (code1 == SYMBOL_REF || code1 == CONST)
	{
	  if (TARGET_MIPS16
	      && code1 == CONST
	      && GET_CODE (XEXP (op1, 0)) == REG
	      && REGNO (XEXP (op1, 0)) == GP_REG_FIRST + 28)
	    {
	      /* This case arises on the mips16; see
                 mips16_gp_pseudo_reg.  */
	      ret = "move\t%0,%+";
	    }
	  else if (TARGET_MIPS16
		   && code1 == SYMBOL_REF
		   && SYMBOL_REF_FLAG (op1)
		   && (XSTR (op1, 0)[0] != '*'
		       || strncmp (XSTR (op1, 0) + 1,
				   LOCAL_LABEL_PREFIX,
				   sizeof LOCAL_LABEL_PREFIX - 1) != 0))
	    {
	      /* This can occur when reloading the address of a GP
                 relative symbol on the mips16.  */
	      ret = "move\t%0,%+\n\taddu\t%0,%%gprel(%a1)";
	    }
	  else
	    {
	      if (TARGET_STATS)
		mips_count_memory_refs (op1, 1);

	      ret = "la\t%0,%a1";
	    }
	}

      else if (code1 == PLUS)
	{
	  rtx add_op0 = XEXP (op1, 0);
	  rtx add_op1 = XEXP (op1, 1);

	  if (GET_CODE (XEXP (op1, 1)) == REG
	      && GET_CODE (XEXP (op1, 0)) == CONST_INT)
	    add_op0 = XEXP (op1, 1), add_op1 = XEXP (op1, 0);

	  operands[2] = add_op0;
	  operands[3] = add_op1;
	  ret = "add%:\t%0,%2,%3";
	}

      else if (code1 == HIGH)
	{
	  operands[1] = XEXP (op1, 0);
	  ret = "lui\t%0,%%hi(%1)";
	}
    }

  else if (code0 == MEM)
    {
      if (TARGET_STATS)
	mips_count_memory_refs (op0, 1);

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_offset1;

	  if (GP_REG_P (regno1))
	    {
	      switch (mode)
		{
		case SFmode: ret = "sw\t%1,%0"; break;
		case SImode: ret = "sw\t%1,%0"; break;
		case HImode: ret = "sh\t%1,%0"; break;
		case QImode: ret = "sb\t%1,%0"; break;
		default: break;
		}
	    }

	  else if (FP_REG_P (regno1) && (mode == SImode || mode == SFmode))
	    ret = "s.s\t%1,%0";
	  else if (ALL_COP_REG_P (regno1))
	    {
	      static char retval[] = "swc_\t%1,%0";

	      retval[3] = COPNUM_AS_CHAR_FROM_REGNUM (regno1);
	      ret = retval;
	    }
	}

      else if (code1 == CONST_INT && INTVAL (op1) == 0)
	{
	  switch (mode)
	    {
	    case SFmode: ret = "sw\t%z1,%0"; break;
	    case SImode: ret = "sw\t%z1,%0"; break;
	    case HImode: ret = "sh\t%z1,%0"; break;
	    case QImode: ret = "sb\t%z1,%0"; break;
	    default: break;
	    }
	}

      else if (code1 == CONST_DOUBLE && op1 == CONST0_RTX (mode))
	{
	  switch (mode)
	    {
	    case SFmode: ret = "sw\t%.,%0"; break;
	    case SImode: ret = "sw\t%.,%0"; break;
	    case HImode: ret = "sh\t%.,%0"; break;
	    case QImode: ret = "sb\t%.,%0"; break;
	    default: break;
	    }
	}

      if (ret != 0 && MEM_VOLATILE_P (op0))
	{
	  size_t i = strlen (ret);

	  if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
	    abort ();

	  sprintf (volatile_buffer, "%%{%s%%}", ret);
	  ret = volatile_buffer;
	}
    }

  if (ret == 0)
    {
      abort_with_insn (insn, "bad move");
      return 0;
    }

  if (delay != DELAY_NONE)
    return mips_fill_delay_slot (ret, delay, operands, insn);

  return ret;
}

/* Return instructions to restore the global pointer from the stack,
   assuming TARGET_ABICALLS.  Used by exception_receiver to set up
   the GP for exception handlers.

   OPERANDS is an array of operands whose contents are undefined
   on entry.  INSN is the exception_handler instruction.  */

const char *
mips_restore_gp (operands, insn)
     rtx *operands, insn;
{
  rtx loc;

  operands[0] = pic_offset_table_rtx;
  if (frame_pointer_needed)
    loc = hard_frame_pointer_rtx;
  else
    loc = stack_pointer_rtx;
  loc = plus_constant (loc, cfun->machine->frame.args_size);
  operands[1] = gen_rtx_MEM (Pmode, loc);

  return mips_move_1word (operands, insn, 0);
}

/* Return an instruction to sign-extend SImode value SRC and store it
   in DImode value DEST.  INSN is the original extendsidi2-type insn.  */

const char *
mips_sign_extend (insn, dest, src)
     rtx insn, dest, src;
{
  rtx operands[MAX_RECOG_OPERANDS];

  if ((register_operand (src, SImode) && FP_REG_P (true_regnum (src)))
      || memory_operand (src, SImode))
    {
      /* If the source is a floating-point register, we need to use a
	 32-bit move, since the float register is not kept sign-extended.
	 If the source is in memory, we need a 32-bit load.  */
      operands[0] = gen_lowpart_SUBREG (SImode, dest);
      operands[1] = src;
      return mips_move_1word (operands, insn, false);
    }
  else
    {
      operands[0] = dest;
      operands[1] = src;
      return mips_move_2words (operands, insn);
    }
}

/* Return the appropriate instructions to move 2 words */

const char *
mips_move_2words (operands, insn)
     rtx operands[];
     rtx insn;
{
  const char *ret = 0;
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  int subreg_offset0 = 0;
  int subreg_offset1 = 0;
  enum delay_type delay = DELAY_NONE;

  if (code1 == SIGN_EXTEND)
    return mips_sign_extend (insn, op0, XEXP (op1, 0));

  while (code0 == SUBREG)
    {
      subreg_offset0 += subreg_regno_offset (REGNO (SUBREG_REG (op0)),
					     GET_MODE (SUBREG_REG (op0)),
					     SUBREG_BYTE (op0),
					     GET_MODE (op0));
      op0 = SUBREG_REG (op0);
      code0 = GET_CODE (op0);
    }

  while (code1 == SUBREG)
    {
      subreg_offset1 += subreg_regno_offset (REGNO (SUBREG_REG (op1)),
					     GET_MODE (SUBREG_REG (op1)),
					     SUBREG_BYTE (op1),
					     GET_MODE (op1));
      op1 = SUBREG_REG (op1);
      code1 = GET_CODE (op1);
    }

  if (code0 == REG)
    {
      int regno0 = REGNO (op0) + subreg_offset0;

      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_offset1;

	  /* Just in case, don't do anything for assigning a register
	     to itself, unless we are filling a delay slot.  */
	  if (regno0 == regno1 && set_nomacro == 0)
	    ret = "";

	  else if (FP_REG_P (regno0))
	    {
	      if (FP_REG_P (regno1))
		ret = "mov.d\t%0,%1";

	      else
		{
		  delay = DELAY_LOAD;
		  if (TARGET_FLOAT64)
		    {
		      if (!TARGET_64BIT)
			abort_with_insn (insn, "bad move");

#ifdef TARGET_FP_CALL_32
		      if (FP_CALL_GP_REG_P (regno1))
			ret = "dsll\t%1,32\n\tor\t%1,%D1\n\tdmtc1\t%1,%0";
		      else
#endif
			ret = "dmtc1\t%1,%0";
		    }
		  else
		    ret = "mtc1\t%L1,%0\n\tmtc1\t%M1,%D0";
		}
	    }

	  else if (FP_REG_P (regno1))
	    {
	      delay = DELAY_LOAD;
	      if (TARGET_FLOAT64)
		{
		  if (!TARGET_64BIT)
		    abort_with_insn (insn, "bad move");

#ifdef TARGET_FP_CALL_32
		  if (FP_CALL_GP_REG_P (regno0))
		    ret = "dmfc1\t%0,%1\n\tmfc1\t%D0,%1\n\tdsrl\t%0,32";
		  else
#endif
		    ret = "dmfc1\t%0,%1";
		}
	      else
		ret = "mfc1\t%L0,%1\n\tmfc1\t%M0,%D1";
	    }

	  else if (MD_REG_P (regno0) && GP_REG_P (regno1) && !TARGET_MIPS16)
	    {
	      delay = DELAY_HILO;
	      if (TARGET_64BIT)
		{
		  if (regno0 != HILO_REGNUM)
		    ret = "mt%0\t%1";
		  else if (regno1 == 0)
		    ret = "mtlo\t%.\n\tmthi\t%.";
		}
	      else
		ret = "mthi\t%M1\n\tmtlo\t%L1";
	    }

	  else if (GP_REG_P (regno0) && MD_REG_P (regno1))
	    {
	      delay = DELAY_HILO;
	      if (TARGET_64BIT)
		{
		  if (regno1 != HILO_REGNUM)
		    ret = "mf%1\t%0";
		}
	      else
		ret = "mfhi\t%M0\n\tmflo\t%L0";
	    }
	  else if (GP_REG_P (regno0) && ALL_COP_REG_P (regno1)
		   && TARGET_64BIT)
	    {
	      static char retval[] = "dmfc_\t%0,%1";

	      delay = DELAY_LOAD;
	      retval[4] = COPNUM_AS_CHAR_FROM_REGNUM (regno1);
	      ret = retval;
	    }
	  else if (ALL_COP_REG_P (regno0) && GP_REG_P (regno1)
		   && TARGET_64BIT)
	    {
	      static char retval[] = "dmtc_\t%1,%0";
	      char cop = COPNUM_AS_CHAR_FROM_REGNUM (regno0);

	      if (cop == '0')
		abort_with_insn (insn,
				 "dmtc0 not supported; it disturbs virtual address translation");
	      delay = DELAY_LOAD;
	      retval[4] = cop;
	      ret = retval;
	    }
	  else if (TARGET_64BIT)
	    ret = "move\t%0,%1";

	  else if (regno0 != (regno1+1))
	    ret = "move\t%0,%1\n\tmove\t%D0,%D1";

	  else
	    ret = "move\t%D0,%D1\n\tmove\t%0,%1";
	}

      else if (code1 == CONST_DOUBLE)
	{
	  /* Move zero from $0 unless !TARGET_64BIT and recipient
	     is 64-bit fp reg, in which case generate a constant.  */
	  if (op1 != CONST0_RTX (GET_MODE (op1))
	      || (TARGET_FLOAT64 && !TARGET_64BIT && FP_REG_P (regno0)))
	    {
	      if (GET_MODE (op1) == DFmode)
		{
		  delay = DELAY_LOAD;

#ifdef TARGET_FP_CALL_32
		  if (FP_CALL_GP_REG_P (regno0))
		    {
		      if (TARGET_FLOAT64 && !TARGET_64BIT)
			{
			  split_double (op1, operands + 2, operands + 3);
			  ret = "li\t%0,%2\n\tli\t%D0,%3";
			}
		      else
			ret = "li.d\t%0,%1\n\tdsll\t%D0,%0,32\n\tdsrl\t%D0,32\n\tdsrl\t%0,32";
		    }
		  else
#endif
		    /* GNU as emits 64-bit code for li.d if the ISA is 3
		       or higher.  For !TARGET_64BIT && gp registers we
		       need to avoid this by using two li instructions
		       instead.  */
		    if (ISA_HAS_64BIT_REGS
			&& ! TARGET_64BIT
			&& ! FP_REG_P (regno0))
		      {
			split_double (op1, operands + 2, operands + 3);
			ret = "li\t%0,%2\n\tli\t%D0,%3";
		      }
		    else
		      ret = "li.d\t%0,%1";
		}

	      else if (TARGET_64BIT)
		{
		  if (! TARGET_MIPS16)
		    ret = "dli\t%0,%1";
		}

	      else
		{
		  split_double (op1, operands + 2, operands + 3);
		  ret = "li\t%0,%2\n\tli\t%D0,%3";
		}
	    }

	  else
	    {
	      if (GP_REG_P (regno0))
		ret = (TARGET_64BIT
#ifdef TARGET_FP_CALL_32
		       && ! FP_CALL_GP_REG_P (regno0)
#endif
		       ? "move\t%0,%."
		       : "move\t%0,%.\n\tmove\t%D0,%.");

	      else if (FP_REG_P (regno0))
		{
		  delay = DELAY_LOAD;
		  ret = (TARGET_64BIT
			 ? "dmtc1\t%.,%0"
			 : "mtc1\t%.,%0\n\tmtc1\t%.,%D0");
		}
	    }
	}

      else if (code1 == CONST_INT && INTVAL (op1) == 0 && ! TARGET_MIPS16)
	{
	  if (GP_REG_P (regno0))
	    ret = (TARGET_64BIT
		   ? "move\t%0,%."
		   : "move\t%0,%.\n\tmove\t%D0,%.");

	  else if (FP_REG_P (regno0))
	    {
	      delay = DELAY_LOAD;
	      ret = (TARGET_64BIT
		     ? "dmtc1\t%.,%0"
		     : (TARGET_FLOAT64
			? "li.d\t%0,%1"
			: "mtc1\t%.,%0\n\tmtc1\t%.,%D0"));
	    }
	  else if (MD_REG_P (regno0))
	    {
	      delay = DELAY_HILO;
	      ret =  (regno0 == HILO_REGNUM
		      ? "mtlo\t%.\n\tmthi\t%."
		      : "mt%0\t%.\n");
	    }
	}

      else if (code1 == CONST_INT && GET_MODE (op0) == DImode
	       && GP_REG_P (regno0))
	{
	  if (TARGET_64BIT)
	    {
	      if (TARGET_MIPS16)
		{
		  if (INTVAL (op1) >= 0 && INTVAL (op1) <= 0xffff)
		    ret = "li\t%0,%1";
		  else if (INTVAL (op1) < 0 && INTVAL (op1) >= -0xffff)
		    ret = "li\t%0,%n1\n\tneg\t%0";
		}
	      else if (GET_CODE (operands[1]) == SIGN_EXTEND)
		ret = "li\t%0,%1\t\t# %X1";
	      else if (HOST_BITS_PER_WIDE_INT < 64)
		/* We can't use 'X' for negative numbers, because then we won't
		   get the right value for the upper 32 bits.  */
		ret = (INTVAL (op1) < 0
		       ? "dli\t%0,%1\t\t\t# %X1"
		       : "dli\t%0,%X1\t\t# %1");
	      else
		/* We must use 'X', because otherwise LONG_MIN will print as
		   a number that the assembler won't accept.  */
		ret = "dli\t%0,%X1\t\t# %1";
	    }
	  else if (HOST_BITS_PER_WIDE_INT < 64)
	    {
	      operands[2] = GEN_INT (INTVAL (operands[1]) >= 0 ? 0 : -1);
	      if (TARGET_MIPS16)
		{
		  if (INTVAL (op1) >= 0 && INTVAL (op1) <= 0xffff)
		    ret = "li\t%M0,%2\n\tli\t%L0,%1";
		  else if (INTVAL (op1) < 0 && INTVAL (op1) >= -0xffff)
		    {
		      operands[2] = GEN_INT (1);
		      ret = "li\t%M0,%2\n\tneg\t%M0\n\tli\t%L0,%n1\n\tneg\t%L0";
		    }
		}
	      else
		ret = "li\t%M0,%2\n\tli\t%L0,%1";
	    }
	  else
	    {
	      /* We use multiple shifts here, to avoid warnings about out
		 of range shifts on 32 bit hosts.  */
	      operands[2] = GEN_INT (INTVAL (operands[1]) >> 16 >> 16);
	      operands[1]
		= GEN_INT (INTVAL (operands[1]) << 16 << 16 >> 16 >> 16);
	      if (TARGET_MIPS16)
		{
		  if (INTVAL (op1) >= 0 && INTVAL (op1) <= 0xffff)
		    ret = "li\t%M0,%2\n\tli\t%L0,%1";
		  else if (INTVAL (op1) < 0 && INTVAL (op1) >= -0xffff)
		    {
		      operands[2] = GEN_INT (1);
		      ret = "li\t%M0,%2\n\tneg\t%M0\n\tli\t%L0,%n1\n\tneg\t%L0";
		    }
		}
	      else
		ret = "li\t%M0,%2\n\tli\t%L0,%1";
	    }
	}

      else if (code1 == MEM)
	{
	  delay = DELAY_LOAD;

	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 2);

	  if (FP_REG_P (regno0))
	    ret = "l.d\t%0,%1";

	  else if (ALL_COP_REG_P (regno0) && TARGET_64BIT)
	    {
	      static char retval[] = "ldc_\t%0,%1";
	      char cop = COPNUM_AS_CHAR_FROM_REGNUM (regno0);

	      if (cop == '0')
		abort_with_insn (insn,
				 "loads from memory to COP0 are illegal");
	      delay = DELAY_LOAD;
	      retval[3] = cop;
	      ret = retval;
	    }

	  else if (TARGET_64BIT)
	    {

#ifdef TARGET_FP_CALL_32
	      if (FP_CALL_GP_REG_P (regno0))
		ret = (double_memory_operand (op1, GET_MODE (op1))
		       ? "lwu\t%0,%1\n\tlwu\t%D0,4+%1"
		       : "ld\t%0,%1\n\tdsll\t%D0,%0,32\n\tdsrl\t%D0,32\n\tdsrl\t%0,32");
	      else
#endif
		ret = "ld\t%0,%1";
	    }

	  else if (double_memory_operand (op1, GET_MODE (op1)))
	    ret = (reg_mentioned_p (op0, op1)
		   ? "lw\t%D0,%D1\n\tlw\t%0,%1"
		   : "lw\t%0,%1\n\tlw\t%D0,%D1");

	  if (ret != 0 && MEM_VOLATILE_P (op1))
	    {
	      size_t i = strlen (ret);

	      if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
		abort ();

	      sprintf (volatile_buffer, "%%{%s%%}", ret);
	      ret = volatile_buffer;
	    }
	}

      else if (code1 == LABEL_REF)
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (op1, 2);

	  if (GET_CODE (operands[1]) == SIGN_EXTEND)
	    /* We deliberately remove the 'a' from '%1', so that we don't
	       have to add SIGN_EXTEND support to print_operand_address.
	       print_operand will just call print_operand_address in this
	       case, so there is no problem.  */
	    ret = "la\t%0,%1";
	  else
	    ret = "dla\t%0,%a1";
	}
      else if (code1 == SYMBOL_REF || code1 == CONST)
	{
	  if (TARGET_MIPS16
	      && code1 == CONST
	      && GET_CODE (XEXP (op1, 0)) == REG
	      && REGNO (XEXP (op1, 0)) == GP_REG_FIRST + 28)
	    {
	      /* This case arises on the mips16; see
                 mips16_gp_pseudo_reg.  */
	      ret = "move\t%0,%+";
	    }
	  else if (TARGET_MIPS16
		   && code1 == SYMBOL_REF
		   && SYMBOL_REF_FLAG (op1)
		   && (XSTR (op1, 0)[0] != '*'
		       || strncmp (XSTR (op1, 0) + 1,
				   LOCAL_LABEL_PREFIX,
				   sizeof LOCAL_LABEL_PREFIX - 1) != 0))
	    {
	      /* This can occur when reloading the address of a GP
                 relative symbol on the mips16.  */
	      ret = "move\t%0,%+\n\taddu\t%0,%%gprel(%a1)";
	    }
	  else
	    {
	      if (TARGET_STATS)
		mips_count_memory_refs (op1, 2);

	      if (GET_CODE (operands[1]) == SIGN_EXTEND)
		/* We deliberately remove the 'a' from '%1', so that we don't
		   have to add SIGN_EXTEND support to print_operand_address.
		   print_operand will just call print_operand_address in this
		   case, so there is no problem.  */
		ret = "la\t%0,%1";
	      else
		ret = "dla\t%0,%a1";
	    }
	}
    }

  else if (code0 == MEM)
    {
      if (code1 == REG)
	{
	  int regno1 = REGNO (op1) + subreg_offset1;

	  if (FP_REG_P (regno1))
	    ret = "s.d\t%1,%0";

	  else if (ALL_COP_REG_P (regno1) && TARGET_64BIT)
	    {
	      static char retval[] = "sdc_\t%1,%0";

	      retval[3] = COPNUM_AS_CHAR_FROM_REGNUM (regno1);
	      ret = retval;
	    }
	  else if (TARGET_64BIT)
	    {

#ifdef TARGET_FP_CALL_32
	      if (FP_CALL_GP_REG_P (regno1))
		ret = "dsll\t%1,32\n\tor\t%1,%D1\n\tsd\t%1,%0";
	      else
#endif
		ret = "sd\t%1,%0";
	    }

	  else if (double_memory_operand (op0, GET_MODE (op0)))
	    ret = "sw\t%1,%0\n\tsw\t%D1,%D0";
	}

      else if (((code1 == CONST_INT && INTVAL (op1) == 0)
		|| (code1 == CONST_DOUBLE
		    && op1 == CONST0_RTX (GET_MODE (op1))))
	       && (TARGET_64BIT
		   || double_memory_operand (op0, GET_MODE (op0))))
	{
	  if (TARGET_64BIT)
	    ret = "sd\t%.,%0";
	  else
	    ret = "sw\t%.,%0\n\tsw\t%.,%D0";
	}

      if (TARGET_STATS)
	mips_count_memory_refs (op0, 2);

      if (ret != 0 && MEM_VOLATILE_P (op0))
	{
	  size_t i = strlen (ret);

	  if (i > sizeof (volatile_buffer) - sizeof ("%{%}"))
	    abort ();

	  sprintf (volatile_buffer, "%%{%s%%}", ret);
	  ret = volatile_buffer;
	}
    }

  if (ret == 0)
    {
      abort_with_insn (insn, "bad move");
      return 0;
    }

  if (delay != DELAY_NONE)
    return mips_fill_delay_slot (ret, delay, operands, insn);

  return ret;
}

/* Provide the costs of an addressing mode that contains ADDR.
   If ADDR is not a valid address, its cost is irrelevant.  */

int
mips_address_cost (addr)
     rtx addr;
{
  switch (GET_CODE (addr))
    {
    case LO_SUM:
      return 1;

    case LABEL_REF:
      return 2;

    case CONST:
      {
	rtx offset = const0_rtx;
	addr = eliminate_constant_term (XEXP (addr, 0), &offset);
	if (GET_CODE (addr) == LABEL_REF)
	  return 2;

	if (GET_CODE (addr) != SYMBOL_REF)
	  return 4;

	if (! SMALL_INT (offset))
	  return 2;
      }

      /* ... fall through ...  */

    case SYMBOL_REF:
      return SYMBOL_REF_FLAG (addr) ? 1 : 2;

    case PLUS:
      {
	register rtx plus0 = XEXP (addr, 0);
	register rtx plus1 = XEXP (addr, 1);

	if (GET_CODE (plus0) != REG && GET_CODE (plus1) == REG)
	  plus0 = XEXP (addr, 1), plus1 = XEXP (addr, 0);

	if (GET_CODE (plus0) != REG)
	  break;

	switch (GET_CODE (plus1))
	  {
	  case CONST_INT:
	    return SMALL_INT (plus1) ? 1 : 2;

	  case CONST:
	  case SYMBOL_REF:
	  case LABEL_REF:
	  case HIGH:
	  case LO_SUM:
	    return mips_address_cost (plus1) + 1;

	  default:
	    break;
	  }
      }

    default:
      break;
    }

  return 4;
}

/* Return nonzero if X is an address which needs a temporary register when
   reloaded while generating PIC code.  */

int
pic_address_needs_scratch (x)
     rtx x;
{
  /* An address which is a symbolic plus a non SMALL_INT needs a temp reg.  */
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && ! SMALL_INT (XEXP (XEXP (x, 0), 1)))
    return 1;

  return 0;
}

/* Make normal rtx_code into something we can index from an array */

static enum internal_test
map_test_to_internal_test (test_code)
     enum rtx_code test_code;
{
  enum internal_test test = ITEST_MAX;

  switch (test_code)
    {
    case EQ:  test = ITEST_EQ;  break;
    case NE:  test = ITEST_NE;  break;
    case GT:  test = ITEST_GT;  break;
    case GE:  test = ITEST_GE;  break;
    case LT:  test = ITEST_LT;  break;
    case LE:  test = ITEST_LE;  break;
    case GTU: test = ITEST_GTU; break;
    case GEU: test = ITEST_GEU; break;
    case LTU: test = ITEST_LTU; break;
    case LEU: test = ITEST_LEU; break;
    default:			break;
    }

  return test;
}


/* Generate the code to compare two integer values.  The return value is:
   (reg:SI xx)		The pseudo register the comparison is in
   0		       	No register, generate a simple branch.

   ??? This is called with result nonzero by the Scond patterns in
   mips.md.  These patterns are called with a target in the mode of
   the Scond instruction pattern.  Since this must be a constant, we
   must use SImode.  This means that if RESULT is nonzero, it will
   always be an SImode register, even if TARGET_64BIT is true.  We
   cope with this by calling convert_move rather than emit_move_insn.
   This will sometimes lead to an unnecessary extension of the result;
   for example:

   long long
   foo (long long i)
   {
     return i < 5;
   }

   */

rtx
gen_int_relational (test_code, result, cmp0, cmp1, p_invert)
     enum rtx_code test_code;	/* relational test (EQ, etc) */
     rtx result;		/* result to store comp. or 0 if branch */
     rtx cmp0;			/* first operand to compare */
     rtx cmp1;			/* second operand to compare */
     int *p_invert;		/* NULL or ptr to hold whether branch needs */
				/* to reverse its test */
{
  struct cmp_info
  {
    enum rtx_code test_code;	/* code to use in instruction (LT vs. LTU) */
    int const_low;		/* low bound of constant we can accept */
    int const_high;		/* high bound of constant we can accept */
    int const_add;		/* constant to add (convert LE -> LT) */
    int reverse_regs;		/* reverse registers in test */
    int invert_const;		/* != 0 if invert value if cmp1 is constant */
    int invert_reg;		/* != 0 if invert value if cmp1 is register */
    int unsignedp;		/* != 0 for unsigned comparisons.  */
  };

  static const struct cmp_info info[ (int)ITEST_MAX ] = {

    { XOR,	 0,  65535,  0,	 0,  0,	 0, 0 },	/* EQ  */
    { XOR,	 0,  65535,  0,	 0,  1,	 1, 0 },	/* NE  */
    { LT,   -32769,  32766,  1,	 1,  1,	 0, 0 },	/* GT  */
    { LT,   -32768,  32767,  0,	 0,  1,	 1, 0 },	/* GE  */
    { LT,   -32768,  32767,  0,	 0,  0,	 0, 0 },	/* LT  */
    { LT,   -32769,  32766,  1,	 1,  0,	 1, 0 },	/* LE  */
    { LTU,  -32769,  32766,  1,	 1,  1,	 0, 1 },	/* GTU */
    { LTU,  -32768,  32767,  0,	 0,  1,	 1, 1 },	/* GEU */
    { LTU,  -32768,  32767,  0,	 0,  0,	 0, 1 },	/* LTU */
    { LTU,  -32769,  32766,  1,	 1,  0,	 1, 1 },	/* LEU */
  };

  enum internal_test test;
  enum machine_mode mode;
  const struct cmp_info *p_info;
  int branch_p;
  int eqne_p;
  int invert;
  rtx reg;
  rtx reg2;

  test = map_test_to_internal_test (test_code);
  if (test == ITEST_MAX)
    abort ();

  p_info = &info[(int) test];
  eqne_p = (p_info->test_code == XOR);

  mode = GET_MODE (cmp0);
  if (mode == VOIDmode)
    mode = GET_MODE (cmp1);

  /* Eliminate simple branches */
  branch_p = (result == 0);
  if (branch_p)
    {
      if (GET_CODE (cmp0) == REG || GET_CODE (cmp0) == SUBREG)
	{
	  /* Comparisons against zero are simple branches */
	  if (GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) == 0
	      && (! TARGET_MIPS16 || eqne_p))
	    return 0;

	  /* Test for beq/bne.  */
	  if (eqne_p && ! TARGET_MIPS16)
	    return 0;
	}

      /* allocate a pseudo to calculate the value in.  */
      result = gen_reg_rtx (mode);
    }

  /* Make sure we can handle any constants given to us.  */
  if (GET_CODE (cmp0) == CONST_INT)
    cmp0 = force_reg (mode, cmp0);

  if (GET_CODE (cmp1) == CONST_INT)
    {
      HOST_WIDE_INT value = INTVAL (cmp1);

      if (value < p_info->const_low
	  || value > p_info->const_high
	  /* ??? Why?  And why wasn't the similar code below modified too?  */
	  || (TARGET_64BIT
	      && HOST_BITS_PER_WIDE_INT < 64
	      && p_info->const_add != 0
	      && ((p_info->unsignedp
		   ? ((unsigned HOST_WIDE_INT) (value + p_info->const_add)
		      > (unsigned HOST_WIDE_INT) INTVAL (cmp1))
		   : (value + p_info->const_add) > INTVAL (cmp1))
		  != (p_info->const_add > 0))))
	cmp1 = force_reg (mode, cmp1);
    }

  /* See if we need to invert the result.  */
  invert = (GET_CODE (cmp1) == CONST_INT
	    ? p_info->invert_const : p_info->invert_reg);

  if (p_invert != (int *)0)
    {
      *p_invert = invert;
      invert = 0;
    }

  /* Comparison to constants, may involve adding 1 to change a LT into LE.
     Comparison between two registers, may involve switching operands.  */
  if (GET_CODE (cmp1) == CONST_INT)
    {
      if (p_info->const_add != 0)
	{
	  HOST_WIDE_INT new = INTVAL (cmp1) + p_info->const_add;

	  /* If modification of cmp1 caused overflow,
	     we would get the wrong answer if we follow the usual path;
	     thus, x > 0xffffffffU would turn into x > 0U.  */
	  if ((p_info->unsignedp
	       ? (unsigned HOST_WIDE_INT) new >
	       (unsigned HOST_WIDE_INT) INTVAL (cmp1)
	       : new > INTVAL (cmp1))
	      != (p_info->const_add > 0))
	    {
	      /* This test is always true, but if INVERT is true then
		 the result of the test needs to be inverted so 0 should
		 be returned instead.  */
	      emit_move_insn (result, invert ? const0_rtx : const_true_rtx);
	      return result;
	    }
	  else
	    cmp1 = GEN_INT (new);
	}
    }

  else if (p_info->reverse_regs)
    {
      rtx temp = cmp0;
      cmp0 = cmp1;
      cmp1 = temp;
    }

  if (test == ITEST_NE && GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) == 0)
    reg = cmp0;
  else
    {
      reg = (invert || eqne_p) ? gen_reg_rtx (mode) : result;
      convert_move (reg, gen_rtx (p_info->test_code, mode, cmp0, cmp1), 0);
    }

  if (test == ITEST_NE)
    {
      if (! TARGET_MIPS16)
	{
	  convert_move (result, gen_rtx (GTU, mode, reg, const0_rtx), 0);
	  if (p_invert != NULL)
	    *p_invert = 0;
	  invert = 0;
	}
      else
	{
	  reg2 = invert ? gen_reg_rtx (mode) : result;
	  convert_move (reg2, gen_rtx (LTU, mode, reg, const1_rtx), 0);
	  reg = reg2;
	}
    }

  else if (test == ITEST_EQ)
    {
      reg2 = invert ? gen_reg_rtx (mode) : result;
      convert_move (reg2, gen_rtx_LTU (mode, reg, const1_rtx), 0);
      reg = reg2;
    }

  if (invert)
    {
      rtx one;

      if (! TARGET_MIPS16)
	one = const1_rtx;
      else
	{
	  /* The value is in $24.  Copy it to another register, so
             that reload doesn't think it needs to store the $24 and
             the input to the XOR in the same location.  */
	  reg2 = gen_reg_rtx (mode);
	  emit_move_insn (reg2, reg);
	  reg = reg2;
	  one = force_reg (mode, const1_rtx);
	}
      convert_move (result, gen_rtx (XOR, mode, reg, one), 0);
    }

  return result;
}

/* Work out how to check a floating-point condition.  We need a
   separate comparison instruction (C.cond.fmt), followed by a
   branch or conditional move.  Given that IN_CODE is the
   required condition, set *CMP_CODE to the C.cond.fmt code
   and *action_code to the branch or move code.  */

static void
get_float_compare_codes (in_code, cmp_code, action_code)
     enum rtx_code in_code, *cmp_code, *action_code;
{
  switch (in_code)
    {
    case NE:
    case UNGE:
    case UNGT:
    case LTGT:
    case ORDERED:
      *cmp_code = reverse_condition_maybe_unordered (in_code);
      *action_code = EQ;
      break;

    default:
      *cmp_code = in_code;
      *action_code = NE;
      break;
    }
}

/* Emit the common code for doing conditional branches.
   operand[0] is the label to jump to.
   The comparison operands are saved away by cmp{si,di,sf,df}.  */

void
gen_conditional_branch (operands, test_code)
     rtx operands[];
     enum rtx_code test_code;
{
  enum cmp_type type = branch_type;
  rtx cmp0 = branch_cmp[0];
  rtx cmp1 = branch_cmp[1];
  enum machine_mode mode;
  enum rtx_code cmp_code;
  rtx reg;
  int invert;
  rtx label1, label2;

  switch (type)
    {
    case CMP_SI:
    case CMP_DI:
      mode = type == CMP_SI ? SImode : DImode;
      invert = 0;
      reg = gen_int_relational (test_code, NULL_RTX, cmp0, cmp1, &invert);

      if (reg)
	{
	  cmp0 = reg;
	  cmp1 = const0_rtx;
	  test_code = NE;
	}
      else if (GET_CODE (cmp1) == CONST_INT && INTVAL (cmp1) != 0)
	/* We don't want to build a comparison against a nonzero
	   constant.  */
	cmp1 = force_reg (mode, cmp1);

      break;

    case CMP_SF:
    case CMP_DF:
      if (! ISA_HAS_8CC)
	reg = gen_rtx_REG (CCmode, FPSW_REGNUM);
      else
	reg = gen_reg_rtx (CCmode);

      get_float_compare_codes (test_code, &cmp_code, &test_code);
      emit_insn (gen_rtx_SET (VOIDmode, reg,
			      gen_rtx (cmp_code, CCmode, cmp0, cmp1)));

      mode = CCmode;
      cmp0 = reg;
      cmp1 = const0_rtx;
      invert = 0;
      break;

    default:
      abort_with_insn (gen_rtx (test_code, VOIDmode, cmp0, cmp1), "bad test");
    }

  /* Generate the branch.  */

  label1 = gen_rtx_LABEL_REF (VOIDmode, operands[0]);
  label2 = pc_rtx;

  if (invert)
    {
      label2 = label1;
      label1 = pc_rtx;
    }

  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode,
						     gen_rtx (test_code, mode,
							      cmp0, cmp1),
						     label1, label2)));
}

/* Emit the common code for conditional moves.  OPERANDS is the array
   of operands passed to the conditional move defined_expand.  */

void
gen_conditional_move (operands)
     rtx *operands;
{
  rtx op0 = branch_cmp[0];
  rtx op1 = branch_cmp[1];
  enum machine_mode mode = GET_MODE (branch_cmp[0]);
  enum rtx_code cmp_code = GET_CODE (operands[1]);
  enum rtx_code move_code = NE;
  enum machine_mode op_mode = GET_MODE (operands[0]);
  enum machine_mode cmp_mode;
  rtx cmp_reg;

  if (GET_MODE_CLASS (mode) != MODE_FLOAT)
    {
      switch (cmp_code)
	{
	case EQ:
	  cmp_code = XOR;
	  move_code = EQ;
	  break;
	case NE:
	  cmp_code = XOR;
	  break;
	case LT:
	  break;
	case GE:
	  cmp_code = LT;
	  move_code = EQ;
	  break;
	case GT:
	  cmp_code = LT;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  break;
	case LE:
	  cmp_code = LT;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  move_code = EQ;
	  break;
	case LTU:
	  break;
	case GEU:
	  cmp_code = LTU;
	  move_code = EQ;
	  break;
	case GTU:
	  cmp_code = LTU;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  break;
	case LEU:
	  cmp_code = LTU;
	  op0 = force_reg (mode, branch_cmp[1]);
	  op1 = branch_cmp[0];
	  move_code = EQ;
	  break;
	default:
	  abort ();
	}
    }
  else
    get_float_compare_codes (cmp_code, &cmp_code, &move_code);

  if (mode == SImode || mode == DImode)
    cmp_mode = mode;
  else if (mode == SFmode || mode == DFmode)
    cmp_mode = CCmode;
  else
    abort ();

  cmp_reg = gen_reg_rtx (cmp_mode);
  emit_insn (gen_rtx_SET (cmp_mode, cmp_reg,
			  gen_rtx (cmp_code, cmp_mode, op0, op1)));

  emit_insn (gen_rtx_SET (op_mode, operands[0],
			  gen_rtx_IF_THEN_ELSE (op_mode,
						gen_rtx (move_code, VOIDmode,
							 cmp_reg,
							 CONST0_RTX (SImode)),
						operands[2], operands[3])));
}

/* Emit the common code for conditional moves.  OPERANDS is the array
   of operands passed to the conditional move defined_expand.  */

void
mips_gen_conditional_trap (operands)
     rtx operands[];
{
  rtx op0, op1;
  enum rtx_code cmp_code = GET_CODE (operands[0]);
  enum machine_mode mode = GET_MODE (branch_cmp[0]);

  /* MIPS conditional trap machine instructions don't have GT or LE
     flavors, so we must invert the comparison and convert to LT and
     GE, respectively.  */
  switch (cmp_code)
    {
    case GT: cmp_code = LT; break;
    case LE: cmp_code = GE; break;
    case GTU: cmp_code = LTU; break;
    case LEU: cmp_code = GEU; break;
    default: break;
    }
  if (cmp_code == GET_CODE (operands[0]))
    {
      op0 = force_reg (mode, branch_cmp[0]);
      op1 = branch_cmp[1];
    }
  else
    {
      op0 = force_reg (mode, branch_cmp[1]);
      op1 = branch_cmp[0];
    }
  if (GET_CODE (op1) == CONST_INT && ! SMALL_INT (op1))
    op1 = force_reg (mode, op1);

  emit_insn (gen_rtx_TRAP_IF (VOIDmode,
			      gen_rtx (cmp_code, GET_MODE (operands[0]), op0, op1),
			      operands[1]));
}

/* Return true if operand OP is a condition code register.
   Only for use during or after reload.  */

int
fcc_register_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return ((mode == VOIDmode || mode == GET_MODE (op))
	  && (reload_in_progress || reload_completed)
	  && (GET_CODE (op) == REG || GET_CODE (op) == SUBREG)
	  && ST_REG_P (true_regnum (op)));
}

/* Emit code to move general operand SRC into condition-code
   register DEST.  SCRATCH is a scratch TFmode float register.
   The sequence is:

	FP1 = SRC
	FP2 = 0.0f
	DEST = FP2 < FP1

   where FP1 and FP2 are single-precision float registers
   taken from SCRATCH.  */

void
mips_emit_fcc_reload (dest, src, scratch)
     rtx dest, src, scratch;
{
  rtx fp1, fp2;

  /* Change the source to SFmode.  */
  if (GET_CODE (src) == MEM)
    src = adjust_address (src, SFmode, 0);
  else if (GET_CODE (src) == REG || GET_CODE (src) == SUBREG)
    src = gen_rtx_REG (SFmode, true_regnum (src));

  fp1 = gen_rtx_REG (SFmode, REGNO (scratch));
  fp2 = gen_rtx_REG (SFmode, REGNO (scratch) + FP_INC);

  emit_move_insn (copy_rtx (fp1), src);
  emit_move_insn (copy_rtx (fp2), CONST0_RTX (SFmode));
  emit_insn (gen_slt_sf (dest, fp2, fp1));
}

/* Emit code to change the current function's return address to
   ADDRESS.  SCRATCH is available as a scratch register, if needed.
   ADDRESS and SCRATCH are both word-mode GPRs.  */

void
mips_set_return_address (address, scratch)
     rtx address, scratch;
{
  HOST_WIDE_INT gp_offset;

  compute_frame_size (get_frame_size ());
  if (((cfun->machine->frame.mask >> 31) & 1) == 0)
    abort ();
  gp_offset = cfun->machine->frame.gp_sp_offset;

  /* Reduce SP + GP_OFSET to a legitimate address and put it in SCRATCH.  */
  if (gp_offset < 32768)
    scratch = plus_constant (stack_pointer_rtx, gp_offset);
  else
    {
      emit_move_insn (scratch, GEN_INT (gp_offset));
      if (Pmode == DImode)
	emit_insn (gen_adddi3 (scratch, scratch, stack_pointer_rtx));
      else
	emit_insn (gen_addsi3 (scratch, scratch, stack_pointer_rtx));
    }

  emit_move_insn (gen_rtx_MEM (GET_MODE (address), scratch), address);
}

/* Write a loop to move a constant number of bytes.
   Generate load/stores as follows:

   do {
     temp1 = src[0];
     temp2 = src[1];
     ...
     temp<last> = src[MAX_MOVE_REGS-1];
     dest[0] = temp1;
     dest[1] = temp2;
     ...
     dest[MAX_MOVE_REGS-1] = temp<last>;
     src += MAX_MOVE_REGS;
     dest += MAX_MOVE_REGS;
   } while (src != final);

   This way, no NOP's are needed, and only MAX_MOVE_REGS+3 temp
   registers are needed.

   Aligned moves move MAX_MOVE_REGS*4 bytes every (2*MAX_MOVE_REGS)+3
   cycles, unaligned moves move MAX_MOVE_REGS*4 bytes every
   (4*MAX_MOVE_REGS)+3 cycles, assuming no cache misses.  */

#define MAX_MOVE_REGS 4
#define MAX_MOVE_BYTES (MAX_MOVE_REGS * UNITS_PER_WORD)

static void
block_move_loop (dest_reg, src_reg, bytes, align, orig_dest, orig_src)
     rtx dest_reg;		/* register holding destination address */
     rtx src_reg;		/* register holding source address */
     unsigned int bytes;	/* # bytes to move */
     int align;			/* alignment */
     rtx orig_dest;		/* original dest */
     rtx orig_src;		/* original source for making a reg note */
{
  rtx dest_mem = replace_equiv_address (orig_dest, dest_reg);
  rtx src_mem = replace_equiv_address (orig_src, src_reg);
  rtx align_rtx = GEN_INT (align);
  rtx label;
  rtx final_src;
  rtx bytes_rtx;
  int leftover;

  if (bytes < (unsigned)2 * MAX_MOVE_BYTES)
    abort ();

  leftover = bytes % MAX_MOVE_BYTES;
  bytes -= leftover;

  label = gen_label_rtx ();
  final_src = gen_reg_rtx (Pmode);
  bytes_rtx = GEN_INT (bytes);

  if (bytes > 0x7fff)
    {
      if (Pmode == DImode)
	{
	  emit_insn (gen_movdi (final_src, bytes_rtx));
	  emit_insn (gen_adddi3 (final_src, final_src, src_reg));
	}
      else
	{
	  emit_insn (gen_movsi (final_src, bytes_rtx));
	  emit_insn (gen_addsi3 (final_src, final_src, src_reg));
	}
    }
  else
    {
      if (Pmode == DImode)
	emit_insn (gen_adddi3 (final_src, src_reg, bytes_rtx));
      else
	emit_insn (gen_addsi3 (final_src, src_reg, bytes_rtx));
    }

  emit_label (label);

  bytes_rtx = GEN_INT (MAX_MOVE_BYTES);
  emit_insn (gen_movstrsi_internal (dest_mem, src_mem, bytes_rtx, align_rtx));

  if (Pmode == DImode)
    {
      emit_insn (gen_adddi3 (src_reg, src_reg, bytes_rtx));
      emit_insn (gen_adddi3 (dest_reg, dest_reg, bytes_rtx));
      emit_insn (gen_cmpdi (src_reg, final_src));
    }
  else
    {
      emit_insn (gen_addsi3 (src_reg, src_reg, bytes_rtx));
      emit_insn (gen_addsi3 (dest_reg, dest_reg, bytes_rtx));
      emit_insn (gen_cmpsi (src_reg, final_src));
    }

  emit_jump_insn (gen_bne (label));

  if (leftover)
    emit_insn (gen_movstrsi_internal (dest_mem, src_mem, GEN_INT (leftover),
				      align_rtx));
}

/* Use a library function to move some bytes.  */

static void
block_move_call (dest_reg, src_reg, bytes_rtx)
     rtx dest_reg;
     rtx src_reg;
     rtx bytes_rtx;
{
  /* We want to pass the size as Pmode, which will normally be SImode
     but will be DImode if we are using 64 bit longs and pointers.  */
  if (GET_MODE (bytes_rtx) != VOIDmode
      && GET_MODE (bytes_rtx) != (unsigned) Pmode)
    bytes_rtx = convert_to_mode (Pmode, bytes_rtx, 1);

#ifdef TARGET_MEM_FUNCTIONS
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "memcpy"), 0,
		     VOIDmode, 3, dest_reg, Pmode, src_reg, Pmode,
		     convert_to_mode (TYPE_MODE (sizetype), bytes_rtx,
				      TREE_UNSIGNED (sizetype)),
		     TYPE_MODE (sizetype));
#else
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "bcopy"), 0,
		     VOIDmode, 3, src_reg, Pmode, dest_reg, Pmode,
		     convert_to_mode (TYPE_MODE (integer_type_node), bytes_rtx,
				      TREE_UNSIGNED (integer_type_node)),
		     TYPE_MODE (integer_type_node));
#endif
}

/* Expand string/block move operations.

   operands[0] is the pointer to the destination.
   operands[1] is the pointer to the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.  */

void
expand_block_move (operands)
     rtx operands[];
{
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp = GET_CODE (bytes_rtx) == CONST_INT;
  unsigned HOST_WIDE_INT bytes = constp ? INTVAL (bytes_rtx) : 0;
  unsigned int align = INTVAL (align_rtx);
  rtx orig_src	= operands[1];
  rtx orig_dest	= operands[0];
  rtx src_reg;
  rtx dest_reg;

  if (constp && bytes == 0)
    return;

  if (align > (unsigned) UNITS_PER_WORD)
    align = UNITS_PER_WORD;

  /* Move the address into scratch registers.  */
  dest_reg = copy_addr_to_reg (XEXP (orig_dest, 0));
  src_reg  = copy_addr_to_reg (XEXP (orig_src, 0));

  if (TARGET_MEMCPY)
    block_move_call (dest_reg, src_reg, bytes_rtx);

  else if (constp && bytes <= (unsigned)2 * MAX_MOVE_BYTES
	   && align == (unsigned) UNITS_PER_WORD)
    move_by_pieces (orig_dest, orig_src, bytes, align * BITS_PER_WORD);

  else if (constp && bytes <= (unsigned)2 * MAX_MOVE_BYTES)
    emit_insn (gen_movstrsi_internal (replace_equiv_address (orig_dest,
							     dest_reg),
				      replace_equiv_address (orig_src,
							     src_reg),
				      bytes_rtx, align_rtx));

  else if (constp && align >= (unsigned) UNITS_PER_WORD && optimize)
    block_move_loop (dest_reg, src_reg, bytes, align, orig_dest, orig_src);

  else if (constp && optimize)
    {
      /* If the alignment is not word aligned, generate a test at
	 runtime, to see whether things wound up aligned, and we
	 can use the faster lw/sw instead ulw/usw.  */

      rtx temp = gen_reg_rtx (Pmode);
      rtx aligned_label = gen_label_rtx ();
      rtx join_label = gen_label_rtx ();
      int leftover = bytes % MAX_MOVE_BYTES;

      bytes -= leftover;

      if (Pmode == DImode)
	{
	  emit_insn (gen_iordi3 (temp, src_reg, dest_reg));
	  emit_insn (gen_anddi3 (temp, temp, GEN_INT (UNITS_PER_WORD - 1)));
	  emit_insn (gen_cmpdi (temp, const0_rtx));
	}
      else
	{
	  emit_insn (gen_iorsi3 (temp, src_reg, dest_reg));
	  emit_insn (gen_andsi3 (temp, temp, GEN_INT (UNITS_PER_WORD - 1)));
	  emit_insn (gen_cmpsi (temp, const0_rtx));
	}

      emit_jump_insn (gen_beq (aligned_label));

      /* Unaligned loop.  */
      block_move_loop (dest_reg, src_reg, bytes, 1, orig_dest, orig_src);
      emit_jump_insn (gen_jump (join_label));
      emit_barrier ();

      /* Aligned loop.  */
      emit_label (aligned_label);
      block_move_loop (dest_reg, src_reg, bytes, UNITS_PER_WORD, orig_dest,
		       orig_src);
      emit_label (join_label);

      /* Bytes at the end of the loop.  */
      if (leftover)
	emit_insn (gen_movstrsi_internal (replace_equiv_address (orig_dest,
								 dest_reg),
					  replace_equiv_address (orig_src,
								 src_reg),
					  GEN_INT (leftover),
					  GEN_INT (align)));
    }

  else
    block_move_call (dest_reg, src_reg, bytes_rtx);
}

/* Emit load/stores for a small constant block_move.

   operands[0] is the memory address of the destination.
   operands[1] is the memory address of the source.
   operands[2] is the number of bytes to move.
   operands[3] is the alignment.
   operands[4] is a temp register.
   operands[5] is a temp register.
   ...
   operands[3+num_regs] is the last temp register.

   The block move type can be one of the following:
	BLOCK_MOVE_NORMAL	Do all of the block move.
	BLOCK_MOVE_NOT_LAST	Do all but the last store.
	BLOCK_MOVE_LAST		Do just the last store.  */

const char *
output_block_move (insn, operands, num_regs, move_type)
     rtx insn;
     rtx operands[];
     int num_regs;
     enum block_move_type move_type;
{
  rtx dest_reg = XEXP (operands[0], 0);
  rtx src_reg = XEXP (operands[1], 0);
  HOST_WIDE_INT bytes = INTVAL (operands[2]);
  int align = INTVAL (operands[3]);
  int num = 0;
  int offset = 0;
  int use_lwl_lwr = 0;
  int last_operand = num_regs + 4;
  int safe_regs = 4;
  int i;
  rtx xoperands[10];

  struct {
    const char *load;		/* load insn without nop */
    const char *load_nop;	/* load insn with trailing nop */
    const char *store;		/* store insn */
    const char *final;		/* if last_store used: NULL or swr */
    const char *last_store;	/* last store instruction */
    int offset;			/* current offset */
    enum machine_mode mode;	/* mode to use on (MEM) */
  } load_store[4];

  /* ??? Detect a bug in GCC, where it can give us a register
     the same as one of the addressing registers and reduce
     the number of registers available.  */
  for (i = 4; i < last_operand && safe_regs < (int) ARRAY_SIZE (xoperands); i++)
    if (! reg_mentioned_p (operands[i], operands[0])
	&& ! reg_mentioned_p (operands[i], operands[1]))
      xoperands[safe_regs++] = operands[i];

  if (safe_regs < last_operand)
    {
      xoperands[0] = operands[0];
      xoperands[1] = operands[1];
      xoperands[2] = operands[2];
      xoperands[3] = operands[3];
      return output_block_move (insn, xoperands, safe_regs - 4, move_type);
    }

  /* If we are given global or static addresses, and we would be
     emitting a few instructions, try to save time by using a
     temporary register for the pointer.  */
  /* ??? The SGI Irix6 assembler fails when a SYMBOL_REF is used in
     an ldl/ldr instruction pair.  We play it safe, and always move
     constant addresses into registers when generating N32/N64 code, just
     in case we might emit an unaligned load instruction.  */
  if (num_regs > 2 && (bytes > 2 * align || move_type != BLOCK_MOVE_NORMAL
		       || mips_abi == ABI_MEABI
		       || mips_abi == ABI_N32
		       || mips_abi == ABI_64))
    {
      if (CONSTANT_P (src_reg))
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (operands[1], 1);

	  src_reg = operands[3 + num_regs--];
	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      xoperands[1] = operands[1];
	      xoperands[0] = src_reg;
	      if (Pmode == DImode)
		output_asm_insn ("dla\t%0,%1", xoperands);
	      else
		output_asm_insn ("la\t%0,%1", xoperands);
	    }
	}

      if (CONSTANT_P (dest_reg))
	{
	  if (TARGET_STATS)
	    mips_count_memory_refs (operands[0], 1);

	  dest_reg = operands[3 + num_regs--];
	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      xoperands[1] = operands[0];
	      xoperands[0] = dest_reg;
	      if (Pmode == DImode)
		output_asm_insn ("dla\t%0,%1", xoperands);
	      else
		output_asm_insn ("la\t%0,%1", xoperands);
	    }
	}
    }

  /* ??? We really shouldn't get any LO_SUM addresses here, because they
     are not offsettable, however, offsettable_address_p says they are
     offsettable. I think this is a bug in offsettable_address_p.
     For expediency, we fix this by just loading the address into a register
     if we happen to get one.  */

  if (GET_CODE (src_reg) == LO_SUM)
    {
      src_reg = operands[3 + num_regs--];
      if (move_type != BLOCK_MOVE_LAST)
	{
	  xoperands[2] = XEXP (XEXP (operands[1], 0), 1);
	  xoperands[1] = XEXP (XEXP (operands[1], 0), 0);
	  xoperands[0] = src_reg;
	  if (Pmode == DImode)
	    output_asm_insn ("daddiu\t%0,%1,%%lo(%2)", xoperands);
	  else
	    output_asm_insn ("addiu\t%0,%1,%%lo(%2)", xoperands);
	}
    }

  if (GET_CODE (dest_reg) == LO_SUM)
    {
      dest_reg = operands[3 + num_regs--];
      if (move_type != BLOCK_MOVE_LAST)
	{
	  xoperands[2] = XEXP (XEXP (operands[0], 0), 1);
	  xoperands[1] = XEXP (XEXP (operands[0], 0), 0);
	  xoperands[0] = dest_reg;
	  if (Pmode == DImode)
	    output_asm_insn ("daddiu\t%0,%1,%%lo(%2)", xoperands);
	  else
	    output_asm_insn ("addiu\t%0,%1,%%lo(%2)", xoperands);
	}
    }

  if (num_regs > (int) ARRAY_SIZE (load_store))
    num_regs = ARRAY_SIZE (load_store);

  else if (num_regs < 1)
    abort_with_insn (insn,
		     "cannot do block move, not enough scratch registers");

  while (bytes > 0)
    {
      load_store[num].offset = offset;

      if (TARGET_64BIT && bytes >= 8 && align >= 8)
	{
	  load_store[num].load = "ld\t%0,%1";
	  load_store[num].load_nop = "ld\t%0,%1%#";
	  load_store[num].store = "sd\t%0,%1";
	  load_store[num].last_store = "sd\t%0,%1";
	  load_store[num].final = 0;
	  load_store[num].mode = DImode;
	  offset += 8;
	  bytes -= 8;
	}

      /* ??? Fails because of a MIPS assembler bug?  */
      else if (TARGET_64BIT && bytes >= 8
	       && ! TARGET_SR71K
	       && ! TARGET_MIPS16)
	{
	  if (BYTES_BIG_ENDIAN)
	    {
	      load_store[num].load = "ldl\t%0,%1\n\tldr\t%0,%2";
	      load_store[num].load_nop = "ldl\t%0,%1\n\tldr\t%0,%2%#";
	      load_store[num].store = "sdl\t%0,%1\n\tsdr\t%0,%2";
	      load_store[num].last_store = "sdr\t%0,%2";
	      load_store[num].final = "sdl\t%0,%1";
	    }
	  else
	    {
	      load_store[num].load = "ldl\t%0,%2\n\tldr\t%0,%1";
	      load_store[num].load_nop = "ldl\t%0,%2\n\tldr\t%0,%1%#";
	      load_store[num].store = "sdl\t%0,%2\n\tsdr\t%0,%1";
	      load_store[num].last_store = "sdr\t%0,%1";
	      load_store[num].final = "sdl\t%0,%2";
	    }

	  load_store[num].mode = DImode;
	  offset += 8;
	  bytes -= 8;
	  use_lwl_lwr = 1;
	}

      else if (bytes >= 4 && align >= 4)
	{
	  load_store[num].load = "lw\t%0,%1";
	  load_store[num].load_nop = "lw\t%0,%1%#";
	  load_store[num].store = "sw\t%0,%1";
	  load_store[num].last_store = "sw\t%0,%1";
	  load_store[num].final = 0;
	  load_store[num].mode = SImode;
	  offset += 4;
	  bytes -= 4;
	}

      else if (bytes >= 4
	       && ! TARGET_SR71K
	       && ! TARGET_MIPS16)
	{
	  if (BYTES_BIG_ENDIAN)
	    {
	      load_store[num].load = "lwl\t%0,%1\n\tlwr\t%0,%2";
	      load_store[num].load_nop = "lwl\t%0,%1\n\tlwr\t%0,%2%#";
	      load_store[num].store = "swl\t%0,%1\n\tswr\t%0,%2";
	      load_store[num].last_store = "swr\t%0,%2";
	      load_store[num].final = "swl\t%0,%1";
	    }
	  else
	    {
	      load_store[num].load = "lwl\t%0,%2\n\tlwr\t%0,%1";
	      load_store[num].load_nop = "lwl\t%0,%2\n\tlwr\t%0,%1%#";
	      load_store[num].store = "swl\t%0,%2\n\tswr\t%0,%1";
	      load_store[num].last_store = "swr\t%0,%1";
	      load_store[num].final = "swl\t%0,%2";
	    }

	  load_store[num].mode = SImode;
	  offset += 4;
	  bytes -= 4;
	  use_lwl_lwr = 1;
	}

      else if (bytes >= 2 && align >= 2)
	{
	  load_store[num].load = "lh\t%0,%1";
	  load_store[num].load_nop = "lh\t%0,%1%#";
	  load_store[num].store = "sh\t%0,%1";
	  load_store[num].last_store = "sh\t%0,%1";
	  load_store[num].final = 0;
	  load_store[num].mode = HImode;
	  offset += 2;
	  bytes -= 2;
	}
      else
	{
	  load_store[num].load = "lb\t%0,%1";
	  load_store[num].load_nop = "lb\t%0,%1%#";
	  load_store[num].store = "sb\t%0,%1";
	  load_store[num].last_store = "sb\t%0,%1";
	  load_store[num].final = 0;
	  load_store[num].mode = QImode;
	  offset++;
	  bytes--;
	}

      if (TARGET_STATS && move_type != BLOCK_MOVE_LAST)
	{
	  dslots_load_total++;
	  dslots_load_filled++;

	  if (CONSTANT_P (src_reg))
	    mips_count_memory_refs (src_reg, 1);

	  if (CONSTANT_P (dest_reg))
	    mips_count_memory_refs (dest_reg, 1);
	}

      /* Emit load/stores now if we have run out of registers or are
	 at the end of the move.  */

      if (++num == num_regs || bytes == 0)
	{
	  /* If only load/store, we need a NOP after the load.  */
	  if (num == 1)
	    {
	      load_store[0].load = load_store[0].load_nop;
	      if (TARGET_STATS && move_type != BLOCK_MOVE_LAST)
		dslots_load_filled--;
	    }

	  if (move_type != BLOCK_MOVE_LAST)
	    {
	      for (i = 0; i < num; i++)
		{
		  int offset;

		  if (!operands[i + 4])
		    abort ();

		  if (GET_MODE (operands[i + 4]) != load_store[i].mode)
		    operands[i + 4] = gen_rtx_REG (load_store[i].mode,
						   REGNO (operands[i + 4]));

		  offset = load_store[i].offset;
		  xoperands[0] = operands[i + 4];
		  xoperands[1] = gen_rtx_MEM (load_store[i].mode,
					      plus_constant (src_reg, offset));

		  if (use_lwl_lwr)
		    {
		      int extra_offset
			= GET_MODE_SIZE (load_store[i].mode) - 1;

		      xoperands[2] = gen_rtx_MEM (load_store[i].mode,
						  plus_constant (src_reg,
								 extra_offset
								 + offset));
		    }

		  output_asm_insn (load_store[i].load, xoperands);
		}
	    }

	  for (i = 0; i < num; i++)
	    {
	      int last_p = (i == num-1 && bytes == 0);
	      int offset = load_store[i].offset;

	      xoperands[0] = operands[i + 4];
	      xoperands[1] = gen_rtx_MEM (load_store[i].mode,
					  plus_constant (dest_reg, offset));


	      if (use_lwl_lwr)
		{
		  int extra_offset = GET_MODE_SIZE (load_store[i].mode) - 1;
		  xoperands[2] = gen_rtx_MEM (load_store[i].mode,
					      plus_constant (dest_reg,
							     extra_offset
							     + offset));
		}

	      if (move_type == BLOCK_MOVE_NORMAL)
		output_asm_insn (load_store[i].store, xoperands);

	      else if (move_type == BLOCK_MOVE_NOT_LAST)
		{
		  if (!last_p)
		    output_asm_insn (load_store[i].store, xoperands);

		  else if (load_store[i].final != 0)
		    output_asm_insn (load_store[i].final, xoperands);
		}

	      else if (last_p)
		output_asm_insn (load_store[i].last_store, xoperands);
	    }

	  num = 0;		/* reset load_store */
	  use_lwl_lwr = 0;
	}
    }

  return "";
}

/* Argument support functions.  */

/* Initialize CUMULATIVE_ARGS for a function.  */

void
init_cumulative_args (cum, fntype, libname)
     CUMULATIVE_ARGS *cum;		/* argument info to initialize */
     tree fntype;			/* tree ptr for function decl */
     rtx libname ATTRIBUTE_UNUSED;	/* SYMBOL_REF of library name or 0 */
{
  static CUMULATIVE_ARGS zero_cum;
  tree param, next_param;

  if (TARGET_DEBUG_E_MODE)
    {
      fprintf (stderr,
	       "\ninit_cumulative_args, fntype = 0x%.8lx", (long)fntype);

      if (!fntype)
	fputc ('\n', stderr);

      else
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, ", fntype code = %s, ret code = %s\n",
		   tree_code_name[(int)TREE_CODE (fntype)],
		   tree_code_name[(int)TREE_CODE (ret_type)]);
	}
    }

  *cum = zero_cum;
  cum->prototype = (fntype && TYPE_ARG_TYPES (fntype));

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  The standard MIPS calling sequence
     passes all arguments in the general purpose registers in this case.  */

  for (param = fntype ? TYPE_ARG_TYPES (fntype) : 0;
       param != 0; param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if (next_param == 0 && TREE_VALUE (param) != void_type_node)
	cum->gp_reg_found = 1;
    }
}

static void
mips_arg_info (cum, mode, type, named, info)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named;
     struct mips_arg_info *info;
{
  bool even_reg_p;
  unsigned int num_words, max_regs;

  info->struct_p = (type != 0
		    && (TREE_CODE (type) == RECORD_TYPE
			|| TREE_CODE (type) == UNION_TYPE
			|| TREE_CODE (type) == QUAL_UNION_TYPE));

  /* Decide whether this argument should go in a floating-point register,
     assuming one is free.  Later code checks for availablity.  */

  info->fpr_p = false;
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_SIZE (mode) <= UNITS_PER_FPVALUE)
    {
      switch (mips_abi)
	{
	case ABI_32:
	case ABI_O64:
	  info->fpr_p = (!cum->gp_reg_found && cum->arg_number < 2);
	  break;

	case ABI_EABI:
	  info->fpr_p = true;
	  break;

	case ABI_MEABI:
	  /* The MIPS eabi says only structures containing doubles get
	     passed in a fp register, so force a structure containing
	     a float to be passed in the integer registers.  */
	  info->fpr_p = (named && !(mode == SFmode && info->struct_p));
	  break;

	default:
	  info->fpr_p = named;
	  break;
	}
    }

  /* Now decide whether the argument must go in an even-numbered register.  */

  even_reg_p = false;
  if (info->fpr_p)
    {
      /* Under the O64 ABI, the second float argument goes in $f13 if it
	 is a double, but $f14 if it is a single.  Otherwise, on a
	 32-bit double-float machine, each FP argument must start in a
	 new register pair.  */
      even_reg_p = (GET_MODE_SIZE (mode) > UNITS_PER_HWFPVALUE
		    || (mips_abi == ABI_O64 && mode == SFmode)
		    || FP_INC > 1);
    }
  else if (!TARGET_64BIT || LONG_DOUBLE_TYPE_SIZE == 128)
    {
      if (GET_MODE_CLASS (mode) == MODE_INT
	  || GET_MODE_CLASS (mode) == MODE_FLOAT)
	even_reg_p = (GET_MODE_SIZE (mode) > UNITS_PER_WORD);

      else if (type != NULL_TREE && TYPE_ALIGN (type) > BITS_PER_WORD)
	even_reg_p = true;
    }

  /* Set REG_OFFSET to the register count we're interested in.
     The EABI allocates the floating-point registers separately,
     but the other ABIs allocate them like integer registers.  */
  info->reg_offset = (mips_abi == ABI_EABI && info->fpr_p
		      ? cum->num_fprs
		      : cum->num_gprs);

  if (even_reg_p)
    info->reg_offset += info->reg_offset & 1;

  /* The alignment applied to registers is also applied to stack arguments.  */
  info->stack_offset = cum->stack_words;
  if (even_reg_p)
    info->stack_offset += info->stack_offset & 1;

  if (mode == BLKmode)
    info->num_bytes = int_size_in_bytes (type);
  else
    info->num_bytes = GET_MODE_SIZE (mode);

  num_words = (info->num_bytes + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  max_regs = MAX_ARGS_IN_REGISTERS - info->reg_offset;

  /* Partition the argument between registers and stack.  */
  info->reg_words = MIN (num_words, max_regs);
  info->stack_words = num_words - info->reg_words;
}


/* Advance the argument to the next argument position.  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS *cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* whether or not the argument was named */
{
  struct mips_arg_info info;

  mips_arg_info (cum, mode, type, named, &info);

  /* The following is a hack in order to pass 1 byte structures
     the same way that the MIPS compiler does (namely by passing
     the structure in the high byte or half word of the register).
     This also makes varargs work.  If we have such a structure,
     we save the adjustment RTL, and the call define expands will
     emit them.  For the VOIDmode argument (argument after the
     last real argument), pass back a parallel vector holding each
     of the adjustments.  */

  /* ??? This scheme requires everything smaller than the word size to
     shifted to the left, but when TARGET_64BIT and ! TARGET_INT64,
     that would mean every int needs to be shifted left, which is very
     inefficient.  Let's not carry this compatibility to the 64 bit
     calling convention for now.  */

  if (info.struct_p
      && info.reg_words == 1
      && info.num_bytes < UNITS_PER_WORD
      && !TARGET_64BIT
      && mips_abi != ABI_EABI
      && mips_abi != ABI_MEABI)
    {
      rtx amount = GEN_INT (BITS_PER_WORD - info.num_bytes * BITS_PER_UNIT);
      rtx reg = gen_rtx_REG (word_mode, GP_ARG_FIRST + info.reg_offset);

      if (TARGET_64BIT)
	cum->adjust[cum->num_adjusts++] = PATTERN (gen_ashldi3 (reg, reg, amount));
      else
	cum->adjust[cum->num_adjusts++] = PATTERN (gen_ashlsi3 (reg, reg, amount));
    }

  if (!info.fpr_p)
    cum->gp_reg_found = true;

  /* See the comment above the cumulative args structure in mips.h
     for an explanation of what this code does.  It assumes the O32
     ABI, which passes at most 2 arguments in float registers.  */
  if (cum->arg_number < 2 && info.fpr_p)
    cum->fp_code += (mode == SFmode ? 1 : 2) << ((cum->arg_number - 1) * 2);

  if (mips_abi != ABI_EABI || !info.fpr_p)
    cum->num_gprs = info.reg_offset + info.reg_words;
  else if (info.reg_words > 0)
    cum->num_fprs += FP_INC;

  if (info.stack_words > 0)
    cum->stack_words = info.stack_offset + info.stack_words;

  cum->arg_number++;
}

/* Return an RTL expression containing the register for the given mode,
   or 0 if the argument is to be passed on the stack.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum; /* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  struct mips_arg_info info;

  /* We will be called with a mode of VOIDmode after the last argument
     has been seen.  Whatever we return will be passed to the call
     insn.  If we need any shifts for small structures, return them in
     a PARALLEL; in that case, stuff the mips16 fp_code in as the
     mode.  Otherwise, if we need a mips16 fp_code, return a REG
     with the code stored as the mode.  */
  if (mode == VOIDmode)
    {
      if (cum->num_adjusts > 0)
	return gen_rtx_PARALLEL ((enum machine_mode) cum->fp_code,
				 gen_rtvec_v (cum->num_adjusts,
					      (rtx *) cum->adjust));

      else if (TARGET_MIPS16 && cum->fp_code != 0)
	return gen_rtx_REG ((enum machine_mode) cum->fp_code, 0);

      else
	return 0;
    }

  mips_arg_info (cum, mode, type, named, &info);

  /* Return straight away if the whole argument is passed on the stack.  */
  if (info.reg_offset == MAX_ARGS_IN_REGISTERS)
    return 0;

  if (type != 0
      && TREE_CODE (type) == RECORD_TYPE
      && (mips_abi == ABI_N32 || mips_abi == ABI_64)
      && TYPE_SIZE_UNIT (type)
      && host_integerp (TYPE_SIZE_UNIT (type), 1)
      && named
      && mode != DFmode)
    {
      /* The Irix 6 n32/n64 ABIs say that if any 64 bit chunk of the
	 structure contains a double in its entirety, then that 64 bit
	 chunk is passed in a floating point register.  */
      tree field;

      /* First check to see if there is any such field.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	if (TREE_CODE (field) == FIELD_DECL
	    && TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
	    && TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD
	    && host_integerp (bit_position (field), 0)
	    && int_bit_position (field) % BITS_PER_WORD == 0)
	  break;

      if (field != 0)
	{
	  /* Now handle the special case by returning a PARALLEL
	     indicating where each 64 bit chunk goes.  INFO.REG_WORDS
	     chunks are passed in registers.  */
	  unsigned int i;
	  HOST_WIDE_INT bitpos;
	  rtx ret;

	  /* assign_parms checks the mode of ENTRY_PARM, so we must
	     use the actual mode here.  */
	  ret = gen_rtx_PARALLEL (mode, rtvec_alloc (info.reg_words));

	  bitpos = 0;
	  field = TYPE_FIELDS (type);
	  for (i = 0; i < info.reg_words; i++)
	    {
	      rtx reg;

	      for (; field; field = TREE_CHAIN (field))
		if (TREE_CODE (field) == FIELD_DECL
		    && int_bit_position (field) >= bitpos)
		  break;

	      if (field
		  && int_bit_position (field) == bitpos
		  && TREE_CODE (TREE_TYPE (field)) == REAL_TYPE
		  && !TARGET_SOFT_FLOAT
		  && TYPE_PRECISION (TREE_TYPE (field)) == BITS_PER_WORD)
		reg = gen_rtx_REG (DFmode, FP_ARG_FIRST + info.reg_offset + i);
	      else
		reg = gen_rtx_REG (DImode, GP_ARG_FIRST + info.reg_offset + i);

	      XVECEXP (ret, 0, i)
		= gen_rtx_EXPR_LIST (VOIDmode, reg,
				     GEN_INT (bitpos / BITS_PER_UNIT));

	      bitpos += BITS_PER_WORD;
	    }
	  return ret;
	}
    }

  if (mips_abi == ABI_MEABI && info.fpr_p && !cum->prototype)
    {
      /* To make K&R varargs work we need to pass floating
	 point arguments in both integer and FP registers.  */
      return gen_rtx_PARALLEL
	(mode,
	 gen_rtvec (2,
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG (mode,
						    GP_ARG_FIRST
						    + info.reg_offset),
				       const0_rtx),
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG (mode,
						    FP_ARG_FIRST
						    + info.reg_offset),
				       const0_rtx)));
    }

  if (info.fpr_p)
    return gen_rtx_REG (mode, FP_ARG_FIRST + info.reg_offset);
  else
    return gen_rtx_REG (mode, GP_ARG_FIRST + info.reg_offset);
}

int
function_arg_partial_nregs (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum; /* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  struct mips_arg_info info;

  mips_arg_info (cum, mode, type, named, &info);
  return info.stack_words > 0 ? info.reg_words : 0;
}

int
mips_setup_incoming_varargs (cum, mode, type, no_rtl)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int no_rtl;
{
  CUMULATIVE_ARGS local_cum;
  int gp_saved, fp_saved;

  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    return 0;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argument.  Advance a local copy of CUM past the last "real" named
     argument, to find out how many registers are left over.  */

  local_cum = *cum;
  FUNCTION_ARG_ADVANCE (local_cum, mode, type, 1);

  /* Found out how many registers we need to save.  */
  gp_saved = MAX_ARGS_IN_REGISTERS - local_cum.num_gprs;
  fp_saved = (EABI_FLOAT_VARARGS_P
	      ? MAX_ARGS_IN_REGISTERS - local_cum.num_fprs
	      : 0);

  if (!no_rtl)
    {
      if (gp_saved > 0)
	{
	  rtx ptr, mem;

	  ptr = virtual_incoming_args_rtx;
	  if (mips_abi == ABI_EABI)
	    ptr = plus_constant (ptr, -gp_saved * UNITS_PER_WORD);
	  mem = gen_rtx_MEM (BLKmode, ptr);

	  /* va_arg is an array access in this case, which causes
	     it to get MEM_IN_STRUCT_P set.  We must set it here
	     so that the insn scheduler won't assume that these
	     stores can't possibly overlap with the va_arg loads.  */
	  if (mips_abi != ABI_EABI && BYTES_BIG_ENDIAN)
	    MEM_SET_IN_STRUCT_P (mem, 1);

	  move_block_from_reg (local_cum.num_gprs + GP_ARG_FIRST, mem,
			       gp_saved, gp_saved * UNITS_PER_WORD);
	}
      if (fp_saved > 0)
	{
	  /* We can't use move_block_from_reg, because it will use
	     the wrong mode. */
	  enum machine_mode mode;
	  int off, i;

	  /* Set OFF to the offset from virtual_incoming_args_rtx of
	     the first float register.   The FP save area lies below
	     the integer one, and is aligned to UNITS_PER_FPVALUE bytes.  */
	  off = -gp_saved * UNITS_PER_WORD;
	  off &= ~(UNITS_PER_FPVALUE - 1);
	  off -= fp_saved * UNITS_PER_FPREG;

	  mode = TARGET_SINGLE_FLOAT ? SFmode : DFmode;

	  for (i = local_cum.num_fprs; i < MAX_ARGS_IN_REGISTERS; i += FP_INC)
	    {
	      rtx ptr = plus_constant (virtual_incoming_args_rtx, off);
	      emit_move_insn (gen_rtx_MEM (mode, ptr),
			      gen_rtx_REG (mode, FP_ARG_FIRST + i));
	      off += UNITS_PER_HWFPVALUE;
	    }
	}
    }
  return (gp_saved * UNITS_PER_WORD) + (fp_saved * UNITS_PER_FPREG);
}

/* Create the va_list data type.
   We keep 3 pointers, and two offsets.
   Two pointers are to the overflow area, which starts at the CFA.
     One of these is constant, for addressing into the GPR save area below it.
     The other is advanced up the stack through the overflow region.
   The third pointer is to the GPR save area.  Since the FPR save area
     is just below it, we can address FPR slots off this pointer.
   We also keep two one-byte offsets, which are to be subtracted from the
     constant pointers to yield addresses in the GPR and FPR save areas.
     These are downcounted as float or non-float arguments are used,
     and when they get to zero, the argument must be obtained from the
     overflow region.
   If !EABI_FLOAT_VARARGS_P, then no FPR save area exists, and a single
     pointer is enough.  It's started at the GPR save area, and is
     advanced, period.
   Note that the GPR save area is not constant size, due to optimization
     in the prologue.  Hence, we can't use a design with two pointers
     and two offsets, although we could have designed this with two pointers
     and three offsets.  */


tree
mips_build_va_list ()
{
  if (EABI_FLOAT_VARARGS_P)
    {
      tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff, record;

      record = make_node (RECORD_TYPE);

      f_ovfl = build_decl (FIELD_DECL, get_identifier ("__overflow_argptr"),
			  ptr_type_node);
      f_gtop = build_decl (FIELD_DECL, get_identifier ("__gpr_top"),
			  ptr_type_node);
      f_ftop = build_decl (FIELD_DECL, get_identifier ("__fpr_top"),
			  ptr_type_node);
      f_goff = build_decl (FIELD_DECL, get_identifier ("__gpr_offset"),
			  unsigned_char_type_node);
      f_foff = build_decl (FIELD_DECL, get_identifier ("__fpr_offset"),
			  unsigned_char_type_node);


      DECL_FIELD_CONTEXT (f_ovfl) = record;
      DECL_FIELD_CONTEXT (f_gtop) = record;
      DECL_FIELD_CONTEXT (f_ftop) = record;
      DECL_FIELD_CONTEXT (f_goff) = record;
      DECL_FIELD_CONTEXT (f_foff) = record;

      TYPE_FIELDS (record) = f_ovfl;
      TREE_CHAIN (f_ovfl) = f_gtop;
      TREE_CHAIN (f_gtop) = f_ftop;
      TREE_CHAIN (f_ftop) = f_goff;
      TREE_CHAIN (f_goff) = f_foff;

      layout_type (record);
      return record;
    }
  else
    return ptr_type_node;
}

/* Implement va_start.   stdarg_p is always 1.  */

void
mips_va_start (valist, nextarg)
     tree valist;
     rtx nextarg;
{
  const CUMULATIVE_ARGS *cum = &current_function_args_info;

  /* ARG_POINTER_REGNUM is initialized to STACK_POINTER_BOUNDARY, but
     since the stack is aligned for a pair of argument-passing slots,
     and the beginning of a variable argument list may be an odd slot,
     we have to decrease its alignment.  */
  if (cfun && cfun->emit->regno_pointer_align)
    while (((current_function_pretend_args_size * BITS_PER_UNIT)
	    & (REGNO_POINTER_ALIGN (ARG_POINTER_REGNUM) - 1)) != 0)
      REGNO_POINTER_ALIGN (ARG_POINTER_REGNUM) /= 2;

  if (mips_abi == ABI_EABI)
    {
      int gpr_save_area_size;

      gpr_save_area_size
	= (MAX_ARGS_IN_REGISTERS - cum->num_gprs) * UNITS_PER_WORD;

      if (EABI_FLOAT_VARARGS_P)
	{
	  tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff;
	  tree ovfl, gtop, ftop, goff, foff;
	  tree t;
	  int fpr_offset;
	  int fpr_save_area_size;

	  f_ovfl = TYPE_FIELDS (va_list_type_node);
	  f_gtop = TREE_CHAIN (f_ovfl);
	  f_ftop = TREE_CHAIN (f_gtop);
	  f_goff = TREE_CHAIN (f_ftop);
	  f_foff = TREE_CHAIN (f_goff);

	  ovfl = build (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl);
	  gtop = build (COMPONENT_REF, TREE_TYPE (f_gtop), valist, f_gtop);
	  ftop = build (COMPONENT_REF, TREE_TYPE (f_ftop), valist, f_ftop);
	  goff = build (COMPONENT_REF, TREE_TYPE (f_goff), valist, f_goff);
	  foff = build (COMPONENT_REF, TREE_TYPE (f_foff), valist, f_foff);

	  /* Emit code to initialize OVFL, which points to the next varargs
	     stack argument.  CUM->STACK_WORDS gives the number of stack
	     words used by named arguments.  */
	  t = make_tree (TREE_TYPE (ovfl), virtual_incoming_args_rtx);
	  if (cum->stack_words > 0)
	    t = build (PLUS_EXPR, TREE_TYPE (ovfl), t,
		       build_int_2 (cum->stack_words * UNITS_PER_WORD, 0));
	  t = build (MODIFY_EXPR, TREE_TYPE (ovfl), ovfl, t);
 	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  /* Emit code to initialize GTOP, the top of the GPR save area.  */
	  t = make_tree (TREE_TYPE (gtop), virtual_incoming_args_rtx);
	  t = build (MODIFY_EXPR, TREE_TYPE (gtop), gtop, t);
 	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  /* Emit code to initialize FTOP, the top of the FPR save area.
	     This address is gpr_save_area_bytes below GTOP, rounded
	     down to the next fp-aligned boundary.  */
	  t = make_tree (TREE_TYPE (ftop), virtual_incoming_args_rtx);
	  fpr_offset = gpr_save_area_size + UNITS_PER_FPVALUE - 1;
	  fpr_offset &= ~(UNITS_PER_FPVALUE - 1);
	  if (fpr_offset)
	    t = build (PLUS_EXPR, TREE_TYPE (ftop), t,
		       build_int_2 (-fpr_offset, -1));
	  t = build (MODIFY_EXPR, TREE_TYPE (ftop), ftop, t);
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  /* Emit code to initialize GOFF, the offset from GTOP of the
	     next GPR argument.  */
	  t = build (MODIFY_EXPR, TREE_TYPE (goff), goff,
		     build_int_2 (gpr_save_area_size, 0));
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  /* Likewise emit code to initialize FOFF, the offset from FTOP
	     of the next FPR argument.  */
	  fpr_save_area_size
	    = (MAX_ARGS_IN_REGISTERS - cum->num_fprs) * UNITS_PER_FPREG;
	  t = build (MODIFY_EXPR, TREE_TYPE (foff), foff,
		     build_int_2 (fpr_save_area_size, 0));
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	}
      else
	{
	  /* Everything is in the GPR save area, or in the overflow
	     area which is contiguous with it.  */
	  nextarg = plus_constant (nextarg, -gpr_save_area_size);
	  std_expand_builtin_va_start (valist, nextarg);
	}
    }
  else
    std_expand_builtin_va_start (valist, nextarg);
}

/* Implement va_arg.  */

rtx
mips_va_arg (valist, type)
     tree valist, type;
{
  HOST_WIDE_INT size, rsize;
  rtx addr_rtx;
  tree t;

  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;

  if (mips_abi == ABI_EABI)
    {
      bool indirect;
      rtx r;

      indirect
	= function_arg_pass_by_reference (NULL, TYPE_MODE (type), type, 0);

      if (indirect)
	{
	  size = POINTER_SIZE / BITS_PER_UNIT;
	  rsize = UNITS_PER_WORD;
	}

      addr_rtx = gen_reg_rtx (Pmode);

      if (!EABI_FLOAT_VARARGS_P)
	{
	  /* Case of all args in a merged stack.  No need to check bounds,
	     just advance valist along the stack.  */

	  tree gpr = valist;
	  if (!indirect
	      && !TARGET_64BIT
	      && TYPE_ALIGN (type) > (unsigned) BITS_PER_WORD)
	    {
	      /* Align the pointer using: ap = (ap + align - 1) & -align,
		 where align is 2 * UNITS_PER_WORD.  */
	      t = build (PLUS_EXPR, TREE_TYPE (gpr), gpr,
			 build_int_2 (2 * UNITS_PER_WORD - 1, 0));
	      t = build (BIT_AND_EXPR, TREE_TYPE (t), t,
			 build_int_2 (-2 * UNITS_PER_WORD, -1));
	      t = build (MODIFY_EXPR, TREE_TYPE (gpr), gpr, t);
	      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	    }

	  /* Emit code to set addr_rtx to the valist, and postincrement
	     the valist by the size of the argument, rounded up to the
	     next word.	 */
	  t = build (POSTINCREMENT_EXPR, TREE_TYPE (gpr), gpr,
		     size_int (rsize));
	  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
	  if (r != addr_rtx)
	    emit_move_insn (addr_rtx, r);

	  /* Flush the POSTINCREMENT.  */
	  emit_queue();
	}
      else
	{
	  /* Not a simple merged stack.	 */

	  tree f_ovfl, f_gtop, f_ftop, f_goff, f_foff;
	  tree ovfl, top, off;
	  rtx lab_over = NULL_RTX, lab_false;
	  HOST_WIDE_INT osize;

	  f_ovfl = TYPE_FIELDS (va_list_type_node);
	  f_gtop = TREE_CHAIN (f_ovfl);
	  f_ftop = TREE_CHAIN (f_gtop);
	  f_goff = TREE_CHAIN (f_ftop);
	  f_foff = TREE_CHAIN (f_goff);

	  /* We maintain separate pointers and offsets for floating-point
	     and integer arguments, but we need similar code in both cases.
	     Let:

		 TOP be the top of the register save area;
		 OFF be the offset from TOP of the next register;
		 ADDR_RTX be the address of the argument; and
		 RSIZE be the number of bytes used to store the argument
		   when it's in the register save area
		 OSIZE be the number of bytes used to store it when it's
		   in the stack overflow area
		 PADDING be (BYTES_BIG_ENDIAN ? OSIZE - RSIZE : 0)

	     The code we want is:

		  1: off &= -rsize;	  // round down
		  2: if (off != 0)
		  3:   {
		  4:	 addr_rtx = top - off;
		  5:	 off -= rsize;
		  6:   }
		  7: else
		  8:   {
		  9:	 ovfl += ((intptr_t) ovfl + osize - 1) & -osize;
		 10:	 addr_rtx = ovfl + PADDING;
		 11:	 ovfl += osize;
		 14:   }

	     [1] and [9] can sometimes be optimized away.  */

	  lab_false = gen_label_rtx ();
	  lab_over = gen_label_rtx ();

	  ovfl = build (COMPONENT_REF, TREE_TYPE (f_ovfl), valist, f_ovfl);

	  if (TREE_CODE (type) == REAL_TYPE)
	    {
	      top = build (COMPONENT_REF, TREE_TYPE (f_ftop), valist, f_ftop);
	      off = build (COMPONENT_REF, TREE_TYPE (f_foff), valist, f_foff);

	      /* When floating-point registers are saved to the stack,
		 each one will take up UNITS_PER_HWFPVALUE bytes, regardless
		 of the float's precision.  */
	      rsize = UNITS_PER_HWFPVALUE;
	    }
	  else
	    {
	      top = build (COMPONENT_REF, TREE_TYPE (f_gtop), valist, f_gtop);
	      off = build (COMPONENT_REF, TREE_TYPE (f_goff), valist, f_goff);
	      if (rsize > UNITS_PER_WORD)
		{
		  /* [1] Emit code for: off &= -rsize.	*/
		  t = build (BIT_AND_EXPR, TREE_TYPE (off), off,
			     build_int_2 (-rsize, -1));
		  t = build (MODIFY_EXPR, TREE_TYPE (off), off, t);
		  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
		}
	    }
	  /* Every overflow argument must take up at least UNITS_PER_WORD
	     bytes (= PARM_BOUNDARY bits).  RSIZE can sometimes be smaller
	     than that, such as in the combination -mgp64 -msingle-float
	     -fshort-double.  Doubles passed in registers will then take
	     up UNITS_PER_HWFPVALUE bytes, but those passed on the stack
	     take up UNITS_PER_WORD bytes.  */
	  osize = MAX (rsize, UNITS_PER_WORD);

	  /* [2] Emit code to branch if off == 0.  */
	  r = expand_expr (off, NULL_RTX, TYPE_MODE (TREE_TYPE (off)),
			   EXPAND_NORMAL);
	  emit_cmp_and_jump_insns (r, const0_rtx, EQ, const1_rtx, GET_MODE (r),
				   1, lab_false);

	  /* [4] Emit code for: addr_rtx = top - off.  */
	  t = build (MINUS_EXPR, TREE_TYPE (top), top, off);
	  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
	  if (r != addr_rtx)
	    emit_move_insn (addr_rtx, r);

	  /* [5] Emit code for: off -= rsize.  */
	  t = build (MINUS_EXPR, TREE_TYPE (off), off, build_int_2 (rsize, 0));
	  t = build (MODIFY_EXPR, TREE_TYPE (off), off, t);
	  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

	  /* [7] Emit code to jump over the else clause, then the label
	     that starts it.  */
	  emit_queue();
	  emit_jump (lab_over);
	  emit_barrier ();
	  emit_label (lab_false);

	  if (osize > UNITS_PER_WORD)
	    {
	      /* [9] Emit: ovfl += ((intptr_t) ovfl + osize - 1) & -osize.  */
	      t = build (PLUS_EXPR, TREE_TYPE (ovfl), ovfl,
			 build_int_2 (osize - 1, 0));
	      t = build (BIT_AND_EXPR, TREE_TYPE (ovfl), t,
			 build_int_2 (-osize, -1));
	      t = build (MODIFY_EXPR, TREE_TYPE (ovfl), ovfl, t);
	      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
	    }

	  /* [10, 11].	Emit code to store ovfl in addr_rtx, then
	     post-increment ovfl by osize.  On big-endian machines,
	     the argument has OSIZE - RSIZE bytes of leading padding.  */
	  t = build (POSTINCREMENT_EXPR, TREE_TYPE (ovfl), ovfl,
		     size_int (osize));
	  if (BYTES_BIG_ENDIAN && osize > rsize)
	    t = build (PLUS_EXPR, TREE_TYPE (t), t,
		       build_int_2 (osize - rsize, 0));
	  r = expand_expr (t, addr_rtx, Pmode, EXPAND_NORMAL);
	  if (r != addr_rtx)
	    emit_move_insn (addr_rtx, r);

	  emit_queue();
	  emit_label (lab_over);
	}
      if (indirect)
	{
	  addr_rtx = force_reg (Pmode, addr_rtx);
	  r = gen_rtx_MEM (Pmode, addr_rtx);
	  set_mem_alias_set (r, get_varargs_alias_set ());
	  emit_move_insn (addr_rtx, r);
	}
      else
	{
	  if (BYTES_BIG_ENDIAN && rsize != size)
	    addr_rtx = plus_constant (addr_rtx, rsize - size);
	}
      return addr_rtx;
    }
  else
    {
      /* Not EABI.  */
      int align;

      /* ??? The original va-mips.h did always align, despite the fact
	 that alignments <= UNITS_PER_WORD are preserved by the va_arg
	 increment mechanism.  */

      if ((mips_abi == ABI_N32 || mips_abi == ABI_64)
	  && TYPE_ALIGN (type) > 64)
	align = 16;
      else if (TARGET_64BIT)
	align = 8;
      else if (TYPE_ALIGN (type) > 32)
	align = 8;
      else
	align = 4;

      t = build (PLUS_EXPR, TREE_TYPE (valist), valist,
		 build_int_2 (align - 1, 0));
      t = build (BIT_AND_EXPR, TREE_TYPE (t), t, build_int_2 (-align, -1));
      t = build (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

      /* Everything past the alignment is standard.  */
      return std_expand_builtin_va_arg (valist, type);
    }
}

/* Abort after printing out a specific insn.  */

static void
abort_with_insn (insn, reason)
     rtx insn;
     const char *reason;
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

/* Set up globals to generate code for the ISA or processor
   described by INFO.  */

static void
mips_set_architecture (info)
     const struct mips_cpu_info *info;
{
  if (info != 0)
    {
      mips_arch_info = info;
      mips_arch = info->cpu;
      mips_isa = info->isa;
    }
}


/* Likewise for tuning.  */

static void
mips_set_tune (info)
     const struct mips_cpu_info *info;
{
  if (info != 0)
    {
      mips_tune_info = info;
      mips_tune = info->cpu;
    }
}


/* Set up the threshold for data to go into the small data area, instead
   of the normal data area, and detect any conflicts in the switches.  */

void
override_options ()
{
  int i, start, regno;
  enum machine_mode mode;

  mips_section_threshold = g_switch_set ? g_switch_value : MIPS_DEFAULT_GVALUE;

  if (mips_section_threshold <= 0)
    target_flags &= ~MASK_GPOPT;
  else if (optimize)
    target_flags |= MASK_GPOPT;

  /* If both single-float and soft-float are set, then clear the one that
     was set by TARGET_DEFAULT, leaving the one that was set by the
     user.  We assume here that the specs prevent both being set by the
     user.  */
#ifdef TARGET_DEFAULT
  if (TARGET_SINGLE_FLOAT && TARGET_SOFT_FLOAT)
    target_flags &= ~((TARGET_DEFAULT) & (MASK_SOFT_FLOAT | MASK_SINGLE_FLOAT));
#endif

  /* Interpret -mabi.  */
  mips_abi = MIPS_ABI_DEFAULT;
  if (mips_abi_string != 0)
    {
      if (strcmp (mips_abi_string, "32") == 0)
	mips_abi = ABI_32;
      else if (strcmp (mips_abi_string, "o64") == 0)
	mips_abi = ABI_O64;
      else if (strcmp (mips_abi_string, "n32") == 0)
	mips_abi = ABI_N32;
      else if (strcmp (mips_abi_string, "64") == 0)
	mips_abi = ABI_64;
      else if (strcmp (mips_abi_string, "eabi") == 0)
	mips_abi = ABI_EABI;
      else if (strcmp (mips_abi_string, "meabi") == 0)
	mips_abi = ABI_MEABI;
      else
	fatal_error ("bad value (%s) for -mabi= switch", mips_abi_string);
    }

  /* The following code determines the architecture and register size.
     Similar code was added to GAS 2.14 (see tc-mips.c:md_after_parse_args()).
     The GAS and GCC code should be kept in sync as much as possible.  */

  if (mips_arch_string != 0)
    mips_set_architecture (mips_parse_cpu ("-march", mips_arch_string));

  if (mips_tune_string != 0)
    mips_set_tune (mips_parse_cpu ("-mtune", mips_tune_string));

  if (mips_isa_string != 0)
    {
      /* Handle -mipsN.  */
      int level = atoi (mips_isa_string);
      if (level == 16)
	{
	  /* -mips16 specifies an ASE rather than a processor, so don't
	     change mips_arch here.  -mno-mips16 overrides -mips16.  */
	  if (mips_no_mips16_string == NULL)
	    target_flags |= MASK_MIPS16;
	}
      else if (mips_arch_info != 0)
	{
	  /* -march takes precedence over -mipsN, since it is more descriptive.
	     There's no harm in specifying both as long as the ISA levels
	     are the same.  */
	  if (mips_isa != level)
	    error ("-mips%d conflicts with the other architecture options, which specify a MIPS%d processor",
		   level, mips_isa);
	}
      else
	{
	  mips_set_architecture (mips_cpu_info_from_isa (level));
	  if (mips_arch_info == 0)
	    error ("bad value (%s) for -mips switch", mips_isa_string);
	}
    }

  if (mips_arch_info == 0)
    {
#ifdef MIPS_CPU_STRING_DEFAULT
      mips_set_architecture (mips_parse_cpu ("default CPU",
					     MIPS_CPU_STRING_DEFAULT));
#else
      mips_set_architecture (mips_cpu_info_from_isa (MIPS_ISA_DEFAULT));
#endif
    }

  if (ABI_NEEDS_64BIT_REGS && !ISA_HAS_64BIT_REGS)
    error ("-march=%s is not compatible with the selected ABI",
	   mips_arch_info->name);

  /* Optimize for mips_arch, unless -mtune selects a different processor.  */
  if (mips_tune_info == 0)
    mips_set_tune (mips_arch_info);

  if ((target_flags_explicit & MASK_64BIT) != 0)
    {
      /* The user specified the size of the integer registers.  Make sure
	 it agrees with the ABI and ISA.  */
      if (TARGET_64BIT && !ISA_HAS_64BIT_REGS)
	error ("-mgp64 used with a 32-bit processor");
      else if (!TARGET_64BIT && ABI_NEEDS_64BIT_REGS)
	error ("-mgp32 used with a 64-bit ABI");
      else if (TARGET_64BIT && ABI_NEEDS_32BIT_REGS)
	error ("-mgp64 used with a 32-bit ABI");
    }
  else
    {
      /* Infer the integer register size from the ABI and processor.
	 Restrict ourselves to 32-bit registers if that's all the
	 processor has, or if the ABI cannot handle 64-bit registers.  */
      if (ABI_NEEDS_32BIT_REGS || !ISA_HAS_64BIT_REGS)
	target_flags &= ~MASK_64BIT;
      else
	target_flags |= MASK_64BIT;
    }

  if ((target_flags_explicit & MASK_FLOAT64) != 0)
    {
      /* Really, -mfp32 and -mfp64 are ornamental options.  There's
	 only one right answer here.  */
      if (TARGET_64BIT && TARGET_DOUBLE_FLOAT && !TARGET_FLOAT64)
	error ("unsupported combination: %s", "-mgp64 -mfp32 -mdouble-float");
      else if (!TARGET_64BIT && TARGET_FLOAT64)
	error ("unsupported combination: %s", "-mgp32 -mfp64");
      else if (TARGET_SINGLE_FLOAT && TARGET_FLOAT64)
	error ("unsupported combination: %s", "-mfp64 -msingle-float");
    }
  else
    {
      /* -msingle-float selects 32-bit float registers.  Otherwise the
	 float registers should be the same size as the integer ones.  */
      if (TARGET_64BIT && TARGET_DOUBLE_FLOAT)
	target_flags |= MASK_FLOAT64;
      else
	target_flags &= ~MASK_FLOAT64;
    }

  /* End of code shared with GAS.  */

  if ((target_flags_explicit & MASK_LONG64) == 0)
    {
      /* If no type size setting options (-mlong64,-mint64,-mlong32)
	 were used, then set the type sizes.  In the EABI in 64 bit mode,
	 longs and pointers are 64 bits.  Likewise for the SGI Irix6 N64
	 ABI.  */
      if ((mips_abi == ABI_EABI && TARGET_64BIT) || mips_abi == ABI_64)
	target_flags |= MASK_LONG64;
      else
	target_flags &= ~MASK_LONG64;
    }

  if (MIPS_MARCH_CONTROLS_SOFT_FLOAT
      && (target_flags_explicit & MASK_SOFT_FLOAT) == 0)
    {
      /* For some configurations, it is useful to have -march control
	 the default setting of MASK_SOFT_FLOAT.  */
      switch ((int) mips_arch)
	{
	case PROCESSOR_R4100:
	case PROCESSOR_R4120:
	  target_flags |= MASK_SOFT_FLOAT;
	  break;

	default:
	  target_flags &= ~MASK_SOFT_FLOAT;
	  break;
	}
    }

  if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    flag_pcc_struct_return = 0;

  if ((target_flags_explicit & MASK_BRANCHLIKELY) == 0)
    {
      /* If neither -mbranch-likely nor -mno-branch-likely was given
	 on the command line, set MASK_BRANCHLIKELY based on the target
	 architecture.

	 By default, we enable use of Branch Likely instructions on
	 all architectures which support them except for MIPS32 and MIPS64
	 (i.e., the generic MIPS32 and MIPS64 ISAs, and processors which
	 implement them).

	 The MIPS32 and MIPS64 architecture specifications say "Software
	 is strongly encouraged to avoid use of Branch Likely
	 instructions, as they will be removed from a future revision
	 of the [MIPS32 and MIPS64] architecture."  Therefore, we do not
	 issue those instructions unless instructed to do so by
	 -mbranch-likely.  */
      if (ISA_HAS_BRANCHLIKELY && !(ISA_MIPS32 || ISA_MIPS64))
	target_flags |= MASK_BRANCHLIKELY;
      else
	target_flags &= ~MASK_BRANCHLIKELY;
    }
  if (TARGET_BRANCHLIKELY && !ISA_HAS_BRANCHLIKELY)
    warning ("generation of Branch Likely instructions enabled, but not supported by architecture");

  /* -fpic (-KPIC) is the default when TARGET_ABICALLS is defined.  We need
     to set flag_pic so that the LEGITIMATE_PIC_OPERAND_P macro will work.  */
  /* ??? -non_shared turns off pic code generation, but this is not
     implemented.  */
  if (TARGET_ABICALLS)
    {
      mips_abicalls = MIPS_ABICALLS_YES;
      flag_pic = 1;
      if (mips_section_threshold > 0)
	warning ("-G is incompatible with PIC code which is the default");
    }
  else
    mips_abicalls = MIPS_ABICALLS_NO;

  /* -membedded-pic is a form of PIC code suitable for embedded
     systems.  All calls are made using PC relative addressing, and
     all data is addressed using the $gp register.  This requires gas,
     which does most of the work, and GNU ld, which automatically
     expands PC relative calls which are out of range into a longer
     instruction sequence.  All gcc really does differently is
     generate a different sequence for a switch.  */
  if (TARGET_EMBEDDED_PIC)
    {
      flag_pic = 1;
      if (TARGET_ABICALLS)
	warning ("-membedded-pic and -mabicalls are incompatible");

      if (g_switch_set)
	warning ("-G and -membedded-pic are incompatible");

      /* Setting mips_section_threshold is not required, because gas
	 will force everything to be GP addressable anyhow, but
	 setting it will cause gcc to make better estimates of the
	 number of instructions required to access a particular data
	 item.  */
      mips_section_threshold = 0x7fffffff;
    }

  /* This optimization requires a linker that can support a R_MIPS_LO16
     relocation which is not immediately preceded by a R_MIPS_HI16 relocation.
     GNU ld has this support, but not all other MIPS linkers do, so we enable
     this optimization only if the user requests it, or if GNU ld is the
     standard linker for this configuration.  */
  /* ??? This does not work when target addresses are DImode.
     This is because we are missing DImode high/lo_sum patterns.  */
  if (TARGET_GAS && ! TARGET_MIPS16 && TARGET_SPLIT_ADDRESSES && optimize && ! flag_pic
      && Pmode == SImode)
    mips_split_addresses = 1;
  else
    mips_split_addresses = 0;

  /* -mrnames says to use the MIPS software convention for register
     names instead of the hardware names (ie, $a0 instead of $4).
     We do this by switching the names in mips_reg_names, which the
     reg_names points into via the REGISTER_NAMES macro.  */

  if (TARGET_NAME_REGS)
    memcpy (mips_reg_names, mips_sw_reg_names, sizeof (mips_reg_names));

  /* When compiling for the mips16, we can not use floating point.  We
     record the original hard float value in mips16_hard_float.  */
  if (TARGET_MIPS16)
    {
      if (TARGET_SOFT_FLOAT)
	mips16_hard_float = 0;
      else
	mips16_hard_float = 1;
      target_flags |= MASK_SOFT_FLOAT;

      /* Don't run the scheduler before reload, since it tends to
         increase register pressure.  */
      flag_schedule_insns = 0;
    }

  /* We put -mentry in TARGET_OPTIONS rather than TARGET_SWITCHES only
     to avoid using up another bit in target_flags.  */
  if (mips_entry_string != NULL)
    {
      if (*mips_entry_string != '\0')
	error ("invalid option `entry%s'", mips_entry_string);

      if (! TARGET_MIPS16)
	warning ("-mentry is only meaningful with -mips-16");
      else
	mips_entry = 1;
    }

  /* We copy TARGET_MIPS16 into the mips16 global variable, so that
     attributes can access it.  */
  if (TARGET_MIPS16)
    mips16 = 1;
  else
    mips16 = 0;

#ifdef MIPS_TFMODE_FORMAT
  real_format_for_mode[TFmode - QFmode] = &MIPS_TFMODE_FORMAT;
#endif
  
  mips_print_operand_punct['?'] = 1;
  mips_print_operand_punct['#'] = 1;
  mips_print_operand_punct['&'] = 1;
  mips_print_operand_punct['!'] = 1;
  mips_print_operand_punct['*'] = 1;
  mips_print_operand_punct['@'] = 1;
  mips_print_operand_punct['.'] = 1;
  mips_print_operand_punct['('] = 1;
  mips_print_operand_punct[')'] = 1;
  mips_print_operand_punct['['] = 1;
  mips_print_operand_punct[']'] = 1;
  mips_print_operand_punct['<'] = 1;
  mips_print_operand_punct['>'] = 1;
  mips_print_operand_punct['{'] = 1;
  mips_print_operand_punct['}'] = 1;
  mips_print_operand_punct['^'] = 1;
  mips_print_operand_punct['$'] = 1;
  mips_print_operand_punct['+'] = 1;
  mips_print_operand_punct['~'] = 1;

  mips_char_to_class['d'] = TARGET_MIPS16 ? M16_REGS : GR_REGS;
  mips_char_to_class['e'] = M16_NA_REGS;
  mips_char_to_class['t'] = T_REG;
  mips_char_to_class['f'] = (TARGET_HARD_FLOAT ? FP_REGS : NO_REGS);
  mips_char_to_class['h'] = HI_REG;
  mips_char_to_class['l'] = LO_REG;
  mips_char_to_class['a'] = HILO_REG;
  mips_char_to_class['x'] = MD_REGS;
  mips_char_to_class['b'] = ALL_REGS;
  mips_char_to_class['y'] = GR_REGS;
  mips_char_to_class['z'] = ST_REGS;
  mips_char_to_class['B'] = COP0_REGS;
  mips_char_to_class['C'] = COP2_REGS;
  mips_char_to_class['D'] = COP3_REGS;

  /* Set up array to map GCC register number to debug register number.
     Ignore the special purpose register numbers.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    mips_dbx_regno[i] = -1;

  start = GP_DBX_FIRST - GP_REG_FIRST;
  for (i = GP_REG_FIRST; i <= GP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  start = FP_DBX_FIRST - FP_REG_FIRST;
  for (i = FP_REG_FIRST; i <= FP_REG_LAST; i++)
    mips_dbx_regno[i] = i + start;

  /* Set up array giving whether a given register can hold a given mode.
     At present, restrict ints from being in FP registers, because reload
     is a little enthusiastic about storing extra values in FP registers,
     and this is not good for things like OS kernels.  Also, due to the
     mandatory delay, it is as fast to load from cached memory as to move
     from the FP register.  */

  for (mode = VOIDmode;
       mode != MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int)mode + 1))
    {
      register int size		     = GET_MODE_SIZE (mode);
      register enum mode_class class = GET_MODE_CLASS (mode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	{
	  register int temp;

	  if (mode == CCmode)
	    {
	      if (! ISA_HAS_8CC)
		temp = (regno == FPSW_REGNUM);
	      else
		temp = (ST_REG_P (regno) || GP_REG_P (regno)
			|| FP_REG_P (regno));
	    }

	  else if (GP_REG_P (regno))
	    temp = ((regno & 1) == 0 || size <= UNITS_PER_WORD);

	  else if (FP_REG_P (regno))
	    temp = (((regno % FP_INC) == 0
		     /* I think this change is OK regardless of abi, but
                        I'm being cautions untill I can test this more.
                        HARD_REGNO_MODE_OK is about whether or not you
                        can move to and from a register without changing
                        the value, not about whether math works on the
                        register. */
		     || (mips_abi == ABI_MEABI && size <= 4))
		    && (((class == MODE_FLOAT || class == MODE_COMPLEX_FLOAT)
			 && size <= UNITS_PER_FPVALUE)
			/* Allow integer modes that fit into a single
			   register.  We need to put integers into FPRs
			   when using instructions like cvt and trunc.  */
			|| (class == MODE_INT && size <= UNITS_PER_FPREG)
			/* Allow TFmode for CCmode reloads.  */
			|| (ISA_HAS_8CC && mode == TFmode)));

	  else if (MD_REG_P (regno))
	    temp = (class == MODE_INT
		    && (size <= UNITS_PER_WORD
			|| (regno == MD_REG_FIRST
			    && size == 2 * UNITS_PER_WORD)));

	  else if (ALL_COP_REG_P (regno))
	    temp = (class == MODE_INT && size <= UNITS_PER_WORD);
	  else
	    temp = 0;

	  mips_hard_regno_mode_ok[(int)mode][regno] = temp;
	}
    }

  /* Save GPR registers in word_mode sized hunks.  word_mode hasn't been
     initialized yet, so we can't use that here.  */
  gpr_mode = TARGET_64BIT ? DImode : SImode;

  /* Provide default values for align_* for 64-bit targets.  */
  if (TARGET_64BIT && !TARGET_MIPS16)
    {
      if (align_loops == 0)
	align_loops = 8;
      if (align_jumps == 0)
	align_jumps = 8;
      if (align_functions == 0)
	align_functions = 8;
    }

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &mips_init_machine_status;
}

/* Implement CONDITIONAL_REGISTER_USAGE.  */

void
mips_conditional_register_usage ()
{
  if (!TARGET_HARD_FLOAT)
    {
      int regno;

      for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
      for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }
  else if (! ISA_HAS_8CC)
    {
      int regno;

      /* We only have a single condition code register.  We
	 implement this by hiding all the condition code registers,
	 and generating RTL that refers directly to ST_REG_FIRST.  */
      for (regno = ST_REG_FIRST; regno <= ST_REG_LAST; regno++)
	fixed_regs[regno] = call_used_regs[regno] = 1;
    }
  /* In mips16 mode, we permit the $t temporary registers to be used
     for reload.  We prohibit the unused $s registers, since they
     are caller saved, and saving them via a mips16 register would
     probably waste more time than just reloading the value.  */
  if (TARGET_MIPS16)
    {
      fixed_regs[18] = call_used_regs[18] = 1;
      fixed_regs[19] = call_used_regs[19] = 1;
      fixed_regs[20] = call_used_regs[20] = 1;
      fixed_regs[21] = call_used_regs[21] = 1;
      fixed_regs[22] = call_used_regs[22] = 1;
      fixed_regs[23] = call_used_regs[23] = 1;
      fixed_regs[26] = call_used_regs[26] = 1;
      fixed_regs[27] = call_used_regs[27] = 1;
      fixed_regs[30] = call_used_regs[30] = 1;
    }
  /* fp20-23 are now caller saved.  */
  if (mips_abi == ABI_64)
    {
      int regno;
      for (regno = FP_REG_FIRST + 20; regno < FP_REG_FIRST + 24; regno++)
	call_really_used_regs[regno] = call_used_regs[regno] = 1;
    }
  /* odd registers from fp21 to fp31 are now caller saved.  */
  if (mips_abi == ABI_N32 || mips_abi == ABI_MEABI)
    {
      int regno;
      for (regno = FP_REG_FIRST + 21; regno <= FP_REG_FIRST + 31; regno+=2)
	call_really_used_regs[regno] = call_used_regs[regno] = 1;
    }
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */
static struct machine_function *
mips_init_machine_status ()
{
  return ((struct machine_function *)
	  ggc_alloc_cleared (sizeof (struct machine_function)));
}

/* On the mips16, we want to allocate $24 (T_REG) before other
   registers for instructions for which it is possible.  This helps
   avoid shuffling registers around in order to set up for an xor,
   encouraging the compiler to use a cmp instead.  */

void
mips_order_regs_for_local_alloc ()
{
  register int i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    reg_alloc_order[i] = i;

  if (TARGET_MIPS16)
    {
      /* It really doesn't matter where we put register 0, since it is
         a fixed register anyhow.  */
      reg_alloc_order[0] = 24;
      reg_alloc_order[24] = 0;
    }
}


/* The MIPS debug format wants all automatic variables and arguments
   to be in terms of the virtual frame pointer (stack pointer before
   any adjustment in the function), while the MIPS 3.0 linker wants
   the frame pointer to be the stack pointer after the initial
   adjustment.  So, we do the adjustment here.  The arg pointer (which
   is eliminated) points to the virtual frame pointer, while the frame
   pointer (which may be eliminated) points to the stack pointer after
   the initial adjustments.  */

HOST_WIDE_INT
mips_debugger_offset (addr, offset)
     rtx addr;
     HOST_WIDE_INT offset;
{
  rtx offset2 = const0_rtx;
  rtx reg = eliminate_constant_term (addr, &offset2);

  if (offset == 0)
    offset = INTVAL (offset2);

  if (reg == stack_pointer_rtx || reg == frame_pointer_rtx
      || reg == hard_frame_pointer_rtx)
    {
      HOST_WIDE_INT frame_size = (!cfun->machine->frame.initialized)
				  ? compute_frame_size (get_frame_size ())
				  : cfun->machine->frame.total_size;

      /* MIPS16 frame is smaller */
      if (frame_pointer_needed && TARGET_MIPS16)
	frame_size -= current_function_outgoing_args_size;

      offset = offset - frame_size;
    }

  /* sdbout_parms does not want this to crash for unrecognized cases.  */
#if 0
  else if (reg != arg_pointer_rtx)
    abort_with_insn (addr, "mips_debugger_offset called with non stack/frame/arg pointer");
#endif

  return offset;
}

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

   The MIPS specific codes are:

   'X'  X is CONST_INT, prints 32 bits in hexadecimal format = "0x%08x",
   'x'  X is CONST_INT, prints 16 bits in hexadecimal format = "0x%04x",
   'd'  output integer constant in decimal,
   'z'	if the operand is 0, use $0 instead of normal operand.
   'D'  print second part of double-word register or memory operand.
   'L'  print low-order register of double-word register operand.
   'M'  print high-order register of double-word register operand.
   'C'  print part of opcode for a branch condition.
   'F'  print part of opcode for a floating-point branch condition.
   'N'  print part of opcode for a branch condition, inverted.
   'W'  print part of opcode for a floating-point branch condition, inverted.
   'S'  X is CODE_LABEL, print with prefix of "LS" (for embedded switch).
   'B'  print 'z' for EQ, 'n' for NE
   'b'  print 'n' for EQ, 'z' for NE
   'T'  print 'f' for EQ, 't' for NE
   't'  print 't' for EQ, 'f' for NE
   'Z'  print register and a comma, but print nothing for $fcc0
   '('	Turn on .set noreorder
   ')'	Turn on .set reorder
   '['	Turn on .set noat
   ']'	Turn on .set at
   '<'	Turn on .set nomacro
   '>'	Turn on .set macro
   '{'	Turn on .set volatile (not GAS)
   '}'	Turn on .set novolatile (not GAS)
   '&'	Turn on .set noreorder if filling delay slots
   '*'	Turn on both .set noreorder and .set nomacro if filling delay slots
   '!'	Turn on .set nomacro if filling delay slots
   '#'	Print nop if in a .set noreorder section.
   '?'	Print 'l' if we are to use a branch likely instead of normal branch.
   '@'	Print the name of the assembler temporary register (at or $1).
   '.'	Print the name of the register with a hard-wired zero (zero or $0).
   '^'	Print the name of the pic call-through register (t9 or $25).
   '$'	Print the name of the stack pointer register (sp or $29).
   '+'	Print the name of the gp register (gp or $28).
   '~'	Output an branch alignment to LABEL_ALIGN(NULL).  */

void
print_operand (file, op, letter)
     FILE *file;		/* file to write to */
     rtx op;			/* operand to print */
     int letter;		/* %<letter> or 0 */
{
  register enum rtx_code code;

  if (PRINT_OPERAND_PUNCT_VALID_P (letter))
    {
      switch (letter)
	{
	case '?':
	  if (mips_branch_likely)
	    putc ('l', file);
	  break;

	case '@':
	  fputs (reg_names [GP_REG_FIRST + 1], file);
	  break;

	case '^':
	  fputs (reg_names [PIC_FUNCTION_ADDR_REGNUM], file);
	  break;

	case '.':
	  fputs (reg_names [GP_REG_FIRST + 0], file);
	  break;

	case '$':
	  fputs (reg_names[STACK_POINTER_REGNUM], file);
	  break;

	case '+':
	  fputs (reg_names[GP_REG_FIRST + 28], file);
	  break;

	case '&':
	  if (final_sequence != 0 && set_noreorder++ == 0)
	    fputs (".set\tnoreorder\n\t", file);
	  break;

	case '*':
	  if (final_sequence != 0)
	    {
	      if (set_noreorder++ == 0)
		fputs (".set\tnoreorder\n\t", file);

	      if (set_nomacro++ == 0)
		fputs (".set\tnomacro\n\t", file);
	    }
	  break;

	case '!':
	  if (final_sequence != 0 && set_nomacro++ == 0)
	    fputs ("\n\t.set\tnomacro", file);
	  break;

	case '#':
	  if (set_noreorder != 0)
	    fputs ("\n\tnop", file);
	  else if (TARGET_STATS)
	    fputs ("\n\t#nop", file);

	  break;

	case '(':
	  if (set_noreorder++ == 0)
	    fputs (".set\tnoreorder\n\t", file);
	  break;

	case ')':
	  if (set_noreorder == 0)
	    error ("internal error: %%) found without a %%( in assembler pattern");

	  else if (--set_noreorder == 0)
	    fputs ("\n\t.set\treorder", file);

	  break;

	case '[':
	  if (set_noat++ == 0)
	    fputs (".set\tnoat\n\t", file);
	  break;

	case ']':
	  if (set_noat == 0)
	    error ("internal error: %%] found without a %%[ in assembler pattern");
	  else if (--set_noat == 0)
	    fputs ("\n\t.set\tat", file);

	  break;

	case '<':
	  if (set_nomacro++ == 0)
	    fputs (".set\tnomacro\n\t", file);
	  break;

	case '>':
	  if (set_nomacro == 0)
	    error ("internal error: %%> found without a %%< in assembler pattern");
	  else if (--set_nomacro == 0)
	    fputs ("\n\t.set\tmacro", file);

	  break;

	case '{':
	  if (set_volatile++ == 0)
	    fprintf (file, "%s.set\tvolatile\n\t", TARGET_MIPS_AS ? "" : "#");
	  break;

	case '}':
	  if (set_volatile == 0)
	    error ("internal error: %%} found without a %%{ in assembler pattern");
	  else if (--set_volatile == 0)
	    fprintf (file, "\n\t%s.set\tnovolatile", (TARGET_MIPS_AS) ? "" : "#");

	  break;

	case '~':
	  {
	    if (align_labels_log > 0)
	      ASM_OUTPUT_ALIGN (file, align_labels_log);
	  }
	break;

	default:
	  error ("PRINT_OPERAND: unknown punctuation '%c'", letter);
	  break;
	}

      return;
    }

  if (! op)
    {
      error ("PRINT_OPERAND null pointer");
      return;
    }

  code = GET_CODE (op);

  if (code == SIGN_EXTEND)
    op = XEXP (op, 0), code = GET_CODE (op);

  if (letter == 'C')
    switch (code)
      {
      case EQ:	fputs ("eq",  file); break;
      case NE:	fputs ("ne",  file); break;
      case GT:	fputs ("gt",  file); break;
      case GE:	fputs ("ge",  file); break;
      case LT:	fputs ("lt",  file); break;
      case LE:	fputs ("le",  file); break;
      case GTU: fputs ("gtu", file); break;
      case GEU: fputs ("geu", file); break;
      case LTU: fputs ("ltu", file); break;
      case LEU: fputs ("leu", file); break;
      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%C");
      }

  else if (letter == 'N')
    switch (code)
      {
      case EQ:	fputs ("ne",  file); break;
      case NE:	fputs ("eq",  file); break;
      case GT:	fputs ("le",  file); break;
      case GE:	fputs ("lt",  file); break;
      case LT:	fputs ("ge",  file); break;
      case LE:	fputs ("gt",  file); break;
      case GTU: fputs ("leu", file); break;
      case GEU: fputs ("ltu", file); break;
      case LTU: fputs ("geu", file); break;
      case LEU: fputs ("gtu", file); break;
      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%N");
      }

  else if (letter == 'F')
    switch (code)
      {
      case EQ: fputs ("c1f", file); break;
      case NE: fputs ("c1t", file); break;
      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%F");
      }

  else if (letter == 'W')
    switch (code)
      {
      case EQ: fputs ("c1t", file); break;
      case NE: fputs ("c1f", file); break;
      default:
	abort_with_insn (op, "PRINT_OPERAND, invalid insn for %%W");
      }

  else if (letter == 'S')
    {
      char buffer[100];

      ASM_GENERATE_INTERNAL_LABEL (buffer, "LS", CODE_LABEL_NUMBER (op));
      assemble_name (file, buffer);
    }

  else if (letter == 'Z')
    {
      register int regnum;

      if (code != REG)
	abort ();

      regnum = REGNO (op);
      if (! ST_REG_P (regnum))
	abort ();

      if (regnum != ST_REG_FIRST)
	fprintf (file, "%s,", reg_names[regnum]);
    }

  else if (code == REG || code == SUBREG)
    {
      register int regnum;

      if (code == REG)
	regnum = REGNO (op);
      else
	regnum = true_regnum (op);

      if ((letter == 'M' && ! WORDS_BIG_ENDIAN)
	  || (letter == 'L' && WORDS_BIG_ENDIAN)
	  || letter == 'D')
	regnum++;

      fprintf (file, "%s", reg_names[regnum]);
    }

  else if (code == MEM)
    {
      if (letter == 'D')
	output_address (plus_constant (XEXP (op, 0), 4));
      else
	output_address (XEXP (op, 0));
    }

  else if (code == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (op)) == MODE_FLOAT)
    {
      char s[60];

      real_to_decimal (s, CONST_DOUBLE_REAL_VALUE (op), sizeof (s), 0, 1);
      fputs (s, file);
    }

  else if (letter == 'x' && GET_CODE (op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_HEX, 0xffff & INTVAL(op));

  else if (letter == 'X' && GET_CODE(op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_HEX, INTVAL (op));

  else if (letter == 'd' && GET_CODE(op) == CONST_INT)
    fprintf (file, HOST_WIDE_INT_PRINT_DEC, (INTVAL(op)));

  else if (letter == 'z' && GET_CODE (op) == CONST_INT && INTVAL (op) == 0)
    fputs (reg_names[GP_REG_FIRST], file);

  else if (letter == 'd' || letter == 'x' || letter == 'X')
    output_operand_lossage ("invalid use of %%d, %%x, or %%X");

  else if (letter == 'B')
    fputs (code == EQ ? "z" : "n", file);
  else if (letter == 'b')
    fputs (code == EQ ? "n" : "z", file);
  else if (letter == 'T')
    fputs (code == EQ ? "f" : "t", file);
  else if (letter == 't')
    fputs (code == EQ ? "t" : "f", file);

  else if (code == CONST && GET_CODE (XEXP (op, 0)) == REG)
    {
      /* This case arises on the mips16; see mips16_gp_pseudo_reg.  */
      print_operand (file, XEXP (op, 0), letter);
    }

  else if (TARGET_MIPS16 && code == CONST && mips16_gp_offset_p (op))
    {
      fputs ("%gprel(", file);
      mips16_output_gp_offset (file, op);
      fputs (")", file);
    }

  else
    output_addr_const (file, op);
}

/* A C compound statement to output to stdio stream STREAM the
   assembler syntax for an instruction operand that is a memory
   reference whose address is ADDR.  ADDR is an RTL expression.  */

void
print_operand_address (file, addr)
     FILE *file;
     rtx addr;
{
  if (!addr)
    error ("PRINT_OPERAND_ADDRESS, null pointer");

  else
    switch (GET_CODE (addr))
      {
      case REG:
	if (! TARGET_MIPS16 && REGNO (addr) == ARG_POINTER_REGNUM)
	  abort_with_insn (addr, "arg pointer not eliminated");

	fprintf (file, "0(%s)", reg_names [REGNO (addr)]);
	break;

      case LO_SUM:
	{
	  register rtx arg0 = XEXP (addr, 0);
	  register rtx arg1 = XEXP (addr, 1);

	  if (! mips_split_addresses)
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, Spurious LO_SUM");

	  if (GET_CODE (arg0) != REG)
	    abort_with_insn (addr,
			     "PRINT_OPERAND_ADDRESS, LO_SUM with #1 not REG");

	  fprintf (file, "%%lo(");
	  print_operand_address (file, arg1);
	  fprintf (file, ")(%s)", reg_names [REGNO (arg0)]);
	}
	break;

      case PLUS:
	{
	  register rtx reg = 0;
	  register rtx offset = 0;
	  register rtx arg0 = XEXP (addr, 0);
	  register rtx arg1 = XEXP (addr, 1);

	  if (GET_CODE (arg0) == REG)
	    {
	      reg = arg0;
	      offset = arg1;
	      if (GET_CODE (offset) == REG)
		abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, 2 regs");
	    }

	  else if (GET_CODE (arg1) == REG)
	      reg = arg1, offset = arg0;
	  else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
	    {
	      output_addr_const (file, addr);
	      break;
	    }
	  else
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, no regs");

	  if (! CONSTANT_P (offset))
	    abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #2");

	  if (REGNO (reg) == ARG_POINTER_REGNUM)
	    abort_with_insn (addr, "arg pointer not eliminated");

	  if (TARGET_MIPS16
	      && GET_CODE (offset) == CONST
	      && mips16_gp_offset_p (offset))
	    {
	      fputs ("%gprel(", file);
	      mips16_output_gp_offset (file, offset);
	      fputs (")", file);
	    }
	  else
	    output_addr_const (file, offset);
	  fprintf (file, "(%s)", reg_names [REGNO (reg)]);
	}
	break;

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST_INT:
      case CONST:
	output_addr_const (file, addr);
	break;

      default:
	abort_with_insn (addr, "PRINT_OPERAND_ADDRESS, invalid insn #1");
	break;
    }
}

/* Target hook for assembling integer objects.  It appears that the Irix
   6 assembler can't handle 64-bit decimal integers, so avoid printing
   such an integer here.  */

static bool
mips_assemble_integer (x, size, aligned_p)
     rtx x;
     unsigned int size;
     int aligned_p;
{
  if ((TARGET_64BIT || TARGET_GAS) && size == 8 && aligned_p)
    {
      fputs ("\t.dword\t", asm_out_file);
      if (HOST_BITS_PER_WIDE_INT < 64 || GET_CODE (x) != CONST_INT)
	output_addr_const (asm_out_file, x);
      else
	print_operand (asm_out_file, x, 'X');
      fputc ('\n', asm_out_file);
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* If optimizing for the global pointer, keep track of all of the externs, so
   that at the end of the file, we can emit the appropriate .extern
   declaration for them, before writing out the text section.  We assume all
   names passed to us are in the permanent obstack, so they will be valid at
   the end of the compilation.

   If we have -G 0, or the extern size is unknown, or the object is in a user
   specified section that is not .sbss/.sdata, don't bother emitting the
   .externs.  In the case of user specified sections this behavior is
   required as otherwise GAS will think the object lives in .sbss/.sdata.  */

int
mips_output_external (file, decl, name)
     FILE *file ATTRIBUTE_UNUSED;
     tree decl;
     const char *name;
{
  register struct extern_list *p;
  int len;
  tree section_name;

  if (TARGET_GP_OPT
      && TREE_CODE (decl) != FUNCTION_DECL
      && !DECL_COMDAT (decl)
      && (len = int_size_in_bytes (TREE_TYPE (decl))) > 0
      && ((section_name = DECL_SECTION_NAME (decl)) == NULL
	  || strcmp (TREE_STRING_POINTER (section_name), ".sbss") == 0
	  || strcmp (TREE_STRING_POINTER (section_name), ".sdata") == 0))
    {
      p = (struct extern_list *) xmalloc (sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = len;
      extern_head = p;
    }

#ifdef ASM_OUTPUT_UNDEF_FUNCTION
  if (TREE_CODE (decl) == FUNCTION_DECL
      /* ??? Don't include alloca, since gcc will always expand it
	 inline.  If we don't do this, the C++ library fails to build.  */
      && strcmp (name, "alloca")
      /* ??? Don't include __builtin_next_arg, because then gcc will not
	 bootstrap under Irix 5.1.  */
      && strcmp (name, "__builtin_next_arg"))
    {
      p = (struct extern_list *) xmalloc (sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = -1;
      extern_head = p;
    }
#endif

  return 0;
}

#ifdef ASM_OUTPUT_UNDEF_FUNCTION
int
mips_output_external_libcall (file, name)
     FILE *file ATTRIBUTE_UNUSED;
     const char *name;
{
  register struct extern_list *p;

  p = (struct extern_list *) xmalloc (sizeof (struct extern_list));
  p->next = extern_head;
  p->name = name;
  p->size = -1;
  extern_head = p;

  return 0;
}
#endif

/* Emit a new filename to a stream.  If this is MIPS ECOFF, watch out
   for .file's that start within a function.  If we are smuggling stabs, try to
   put out a MIPS ECOFF file and a stab.  */

void
mips_output_filename (stream, name)
     FILE *stream;
     const char *name;
{
  static int first_time = 1;
  char ltext_label_name[100];

  /* If we are emitting DWARF-2, let dwarf2out handle the ".file"
     directives.  */
  if (write_symbols == DWARF2_DEBUG)
    return;
  else if (first_time)
    {
      first_time = 0;
      SET_FILE_NUMBER ();
      current_function_file = name;
      ASM_OUTPUT_FILENAME (stream, num_source_filenames, name);
      /* This tells mips-tfile that stabs will follow.  */
      if (!TARGET_GAS && write_symbols == DBX_DEBUG)
	fprintf (stream, "\t#@stabs\n");
    }

  else if (write_symbols == DBX_DEBUG)
    {
      ASM_GENERATE_INTERNAL_LABEL (ltext_label_name, "Ltext", 0);
      fprintf (stream, "%s", ASM_STABS_OP);
      output_quoted_string (stream, name);
      fprintf (stream, ",%d,0,0,%s\n", N_SOL, &ltext_label_name[1]);
    }

  else if (name != current_function_file
      && strcmp (name, current_function_file) != 0)
    {
      if (inside_function && !TARGET_GAS)
	{
	  if (!file_in_function_warning)
	    {
	      file_in_function_warning = 1;
	      ignore_line_number = 1;
	      warning ("MIPS ECOFF format does not allow changing filenames within functions with #line");
	    }
	}
      else
	{
	  SET_FILE_NUMBER ();
	  current_function_file = name;
	  ASM_OUTPUT_FILENAME (stream, num_source_filenames, name);
	}
    }
}

/* Emit a linenumber.  For encapsulated stabs, we need to put out a stab
   as well as a .loc, since it is possible that MIPS ECOFF might not be
   able to represent the location for inlines that come from a different
   file.  */

void
mips_output_lineno (stream, line)
     FILE *stream;
     int line;
{
  if (write_symbols == DBX_DEBUG)
    {
      ++sym_lineno;
      fprintf (stream, "%sLM%d:\n%s%d,0,%d,%sLM%d\n",
	       LOCAL_LABEL_PREFIX, sym_lineno, ASM_STABN_OP, N_SLINE, line,
	       LOCAL_LABEL_PREFIX, sym_lineno);
    }
  else
    {
      fprintf (stream, "\n\t%s.loc\t%d %d\n",
	       (ignore_line_number) ? "#" : "",
	       num_source_filenames, line);

      LABEL_AFTER_LOC (stream);
    }
}

/* Output an ASCII string, in a space-saving way.  */

void
mips_output_ascii (stream, string_param, len)
     FILE *stream;
     const char *string_param;
     size_t len;
{
  size_t i;
  int cur_pos = 17;
  register const unsigned char *string =
    (const unsigned char *)string_param;

  fprintf (stream, "\t.ascii\t\"");
  for (i = 0; i < len; i++)
    {
      register int c = string[i];

      switch (c)
	{
	case '\"':
	case '\\':
	  putc ('\\', stream);
	  putc (c, stream);
	  cur_pos += 2;
	  break;

	case TARGET_NEWLINE:
	  fputs ("\\n", stream);
	  if (i+1 < len
	      && (((c = string[i+1]) >= '\040' && c <= '~')
		  || c == TARGET_TAB))
	    cur_pos = 32767;		/* break right here */
	  else
	    cur_pos += 2;
	  break;

	case TARGET_TAB:
	  fputs ("\\t", stream);
	  cur_pos += 2;
	  break;

	case TARGET_FF:
	  fputs ("\\f", stream);
	  cur_pos += 2;
	  break;

	case TARGET_BS:
	  fputs ("\\b", stream);
	  cur_pos += 2;
	  break;

	case TARGET_CR:
	  fputs ("\\r", stream);
	  cur_pos += 2;
	  break;

	default:
	  if (c >= ' ' && c < 0177)
	    {
	      putc (c, stream);
	      cur_pos++;
	    }
	  else
	    {
	      fprintf (stream, "\\%03o", c);
	      cur_pos += 4;
	    }
	}

      if (cur_pos > 72 && i+1 < len)
	{
	  cur_pos = 17;
	  fprintf (stream, "\"\n\t.ascii\t\"");
	}
    }
  fprintf (stream, "\"\n");
}

/* If defined, a C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so they will be
   output differently.

   Here the argument OPVEC is the vector containing the operands extracted
   from INSN, and NOPERANDS is the number of elements of the vector which
   contain meaningful data for this insn.  The contents of this vector are
   what will be used to convert the insn template into assembler code, so you
   can change the assembler output by changing the contents of the vector.

   We use it to check if the current insn needs a nop in front of it because
   of load delays, and also to update the delay slot statistics.  */

/* ??? There is no real need for this function, because it never actually
   emits a NOP anymore.  */

void
final_prescan_insn (insn, opvec, noperands)
     rtx insn;
     rtx opvec[] ATTRIBUTE_UNUSED;
     int noperands ATTRIBUTE_UNUSED;
{
  if (dslots_number_nops > 0)
    {
      rtx pattern = PATTERN (insn);
      int length = get_attr_length (insn);

      /* Do we need to emit a NOP? */
      if (length == 0
	  || (mips_load_reg != 0 && reg_mentioned_p (mips_load_reg,  pattern))
	  || (mips_load_reg2 != 0 && reg_mentioned_p (mips_load_reg2, pattern))
	  || (mips_load_reg3 != 0 && reg_mentioned_p (mips_load_reg3, pattern))
	  || (mips_load_reg4 != 0
	      && reg_mentioned_p (mips_load_reg4, pattern)))
	fputs ("\t#nop\n", asm_out_file);

      else
	dslots_load_filled++;

      while (--dslots_number_nops > 0)
	fputs ("\t#nop\n", asm_out_file);

      mips_load_reg = 0;
      mips_load_reg2 = 0;
      mips_load_reg3 = 0;
      mips_load_reg4 = 0;
    }

  if (TARGET_STATS
      && (GET_CODE (insn) == JUMP_INSN || GET_CODE (insn) == CALL_INSN))
    dslots_jump_total++;
}

/* Output at beginning of assembler file.

   If we are optimizing to use the global pointer, create a temporary file to
   hold all of the text stuff, and write it out to the end. This is needed
   because the MIPS assembler is evidently one pass, and if it hasn't seen the
   relevant .comm/.lcomm/.extern/.sdata declaration when the code is
   processed, it generates a two instruction sequence.  */

void
mips_asm_file_start (stream)
     FILE *stream;
{
  ASM_OUTPUT_SOURCE_FILENAME (stream, main_input_filename);

  /* Versions of the MIPS assembler before 2.20 generate errors if a branch
     inside of a .set noreorder section jumps to a label outside of the .set
     noreorder section.  Revision 2.20 just set nobopt silently rather than
     fixing the bug.  */

  if (TARGET_MIPS_AS && optimize && flag_delayed_branch)
    fprintf (stream, "\t.set\tnobopt\n");

  if (TARGET_GAS)
    {
#if defined(OBJECT_FORMAT_ELF) && !(TARGET_IRIX5 || TARGET_IRIX6)
      /* Generate a special section to describe the ABI switches used to
	 produce the resultant binary.  This used to be done by the assembler
	 setting bits in the ELF header's flags field, but we have run out of
	 bits.  GDB needs this information in order to be able to correctly
	 debug these binaries.  See the function mips_gdbarch_init() in
	 gdb/mips-tdep.c.  This is unnecessary for the IRIX 5/6 ABIs and
	 causes unnecessary IRIX 6 ld warnings.  */
      const char * abi_string = NULL;

      switch (mips_abi)
	{
	case ABI_32:   abi_string = "abi32"; break;
	case ABI_N32:  abi_string = "abiN32"; break;
	case ABI_64:   abi_string = "abi64"; break;
	case ABI_O64:  abi_string = "abiO64"; break;
	case ABI_EABI: abi_string = TARGET_64BIT ? "eabi64" : "eabi32"; break;
	case ABI_MEABI:abi_string = TARGET_64BIT ? "meabi64" : "meabi32"; break;
	default:
	  abort ();
	}
      /* Note - we use fprintf directly rather than called named_section()
	 because in this way we can avoid creating an allocated section.  We
	 do not want this section to take up any space in the running
	 executable.  */
      fprintf (stream, "\t.section .mdebug.%s\n", abi_string);

      /* Restore the default section.  */
      fprintf (stream, "\t.previous\n");
#endif
    }



  /* Generate the pseudo ops that System V.4 wants.  */
#ifndef ABICALLS_ASM_OP
#define ABICALLS_ASM_OP "\t.abicalls"
#endif
  if (TARGET_ABICALLS)
    /* ??? but do not want this (or want pic0) if -non-shared? */
    fprintf (stream, "%s\n", ABICALLS_ASM_OP);

  if (TARGET_MIPS16)
    fprintf (stream, "\t.set\tmips16\n");

  /* This code exists so that we can put all externs before all symbol
     references.  This is necessary for the MIPS assembler's global pointer
     optimizations to work.  */
  if (TARGET_FILE_SWITCHING)
    {
      asm_out_data_file = stream;
      asm_out_text_file = tmpfile ();
    }
  else
    asm_out_data_file = asm_out_text_file = stream;

  if (flag_verbose_asm)
    fprintf (stream, "\n%s -G value = %d, Arch = %s, ISA = %d\n",
	     ASM_COMMENT_START,
	     mips_section_threshold, mips_arch_info->name, mips_isa);
}

/* If we are optimizing the global pointer, emit the text section now and any
   small externs which did not have .comm, etc that are needed.  Also, give a
   warning if the data area is more than 32K and -pic because 3 instructions
   are needed to reference the data pointers.  */

void
mips_asm_file_end (file)
     FILE *file;
{
  tree name_tree;
  struct extern_list *p;

  if (extern_head)
    {
      fputs ("\n", file);

      for (p = extern_head; p != 0; p = p->next)
	{
	  name_tree = get_identifier (p->name);

	  /* Positively ensure only one .extern for any given symbol.  */
	  if (! TREE_ASM_WRITTEN (name_tree))
	    {
	      TREE_ASM_WRITTEN (name_tree) = 1;
#ifdef ASM_OUTPUT_UNDEF_FUNCTION
	      if (p->size == -1)
		ASM_OUTPUT_UNDEF_FUNCTION (file, p->name);
	      else
#endif
		{
		  fputs ("\t.extern\t", file);
		  assemble_name (file, p->name);
		  fprintf (file, ", %d\n", p->size);
		}
	    }
	}
    }

  if (TARGET_FILE_SWITCHING)
    {
      fprintf (file, "\n\t.text\n");
      copy_file_data (file, asm_out_text_file);
    }
}

static void
copy_file_data (to, from)
     FILE *to, *from;
{
  char buffer[8192];
  size_t len;
  rewind (from);
  if (ferror (from))
    fatal_io_error ("can't rewind temp file");

  while ((len = fread (buffer, 1, sizeof (buffer), from)) > 0)
    if (fwrite (buffer, 1, len, to) != len)
      fatal_io_error ("can't write to output file");

  if (ferror (from))
    fatal_io_error ("can't read from temp file");

  if (fclose (from))
    fatal_io_error ("can't close temp file");
}

/* Implement ASM_OUTPUT_ALIGNED_DECL_COMMON.  This is usually the same as
   the elfos.h version, but we also need to handle -muninit-const-in-rodata
   and the limitations of the SGI o32 assembler.  */

void
mips_output_aligned_decl_common (stream, decl, name, size, align)
     FILE *stream;
     tree decl;
     const char *name;
     unsigned HOST_WIDE_INT size;
     unsigned int align;
{
  const char *format;

  /* If the target wants uninitialized const declarations in
     .rdata then don't put them in .comm.   */
  if (TARGET_EMBEDDED_DATA && TARGET_UNINIT_CONST_IN_RODATA
      && TREE_CODE (decl) == VAR_DECL && TREE_READONLY (decl)
      && (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node))
    {
      if (TREE_PUBLIC (decl) && DECL_NAME (decl))
	targetm.asm_out.globalize_label (stream, name);

      readonly_data_section ();
      ASM_OUTPUT_ALIGN (stream, floor_log2 (align / BITS_PER_UNIT));

      format = ACONCAT ((":\n\t.space\t", HOST_WIDE_INT_PRINT_UNSIGNED,
			 "\n", NULL));
      mips_declare_object (stream, name, "", format, size);
    }
#ifdef TARGET_IRIX6
    /* The SGI o32 assembler doesn't accept an alignment, so round up
       the size instead.  */
  else if (mips_abi == ABI_32 && !TARGET_GAS)
    {
      size += (align / BITS_PER_UNIT) - 1;
      size -= size % (align / BITS_PER_UNIT);
      format = ACONCAT ((",", HOST_WIDE_INT_PRINT_UNSIGNED, "\n", NULL));
      mips_declare_object (stream, name, "\n\t.comm\t", format, size);
    }
#endif
  else
    {
      format = ACONCAT ((",", HOST_WIDE_INT_PRINT_UNSIGNED, ",%u\n", NULL));
      mips_declare_object (stream, name, "\n\t.comm\t", format,
			   size, align / BITS_PER_UNIT);
    }
}

/* Emit either a label, .comm, or .lcomm directive.  When using assembler
   macros, mark the symbol as written so that mips_file_end won't emit an
   .extern for it.  STREAM is the output file, NAME is the name of the
   symbol, INIT_STRING is the string that should be written before the
   symbol and FINAL_STRING is the string that shoulbe written after it.
   FINAL_STRING is a printf() format that consumes the remaining arguments.  */

void
mips_declare_object VPARAMS ((FILE *stream, const char *name,
			      const char *init_string,
			      const char *final_string, ...))
{
  VA_OPEN (ap, final_string);
  VA_FIXEDARG (ap, FILE *, stream);
  VA_FIXEDARG (ap, const char *, name);
  VA_FIXEDARG (ap, const char *, init_string);
  VA_FIXEDARG (ap, const char *, final_string);

  fputs (init_string, stream);
  assemble_name (stream, name);
  vfprintf (stream, final_string, ap);

  if (TARGET_GP_OPT)
    {
      tree name_tree = get_identifier (name);
      TREE_ASM_WRITTEN (name_tree) = 1;
    }

  VA_CLOSE (ap);
}

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.

   Mips stack frames look like:

             Before call		        After call
        +-----------------------+	+-----------------------+
   high |			|       |      			|
   mem. |		        |	|			|
        |  caller's temps.    	|       |  caller's temps.    	|
	|       		|       |       	        |
        +-----------------------+	+-----------------------+
 	|       		|	|		        |
        |  arguments on stack.  |	|  arguments on stack.  |
	|       		|	|			|
        +-----------------------+	+-----------------------+
 	|  4 words to save     	|	|  4 words to save	|
	|  arguments passed	|	|  arguments passed	|
	|  in registers, even	|	|  in registers, even	|
    SP->|  if not passed.       |  VFP->|  if not passed.	|
	+-----------------------+       +-----------------------+
					|		        |
                                        |  fp register save     |
					|			|
					+-----------------------+
					|		        |
                                        |  gp register save     |
                                        |       		|
					+-----------------------+
					|			|
					|  local variables	|
					|			|
					+-----------------------+
					|			|
                                        |  alloca allocations   |
        				|			|
					+-----------------------+
					|			|
					|  GP save for V.4 abi	|
					|			|
					+-----------------------+
					|			|
                                        |  arguments on stack   |
        				|		        |
					+-----------------------+
                                        |  4 words to save      |
					|  arguments passed     |
                                        |  in registers, even   |
   low                              SP->|  if not passed.       |
   memory        			+-----------------------+

*/

HOST_WIDE_INT
compute_frame_size (size)
     HOST_WIDE_INT size;	/* # of var. bytes allocated */
{
  unsigned int regno;
  HOST_WIDE_INT total_size;	/* # bytes that the entire frame takes up */
  HOST_WIDE_INT var_size;	/* # bytes that variables take up */
  HOST_WIDE_INT args_size;	/* # bytes that outgoing arguments take up */
  HOST_WIDE_INT extra_size;	/* # extra bytes */
  HOST_WIDE_INT gp_reg_rounded;	/* # bytes needed to store gp after rounding */
  HOST_WIDE_INT gp_reg_size;	/* # bytes needed to store gp regs */
  HOST_WIDE_INT fp_reg_size;	/* # bytes needed to store fp regs */
  long mask;			/* mask of saved gp registers */
  long fmask;			/* mask of saved fp registers */
  tree return_type;

  gp_reg_size = 0;
  fp_reg_size = 0;
  mask = 0;
  fmask	= 0;
  extra_size = MIPS_STACK_ALIGN (((TARGET_ABICALLS) ? UNITS_PER_WORD : 0));
  var_size = MIPS_STACK_ALIGN (size);
  args_size = MIPS_STACK_ALIGN (current_function_outgoing_args_size);

  /* The MIPS 3.0 linker does not like functions that dynamically
     allocate the stack and have 0 for STACK_DYNAMIC_OFFSET, since it
     looks like we are trying to create a second frame pointer to the
     function, so allocate some stack space to make it happy.  */

  if (args_size == 0 && current_function_calls_alloca)
    args_size = 4 * UNITS_PER_WORD;

  total_size = var_size + args_size + extra_size;
  return_type = DECL_RESULT (current_function_decl);

  /* Calculate space needed for gp registers.  */
  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    {
      /* $18 is a special case on the mips16.  It may be used to call
         a function which returns a floating point value, but it is
         marked in call_used_regs.  $31 is also a special case.  When
         not using -mentry, it will be used to copy a return value
         into the floating point registers if the return value is
         floating point.  */
      if (MUST_SAVE_REGISTER (regno)
	  || (TARGET_MIPS16
	      && regno == GP_REG_FIRST + 18
	      && regs_ever_live[regno])
	  || (TARGET_MIPS16
	      && regno == GP_REG_FIRST + 31
	      && mips16_hard_float
	      && ! mips_entry
	      && ! aggregate_value_p (return_type)
	      && GET_MODE_CLASS (DECL_MODE (return_type)) == MODE_FLOAT
	      && GET_MODE_SIZE (DECL_MODE (return_type)) <= UNITS_PER_FPVALUE))
	{
	  gp_reg_size += GET_MODE_SIZE (gpr_mode);
	  mask |= 1L << (regno - GP_REG_FIRST);

	  /* The entry and exit pseudo instructions can not save $17
	     without also saving $16.  */
	  if (mips_entry
	      && regno == GP_REG_FIRST + 17
	      && ! MUST_SAVE_REGISTER (GP_REG_FIRST + 16))
	    {
	      gp_reg_size += UNITS_PER_WORD;
	      mask |= 1L << 16;
	    }
	}
    }

  /* We need to restore these for the handler.  */
  if (current_function_calls_eh_return)
    {
      unsigned int i;
      for (i = 0; ; ++i)
	{
	  regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;
	  gp_reg_size += GET_MODE_SIZE (gpr_mode);
	  mask |= 1L << (regno - GP_REG_FIRST);
	}
    }

  /* This loop must iterate over the same space as its companion in
     save_restore_insns.  */
  for (regno = (FP_REG_LAST - FP_INC + 1);
       regno >= FP_REG_FIRST;
       regno -= FP_INC)
    {
      if (regs_ever_live[regno] && !call_used_regs[regno])
	{
	  fp_reg_size += FP_INC * UNITS_PER_FPREG;
	  fmask |= ((1 << FP_INC) - 1) << (regno - FP_REG_FIRST);
	}
    }

  gp_reg_rounded = MIPS_STACK_ALIGN (gp_reg_size);
  total_size += gp_reg_rounded + MIPS_STACK_ALIGN (fp_reg_size);

  /* The gp reg is caller saved in the 32 bit ABI, so there is no need
     for leaf routines (total_size == extra_size) to save the gp reg.
     The gp reg is callee saved in the 64 bit ABI, so all routines must
     save the gp reg.  This is not a leaf routine if -p, because of the
     call to mcount.  */
  if (total_size == extra_size
      && (mips_abi == ABI_32 || mips_abi == ABI_O64 || mips_abi == ABI_EABI)
      && ! current_function_profile)
    total_size = extra_size = 0;
  else if (TARGET_ABICALLS)
    {
      /* Add the context-pointer to the saved registers.  */
      gp_reg_size += UNITS_PER_WORD;
      mask |= 1L << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST);
      total_size -= gp_reg_rounded;
      gp_reg_rounded = MIPS_STACK_ALIGN (gp_reg_size);
      total_size += gp_reg_rounded;
    }

  /* Add in space reserved on the stack by the callee for storing arguments
     passed in registers.  */
  if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    total_size += MIPS_STACK_ALIGN (current_function_pretend_args_size);

  /* The entry pseudo instruction will allocate 32 bytes on the stack.  */
  if (mips_entry && total_size > 0 && total_size < 32)
    total_size = 32;

  /* Save other computed information.  */
  cfun->machine->frame.total_size = total_size;
  cfun->machine->frame.var_size = var_size;
  cfun->machine->frame.args_size = args_size;
  cfun->machine->frame.extra_size = extra_size;
  cfun->machine->frame.gp_reg_size = gp_reg_size;
  cfun->machine->frame.fp_reg_size = fp_reg_size;
  cfun->machine->frame.mask = mask;
  cfun->machine->frame.fmask = fmask;
  cfun->machine->frame.initialized = reload_completed;
  cfun->machine->frame.num_gp = gp_reg_size / UNITS_PER_WORD;
  cfun->machine->frame.num_fp = fp_reg_size / (FP_INC * UNITS_PER_FPREG);

  if (mask)
    {
      unsigned long offset;

      /* When using mips_entry, the registers are always saved at the
         top of the stack.  */
      if (! mips_entry)
	offset = (args_size + extra_size + var_size
		  + gp_reg_size - GET_MODE_SIZE (gpr_mode));
      else
	offset = total_size - GET_MODE_SIZE (gpr_mode);

      cfun->machine->frame.gp_sp_offset = offset;
      cfun->machine->frame.gp_save_offset = offset - total_size;
    }
  else
    {
      cfun->machine->frame.gp_sp_offset = 0;
      cfun->machine->frame.gp_save_offset = 0;
    }

  if (fmask)
    {
      unsigned long offset = (args_size + extra_size + var_size
			      + gp_reg_rounded + fp_reg_size
			      - FP_INC * UNITS_PER_FPREG);
      cfun->machine->frame.fp_sp_offset = offset;
      cfun->machine->frame.fp_save_offset = offset - total_size;
    }
  else
    {
      cfun->machine->frame.fp_sp_offset = 0;
      cfun->machine->frame.fp_save_offset = 0;
    }

  /* Ok, we're done.  */
  return total_size;
}

/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame
   pointer, argument pointer, or return address pointer.  TO is either
   the stack pointer or hard frame pointer.  */

int
mips_initial_elimination_offset (from, to)
     int from, to;
{
  int offset;

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = 0;
      break;

    case ARG_POINTER_REGNUM:
      compute_frame_size (get_frame_size ());
      offset = cfun->machine->frame.total_size;
      if (mips_abi == ABI_N32 || mips_abi == ABI_64 || mips_abi == ABI_MEABI)
	offset -= current_function_pretend_args_size;
      break;

    case RETURN_ADDRESS_POINTER_REGNUM:
      compute_frame_size (get_frame_size ());
      offset = cfun->machine->frame.gp_sp_offset;
      if (BYTES_BIG_ENDIAN)
	offset += UNITS_PER_WORD - (POINTER_SIZE / BITS_PER_UNIT);
      break;

    default:
      abort ();
    }

  if (TARGET_MIPS16 && to == HARD_FRAME_POINTER_REGNUM)
    offset -= current_function_outgoing_args_size;

  return offset;
}

/* Common code to emit the insns (or to write the instructions to a file)
   to save/restore registers.

   Other parts of the code assume that MIPS_TEMP1_REGNUM (aka large_reg)
   is not modified within save_restore_insns.  */

#define BITSET_P(VALUE,BIT) (((VALUE) & (1L << (BIT))) != 0)

/* Emit instructions to load the value (SP + OFFSET) into MIPS_TEMP2_REGNUM
   and return an rtl expression for the register.

   This function is a subroutine of save_restore_insns.  It is used when
   OFFSET is too large to add in a single instruction.  */

static rtx
mips_add_large_offset_to_sp (offset)
     HOST_WIDE_INT offset;
{
  rtx reg = gen_rtx_REG (Pmode, MIPS_TEMP2_REGNUM);
  rtx offset_rtx = GEN_INT (offset);

  emit_move_insn (reg, offset_rtx);
  if (Pmode == DImode)
    emit_insn (gen_adddi3 (reg, reg, stack_pointer_rtx));
  else
    emit_insn (gen_addsi3 (reg, reg, stack_pointer_rtx));
  return reg;
}

/* Make INSN frame related and note that it performs the frame-related
   operation DWARF_PATTERN.  */

static void
mips_annotate_frame_insn (insn, dwarf_pattern)
     rtx insn, dwarf_pattern;
{
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = alloc_EXPR_LIST (REG_FRAME_RELATED_EXPR,
				      dwarf_pattern,
				      REG_NOTES (insn));
}

/* Return a frame-related rtx that stores register REGNO at (SP + OFFSET).
   The expression should only be used to store single registers.  */

static rtx
mips_frame_set (mode, regno, offset)
     enum machine_mode mode;
     int regno;
     int offset;
{
  rtx address = plus_constant (stack_pointer_rtx, offset);
  rtx set = gen_rtx_SET (mode,
			 gen_rtx_MEM (mode, address),
			 gen_rtx_REG (mode, regno));
  RTX_FRAME_RELATED_P (set) = 1;
  return set;
}


/* Emit a move instruction that stores REG in MEM.  Make the instruction
   frame related and note that it stores REG at (SP + OFFSET).  This
   function may be asked to store an FPR pair.  */

static void
mips_emit_frame_related_store (mem, reg, offset)
     rtx mem;
     rtx reg;
     HOST_WIDE_INT offset;
{
  rtx dwarf_expr;

  if (GET_MODE (reg) == DFmode && ! TARGET_FLOAT64)
    {
      /* Two registers are being stored, so the frame-related expression
	 must be a PARALLEL rtx with one SET for each register.  The
	 higher numbered register is stored in the lower address on
	 big-endian targets.  */
      int regno1 = TARGET_BIG_ENDIAN ? REGNO (reg) + 1 : REGNO (reg);
      int regno2 = TARGET_BIG_ENDIAN ? REGNO (reg) : REGNO (reg) + 1;
      rtx set1 = mips_frame_set (SFmode, regno1, offset);
      rtx set2 = mips_frame_set (SFmode, regno2, offset + UNITS_PER_FPREG);
      dwarf_expr = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set1, set2));
    }
  else
    dwarf_expr = mips_frame_set (GET_MODE (reg), REGNO (reg), offset);

  mips_annotate_frame_insn (emit_move_insn (mem, reg), dwarf_expr);
}

static void
save_restore_insns (store_p, large_reg, large_offset)
     int store_p;	/* true if this is prologue */
     rtx large_reg;	/* register holding large offset constant or NULL */
     long large_offset;	/* large constant offset value */
{
  long mask = cfun->machine->frame.mask;
  long fmask = cfun->machine->frame.fmask;
  long real_mask = mask;
  int regno;
  rtx base_reg_rtx;
  HOST_WIDE_INT base_offset;
  HOST_WIDE_INT gp_offset;
  HOST_WIDE_INT fp_offset;
  HOST_WIDE_INT end_offset;
  rtx insn;

  if (frame_pointer_needed
      && ! BITSET_P (mask, HARD_FRAME_POINTER_REGNUM - GP_REG_FIRST))
    abort ();

  /* Do not restore GP under certain conditions.  */
  if (! store_p
      && TARGET_ABICALLS
      && (mips_abi == ABI_32 || mips_abi == ABI_O64))
    mask &= ~(1L << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST));

  if (mask == 0 && fmask == 0)
    return;

  /* Save registers starting from high to low.  The debuggers prefer at least
     the return register be stored at func+4, and also it allows us not to
     need a nop in the epilog if at least one register is reloaded in
     addition to return address.  */

  /* Save GP registers if needed.  */
  if (mask)
    {
      /* Pick which pointer to use as a base register.  For small frames, just
	 use the stack pointer.  Otherwise, use a temporary register.  Save 2
	 cycles if the save area is near the end of a large frame, by reusing
	 the constant created in the prologue/epilogue to adjust the stack
	 frame.  */

      gp_offset = cfun->machine->frame.gp_sp_offset;
      end_offset
	= gp_offset - (cfun->machine->frame.gp_reg_size
		       - GET_MODE_SIZE (gpr_mode));

      if (gp_offset < 0 || end_offset < 0)
	internal_error
	  ("gp_offset (%ld) or end_offset (%ld) is less than zero",
	   (long) gp_offset, (long) end_offset);

      /* If we see a large frame in mips16 mode, we save the registers
         before adjusting the stack pointer, and load them afterward.  */
      else if (TARGET_MIPS16 && large_offset > 32767)
	base_reg_rtx = stack_pointer_rtx, base_offset = large_offset;

      else if (gp_offset < 32768)
	base_reg_rtx = stack_pointer_rtx, base_offset  = 0;

      else if (large_reg != 0
	       && (unsigned HOST_WIDE_INT) (large_offset - gp_offset) < 32768
	       && (unsigned HOST_WIDE_INT) (large_offset - end_offset) < 32768)
	{
	  base_reg_rtx = gen_rtx_REG (Pmode, MIPS_TEMP2_REGNUM);
	  base_offset = large_offset;
	  if (Pmode == DImode)
	    insn = emit_insn (gen_adddi3 (base_reg_rtx, large_reg,
					  stack_pointer_rtx));
	  else
	    insn = emit_insn (gen_addsi3 (base_reg_rtx, large_reg,
					  stack_pointer_rtx));
	}
      else
	{
	  base_offset = gp_offset;
	  base_reg_rtx = mips_add_large_offset_to_sp (base_offset);
	}

      /* When we restore the registers in MIPS16 mode, then if we are
         using a frame pointer, and this is not a large frame, the
         current stack pointer will be offset by
         current_function_outgoing_args_size.  Doing it this way lets
         us avoid offsetting the frame pointer before copying it into
         the stack pointer; there is no instruction to set the stack
         pointer to the sum of a register and a constant.  */
      if (TARGET_MIPS16
	  && ! store_p
	  && frame_pointer_needed
	  && large_offset <= 32767)
	base_offset += current_function_outgoing_args_size;

      for (regno = GP_REG_LAST; regno >= GP_REG_FIRST; regno--)
	{
	  if (BITSET_P (mask, regno - GP_REG_FIRST))
	    {
	      rtx reg_rtx;
	      rtx mem_rtx
		= gen_rtx (MEM, gpr_mode,
			   gen_rtx (PLUS, Pmode, base_reg_rtx,
				    GEN_INT (gp_offset - base_offset)));

	      if (! current_function_calls_eh_return)
		RTX_UNCHANGING_P (mem_rtx) = 1;

	      /* The mips16 does not have an instruction to load
		 $31, so we load $7 instead, and work things out
		 in mips_expand_epilogue.  */
	      if (TARGET_MIPS16 && ! store_p && regno == GP_REG_FIRST + 31)
		reg_rtx = gen_rtx (REG, gpr_mode, GP_REG_FIRST + 7);
	      /* The mips16 sometimes needs to save $18.  */
	      else if (TARGET_MIPS16
		       && regno != GP_REG_FIRST + 31
		       && ! M16_REG_P (regno))
		{
		  if (! store_p)
		    reg_rtx = gen_rtx (REG, gpr_mode, 6);
		  else
		    {
		      reg_rtx = gen_rtx (REG, gpr_mode, 3);
		      emit_move_insn (reg_rtx,
				      gen_rtx (REG, gpr_mode, regno));
		    }
		}
	      else
		reg_rtx = gen_rtx (REG, gpr_mode, regno);

	      if (store_p)
		mips_emit_frame_related_store (mem_rtx, reg_rtx, gp_offset);
	      else
		{
		  emit_move_insn (reg_rtx, mem_rtx);
		  if (TARGET_MIPS16
		      && regno != GP_REG_FIRST + 31
		      && ! M16_REG_P (regno))
		    emit_move_insn (gen_rtx (REG, gpr_mode, regno),
				    reg_rtx);
		}
	    }
	  /* If the restore is being supressed, still take into account
	     the offset at which it is stored.  */
	  if (BITSET_P (real_mask, regno - GP_REG_FIRST))
	    gp_offset -= GET_MODE_SIZE (gpr_mode);
	}
    }
  else
    base_reg_rtx = 0, base_offset  = 0;

  /* Save floating point registers if needed.  */
  if (fmask)
    {
      /* Pick which pointer to use as a base register.  */
      fp_offset = cfun->machine->frame.fp_sp_offset;
      end_offset = fp_offset - (cfun->machine->frame.fp_reg_size
				- UNITS_PER_HWFPVALUE);

      if (fp_offset < 0 || end_offset < 0)
	internal_error
	  ("fp_offset (%ld) or end_offset (%ld) is less than zero",
	   (long) fp_offset, (long) end_offset);

      else if (fp_offset < 32768)
	base_reg_rtx = stack_pointer_rtx, base_offset  = 0;

      else if (base_reg_rtx != 0
	       && (unsigned HOST_WIDE_INT) (base_offset - fp_offset) < 32768
	       && (unsigned HOST_WIDE_INT) (base_offset - end_offset) < 32768)
	;			/* already set up for gp registers above */

      else if (large_reg != 0
	       && (unsigned HOST_WIDE_INT) (large_offset - fp_offset) < 32768
	       && (unsigned HOST_WIDE_INT) (large_offset - end_offset) < 32768)
	{
	  base_reg_rtx = gen_rtx_REG (Pmode, MIPS_TEMP2_REGNUM);
	  base_offset = large_offset;
	  if (Pmode == DImode)
	    insn = emit_insn (gen_adddi3 (base_reg_rtx, large_reg,
					  stack_pointer_rtx));
	  else
	    insn = emit_insn (gen_addsi3 (base_reg_rtx, large_reg,
					  stack_pointer_rtx));
	}
      else
	{
	  base_offset = fp_offset;
	  base_reg_rtx = mips_add_large_offset_to_sp (fp_offset);
	}

      /* This loop must iterate over the same space as its companion in
	 compute_frame_size.  */
      for (regno = (FP_REG_LAST - FP_INC + 1);
	   regno >= FP_REG_FIRST;
	   regno -= FP_INC)
	if (BITSET_P (fmask, regno - FP_REG_FIRST))
	  {
	    enum machine_mode sz = TARGET_SINGLE_FLOAT ? SFmode : DFmode;
	    rtx reg_rtx = gen_rtx (REG, sz, regno);
	    rtx mem_rtx = gen_rtx (MEM, sz,
				   gen_rtx (PLUS, Pmode, base_reg_rtx,
					    GEN_INT (fp_offset
						     - base_offset)));
	    if (! current_function_calls_eh_return)
	      RTX_UNCHANGING_P (mem_rtx) = 1;

	    if (store_p)
	      mips_emit_frame_related_store (mem_rtx, reg_rtx, fp_offset);
	    else
	      emit_move_insn (reg_rtx, mem_rtx);

	    fp_offset -= UNITS_PER_HWFPVALUE;
	  }
    }
}

/* Set up the stack and frame (if desired) for the function.  */

static void
mips_output_function_prologue (file, size)
     FILE *file;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
#ifndef FUNCTION_NAME_ALREADY_DECLARED
  const char *fnname;
#endif
  HOST_WIDE_INT tsize = cfun->machine->frame.total_size;

  /* ??? When is this really needed?  At least the GNU assembler does not
     need the source filename more than once in the file, beyond what is
     emitted by the debug information.  */
  if (!TARGET_GAS)
    ASM_OUTPUT_SOURCE_FILENAME (file, DECL_SOURCE_FILE (current_function_decl));

#ifdef SDB_DEBUGGING_INFO
  if (debug_info_level != DINFO_LEVEL_TERSE && write_symbols == SDB_DEBUG)
    ASM_OUTPUT_SOURCE_LINE (file, DECL_SOURCE_LINE (current_function_decl));
#endif

  /* In mips16 mode, we may need to generate a 32 bit to handle
     floating point arguments.  The linker will arrange for any 32 bit
     functions to call this stub, which will then jump to the 16 bit
     function proper.  */
  if (TARGET_MIPS16 && !TARGET_SOFT_FLOAT
      && current_function_args_info.fp_code != 0)
    build_mips16_function_stub (file);

  inside_function = 1;

#ifndef FUNCTION_NAME_ALREADY_DECLARED
  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.ent\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }

  assemble_name (file, fnname);
  fputs (":\n", file);
#endif

  if (!flag_inhibit_size_directive)
    {
      /* .frame FRAMEREG, FRAMESIZE, RETREG */
      fprintf (file,
	       "\t.frame\t%s,%ld,%s\t\t# vars= %ld, regs= %d/%d, args= %d, extra= %ld\n",
	       (reg_names[(frame_pointer_needed)
			  ? HARD_FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM]),
	       ((frame_pointer_needed && TARGET_MIPS16)
		? ((long) tsize - current_function_outgoing_args_size)
		: (long) tsize),
	       reg_names[GP_REG_FIRST + 31],
	       cfun->machine->frame.var_size,
	       cfun->machine->frame.num_gp,
	       cfun->machine->frame.num_fp,
	       current_function_outgoing_args_size,
	       cfun->machine->frame.extra_size);

      /* .mask MASK, GPOFFSET; .fmask FPOFFSET */
      fprintf (file, "\t.mask\t0x%08lx,%ld\n\t.fmask\t0x%08lx,%ld\n",
	       cfun->machine->frame.mask,
	       cfun->machine->frame.gp_save_offset,
	       cfun->machine->frame.fmask,
	       cfun->machine->frame.fp_save_offset);

      /* Require:
	 OLD_SP == *FRAMEREG + FRAMESIZE => can find old_sp from nominated FP reg.
	 HIGHEST_GP_SAVED == *FRAMEREG + FRAMESIZE + GPOFFSET => can find saved regs.  */
    }

  if (mips_entry && ! mips_can_use_return_insn ())
    {
      int save16 = BITSET_P (cfun->machine->frame.mask, 16);
      int save17 = BITSET_P (cfun->machine->frame.mask, 17);
      int save31 = BITSET_P (cfun->machine->frame.mask, 31);
      int savearg = 0;
      rtx insn;

      /* Look through the initial insns to see if any of them store
	 the function parameters into the incoming parameter storage
	 area.  If they do, we delete the insn, and save the register
	 using the entry pseudo-instruction instead.  We don't try to
	 look past a label, jump, or call.  */
      for (insn = get_insns (); insn != NULL_RTX; insn = NEXT_INSN (insn))
	{
	  rtx note, set, src, dest, base, offset;
	  int hireg;

	  if (GET_CODE (insn) == CODE_LABEL
	      || GET_CODE (insn) == JUMP_INSN
	      || GET_CODE (insn) == CALL_INSN)
	    break;
	  if (GET_CODE (insn) != INSN)
	    continue;
	  set = PATTERN (insn);
	  if (GET_CODE (set) != SET)
	    continue;

	  /* An insn storing a function parameter will still have a
             REG_EQUIV note on it mentioning the argument pointer.  */
	  note = find_reg_note (insn, REG_EQUIV, NULL_RTX);
	  if (note == NULL_RTX)
	    continue;
	  if (! reg_mentioned_p (arg_pointer_rtx, XEXP (note, 0)))
	    continue;

	  src = SET_SRC (set);
	  if (GET_CODE (src) != REG
	      || REGNO (src) < GP_REG_FIRST + 4
	      || REGNO (src) > GP_REG_FIRST + 7)
	    continue;

	  dest = SET_DEST (set);
	  if (GET_CODE (dest) != MEM)
	    continue;
	  if (GET_MODE_SIZE (GET_MODE (dest)) == (unsigned) UNITS_PER_WORD)
	    ;
	  else if (GET_MODE_SIZE (GET_MODE (dest)) == (unsigned)2 * UNITS_PER_WORD
		   && REGNO (src) < GP_REG_FIRST + 7)
	    ;
	  else
	    continue;
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (dest, 0), &offset);
	  if (GET_CODE (base) != REG
	      || GET_CODE (offset) != CONST_INT)
	    continue;
	  if (REGNO (base) == (unsigned) STACK_POINTER_REGNUM
	      && INTVAL (offset) == tsize + (REGNO (src) - 4) * UNITS_PER_WORD)
	    ;
	  else if (REGNO (base) == (unsigned) HARD_FRAME_POINTER_REGNUM
		   && (INTVAL (offset)
		       == (tsize
			   + (REGNO (src) - 4) * UNITS_PER_WORD
			   - current_function_outgoing_args_size)))
	    ;
	  else
	    continue;

	  /* This insn stores a parameter onto the stack, in the same
             location where the entry pseudo-instruction will put it.
             Delete the insn, and arrange to tell the entry
             instruction to save the register.  */
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (insn) = 0;

	  hireg = (REGNO (src)
		   + HARD_REGNO_NREGS (REGNO (src), GET_MODE (dest))
		   - 1);
	  if (hireg > savearg)
	    savearg = hireg;
	}

      /* If this is a varargs function, we need to save all the
         registers onto the stack anyhow.  */
      if (current_function_stdarg)
	savearg = GP_REG_FIRST + 7;

      fprintf (file, "\tentry\t");
      if (savearg > 0)
	{
	  if (savearg == GP_REG_FIRST + 4)
	    fprintf (file, "%s", reg_names[savearg]);
	  else
	    fprintf (file, "%s-%s", reg_names[GP_REG_FIRST + 4],
		     reg_names[savearg]);
	}
      if (save16 || save17)
	{
	  if (savearg > 0)
	    fprintf (file, ",");
	  fprintf (file, "%s", reg_names[GP_REG_FIRST + 16]);
	  if (save17)
	    fprintf (file, "-%s", reg_names[GP_REG_FIRST + 17]);
	}
      if (save31)
	{
	  if (savearg > 0 || save16 || save17)
	    fprintf (file, ",");
	  fprintf (file, "%s", reg_names[GP_REG_FIRST + 31]);
	}
      fprintf (file, "\n");
    }

  if (TARGET_ABICALLS && (mips_abi == ABI_32 || mips_abi == ABI_O64))
    {
      const char *const sp_str = reg_names[STACK_POINTER_REGNUM];

      fprintf (file, "\t.set\tnoreorder\n\t.cpload\t%s\n\t.set\treorder\n",
	       reg_names[PIC_FUNCTION_ADDR_REGNUM]);
      if (tsize > 0)
	{
	  fprintf (file, "\t%s\t%s,%s,%ld\n",
		   (Pmode == DImode ? "dsubu" : "subu"),
		   sp_str, sp_str, (long) tsize);
	  fprintf (file, "\t.cprestore %ld\n", cfun->machine->frame.args_size);
	}

      if (dwarf2out_do_frame ())
	dwarf2out_def_cfa ("", STACK_POINTER_REGNUM, tsize);
    }
}

/* Expand the prologue into a bunch of separate insns.  */

void
mips_expand_prologue ()
{
  int regno;
  HOST_WIDE_INT tsize;
  rtx tmp_rtx = 0;
  int last_arg_is_vararg_marker = 0;
  tree fndecl = current_function_decl;
  tree fntype = TREE_TYPE (fndecl);
  tree fnargs = DECL_ARGUMENTS (fndecl);
  rtx next_arg_reg;
  int i;
  tree next_arg;
  tree cur_arg;
  CUMULATIVE_ARGS args_so_far;
  rtx reg_18_save = NULL_RTX;
  int store_args_on_stack = (mips_abi == ABI_32 || mips_abi == ABI_O64)
                            && (! mips_entry || mips_can_use_return_insn ());

  /* If struct value address is treated as the first argument, make it so.  */
  if (aggregate_value_p (DECL_RESULT (fndecl))
      && ! current_function_returns_pcc_struct
      && struct_value_incoming_rtx == 0)
    {
      tree type = build_pointer_type (fntype);
      tree function_result_decl = build_decl (PARM_DECL, NULL_TREE, type);

      DECL_ARG_TYPE (function_result_decl) = type;
      TREE_CHAIN (function_result_decl) = fnargs;
      fnargs = function_result_decl;
    }

  /* For arguments passed in registers, find the register number
     of the first argument in the variable part of the argument list,
     otherwise GP_ARG_LAST+1.  Note also if the last argument is
     the varargs special argument, and treat it as part of the
     variable arguments.

     This is only needed if store_args_on_stack is true.  */

  INIT_CUMULATIVE_ARGS (args_so_far, fntype, NULL_RTX, 0);
  regno = GP_ARG_FIRST;

  for (cur_arg = fnargs; cur_arg != 0; cur_arg = next_arg)
    {
      tree passed_type = DECL_ARG_TYPE (cur_arg);
      enum machine_mode passed_mode = TYPE_MODE (passed_type);
      rtx entry_parm;

      if (TREE_ADDRESSABLE (passed_type))
	{
	  passed_type = build_pointer_type (passed_type);
	  passed_mode = Pmode;
	}

      entry_parm = FUNCTION_ARG (args_so_far, passed_mode, passed_type, 1);

      FUNCTION_ARG_ADVANCE (args_so_far, passed_mode, passed_type, 1);
      next_arg = TREE_CHAIN (cur_arg);

      if (entry_parm && store_args_on_stack)
	{
	  if (next_arg == 0
	      && DECL_NAME (cur_arg)
	      && ((0 == strcmp (IDENTIFIER_POINTER (DECL_NAME (cur_arg)),
				"__builtin_va_alist"))
		  || (0 == strcmp (IDENTIFIER_POINTER (DECL_NAME (cur_arg)),
				   "va_alist"))))
	    {
	      last_arg_is_vararg_marker = 1;
	      if (GET_CODE (entry_parm) == REG)
		regno = REGNO (entry_parm);
	      else
		regno = GP_ARG_LAST + 1;
	      break;
	    }
	  else
	    regno = GP_ARG_FIRST + args_so_far.num_gprs;
	}
      else
	{
	  regno = GP_ARG_LAST+1;
	  break;
	}
    }

  /* In order to pass small structures by value in registers compatibly with
     the MIPS compiler, we need to shift the value into the high part of the
     register.  Function_arg has encoded a PARALLEL rtx, holding a vector of
     adjustments to be made as the next_arg_reg variable, so we split up the
     insns, and emit them separately.  */

  next_arg_reg = FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1);
  if (next_arg_reg != 0 && GET_CODE (next_arg_reg) == PARALLEL)
    {
      rtvec adjust = XVEC (next_arg_reg, 0);
      int num = GET_NUM_ELEM (adjust);

      for (i = 0; i < num; i++)
	{
	  rtx insn, pattern;

	  pattern = RTVEC_ELT (adjust, i);
	  if (GET_CODE (pattern) != SET
	      || GET_CODE (SET_SRC (pattern)) != ASHIFT)
	    abort_with_insn (pattern, "insn is not a shift");
	  PUT_CODE (SET_SRC (pattern), ASHIFTRT);

	  insn = emit_insn (pattern);

	  /* Global life information isn't valid at this point, so we
	     can't check whether these shifts are actually used.  Mark
	     them MAYBE_DEAD so that flow2 will remove them, and not
	     complain about dead code in the prologue.  */
	  REG_NOTES(insn) = gen_rtx_EXPR_LIST (REG_MAYBE_DEAD, NULL_RTX,
					       REG_NOTES (insn));
	}
    }

  tsize = compute_frame_size (get_frame_size ());

  /* If this function is a varargs function, store any registers that
     would normally hold arguments ($4 - $7) on the stack.  */
  if (store_args_on_stack
      && ((TYPE_ARG_TYPES (fntype) != 0
	   && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	       != void_type_node))
	  || last_arg_is_vararg_marker))
    {
      int offset = (regno - GP_ARG_FIRST) * UNITS_PER_WORD;
      rtx ptr = stack_pointer_rtx;

      /* If we are doing svr4-abi, sp has already been decremented by tsize.  */
      if (TARGET_ABICALLS)
	offset += tsize;

      for (; regno <= GP_ARG_LAST; regno++)
	{
	  if (offset != 0)
	    ptr = gen_rtx (PLUS, Pmode, stack_pointer_rtx, GEN_INT (offset));
	  emit_move_insn (gen_rtx (MEM, gpr_mode, ptr),
			  gen_rtx (REG, gpr_mode, regno));

	  offset += GET_MODE_SIZE (gpr_mode);
	}
    }

  /* If we are using the entry pseudo instruction, it will
     automatically subtract 32 from the stack pointer, so we don't
     need to.  The entry pseudo instruction is emitted by
     function_prologue.  */
  if (mips_entry && ! mips_can_use_return_insn ())
    {
      if (tsize > 0 && tsize <= 32 && frame_pointer_needed)
	{
          rtx insn;

	  /* If we are using a frame pointer with a small stack frame,
             we need to initialize it here since it won't be done
             below.  */
	  if (TARGET_MIPS16 && current_function_outgoing_args_size != 0)
	    {
	      rtx incr = GEN_INT (current_function_outgoing_args_size);
	      if (Pmode == DImode)
		insn = emit_insn (gen_adddi3 (hard_frame_pointer_rtx,
                                              stack_pointer_rtx,
                                              incr));
	      else
		insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
                                              stack_pointer_rtx,
                                              incr));
	    }
	  else if (Pmode == DImode)
	    insn = emit_insn (gen_movdi (hard_frame_pointer_rtx,
					 stack_pointer_rtx));
	  else
	    insn = emit_insn (gen_movsi (hard_frame_pointer_rtx,
					 stack_pointer_rtx));

          RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* We may need to save $18, if it is used to call a function
	 which may return a floating point value.  Set up a sequence
	 of instructions to do so.  Later on we emit them at the right
	 moment.  */
      if (TARGET_MIPS16 && BITSET_P (cfun->machine->frame.mask, 18))
	{
	  rtx reg_rtx = gen_rtx (REG, gpr_mode, GP_REG_FIRST + 3);
	  long gp_offset, base_offset;

	  gp_offset = cfun->machine->frame.gp_sp_offset;
	  if (BITSET_P (cfun->machine->frame.mask, 16))
	    gp_offset -= UNITS_PER_WORD;
	  if (BITSET_P (cfun->machine->frame.mask, 17))
	    gp_offset -= UNITS_PER_WORD;
	  if (BITSET_P (cfun->machine->frame.mask, 31))
	    gp_offset -= UNITS_PER_WORD;
	  if (tsize > 32767)
	    base_offset = tsize;
	  else
	    base_offset = 0;
	  start_sequence ();
	  emit_move_insn (reg_rtx,
			  gen_rtx (REG, gpr_mode, GP_REG_FIRST + 18));
	  emit_move_insn (gen_rtx (MEM, gpr_mode,
				   gen_rtx (PLUS, Pmode, stack_pointer_rtx,
					    GEN_INT (gp_offset
						     - base_offset))),
			  reg_rtx);
	  reg_18_save = get_insns ();
	  end_sequence ();
	}

      if (tsize > 32)
	tsize -= 32;
      else
	{
	  tsize = 0;
	  if (reg_18_save != NULL_RTX)
	    emit_insn (reg_18_save);
	}
    }

  if (tsize > 0)
    {
      rtx tsize_rtx = GEN_INT (tsize);

      /* If we are doing svr4-abi, sp move is done by
         function_prologue.  In mips16 mode with a large frame, we
         save the registers before adjusting the stack.  */
      if ((!TARGET_ABICALLS || (mips_abi != ABI_32 && mips_abi != ABI_O64))
	  && (!TARGET_MIPS16 || tsize <= 32767))
	{
	  rtx adjustment_rtx, insn, dwarf_pattern;

	  if (tsize > 32767)
	    {
	      adjustment_rtx = gen_rtx (REG, Pmode, MIPS_TEMP1_REGNUM);
	      emit_move_insn (adjustment_rtx, tsize_rtx);
	    }
	  else
	    adjustment_rtx = tsize_rtx;

	  if (Pmode == DImode)
	    insn = emit_insn (gen_subdi3 (stack_pointer_rtx, stack_pointer_rtx,
					  adjustment_rtx));
	  else
	    insn = emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx,
					  adjustment_rtx));

	  dwarf_pattern = gen_rtx_SET (Pmode, stack_pointer_rtx,
				       plus_constant (stack_pointer_rtx,
						      -tsize));

	  mips_annotate_frame_insn (insn, dwarf_pattern);
	}

      if (! mips_entry)
	save_restore_insns (1, tmp_rtx, tsize);
      else if (reg_18_save != NULL_RTX)
	emit_insn (reg_18_save);

      if ((!TARGET_ABICALLS || (mips_abi != ABI_32 && mips_abi != ABI_O64))
	  && TARGET_MIPS16
	  && tsize > 32767)
	{
	  rtx reg_rtx;

	  if (!frame_pointer_needed)
	    abort ();

	  reg_rtx = gen_rtx (REG, Pmode, 3);
  	  emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
  	  emit_move_insn (reg_rtx, tsize_rtx);
  	  if (Pmode == DImode)
	    emit_insn (gen_subdi3 (hard_frame_pointer_rtx,
				   hard_frame_pointer_rtx,
				   reg_rtx));
	  else
	    emit_insn (gen_subsi3 (hard_frame_pointer_rtx,
				   hard_frame_pointer_rtx,
				   reg_rtx));
	  emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
	}

      if (frame_pointer_needed)
	{
          rtx insn = 0;

	  /* On the mips16, we encourage the use of unextended
             instructions when using the frame pointer by pointing the
             frame pointer ahead of the argument space allocated on
             the stack.  */
	  if ((! TARGET_ABICALLS || (mips_abi != ABI_32 && mips_abi != ABI_O64))
	      && TARGET_MIPS16
	      && tsize > 32767)
	    {
	      /* In this case, we have already copied the stack
                 pointer into the frame pointer, above.  We need only
                 adjust for the outgoing argument size.  */
	      if (current_function_outgoing_args_size != 0)
		{
		  rtx incr = GEN_INT (current_function_outgoing_args_size);
		  if (Pmode == DImode)
		    insn = emit_insn (gen_adddi3 (hard_frame_pointer_rtx,
                                                  hard_frame_pointer_rtx,
                                                  incr));
		  else
		    insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
                                                  hard_frame_pointer_rtx,
                                                  incr));
		}
	    }
	  else if (TARGET_MIPS16 && current_function_outgoing_args_size != 0)
	    {
	      rtx incr = GEN_INT (current_function_outgoing_args_size);
	      if (Pmode == DImode)
		insn = emit_insn (gen_adddi3 (hard_frame_pointer_rtx,
                                              stack_pointer_rtx,
                                              incr));
	      else
		insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
                                              stack_pointer_rtx,
                                              incr));
	    }
	  else if (Pmode == DImode)
	    insn = emit_insn (gen_movdi (hard_frame_pointer_rtx,
					 stack_pointer_rtx));
	  else
	    insn = emit_insn (gen_movsi (hard_frame_pointer_rtx,
					 stack_pointer_rtx));

	  if (insn)
	    RTX_FRAME_RELATED_P (insn) = 1;
	}

      if (TARGET_ABICALLS && (mips_abi != ABI_32 && mips_abi != ABI_O64))
	emit_insn (gen_loadgp (XEXP (DECL_RTL (current_function_decl), 0),
			       gen_rtx_REG (DImode, 25)));
    }

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */

  if (current_function_profile)
    emit_insn (gen_blockage ());
}

/* Do any necessary cleanup after a function to restore stack, frame,
   and regs.  */

#define RA_MASK BITMASK_HIGH	/* 1 << 31 */
#define PIC_OFFSET_TABLE_MASK (1 << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))

static void
mips_output_function_epilogue (file, size)
     FILE *file ATTRIBUTE_UNUSED;
     HOST_WIDE_INT size ATTRIBUTE_UNUSED;
{
  const char *fnname = "";	/* FIXME: Correct initialisation?  */
  rtx string;

#ifndef FUNCTION_NAME_ALREADY_DECLARED
  /* Get the function name the same way that toplev.c does before calling
     assemble_start_function.  This is needed so that the name used here
     exactly matches the name used in ASM_DECLARE_FUNCTION_NAME.  */
  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

  if (!flag_inhibit_size_directive)
    {
      fputs ("\t.end\t", file);
      assemble_name (file, fnname);
      fputs ("\n", file);
    }
#endif

  if (TARGET_STATS)
    {
      int num_gp_regs = cfun->machine->frame.gp_reg_size / 4;
      int num_fp_regs = cfun->machine->frame.fp_reg_size / 8;
      int num_regs = num_gp_regs + num_fp_regs;
      const char *name = fnname;

      if (name[0] == '*')
	name++;

      dslots_load_total += num_regs;

      fprintf (stderr,
	       "%-20s fp=%c leaf=%c alloca=%c setjmp=%c stack=%4ld arg=%3d reg=%2d/%d delay=%3d/%3dL %3d/%3dJ refs=%3d/%3d/%3d",
	       name, frame_pointer_needed ? 'y' : 'n',
	       (cfun->machine->frame.mask & RA_MASK) != 0 ? 'n' : 'y',
	       current_function_calls_alloca ? 'y' : 'n',
	       current_function_calls_setjmp ? 'y' : 'n',
	       cfun->machine->frame.total_size,
	       current_function_outgoing_args_size, num_gp_regs, num_fp_regs,
	       dslots_load_total, dslots_load_filled,
	       dslots_jump_total, dslots_jump_filled,
	       num_refs[0], num_refs[1], num_refs[2]);

      fputc ('\n', stderr);
    }

  /* Reset state info for each function.  */
  inside_function = 0;
  ignore_line_number = 0;
  dslots_load_total = 0;
  dslots_jump_total = 0;
  dslots_load_filled = 0;
  dslots_jump_filled = 0;
  num_refs[0] = 0;
  num_refs[1] = 0;
  num_refs[2] = 0;
  mips_load_reg = 0;
  mips_load_reg2 = 0;

  while (string_constants != NULL)
    {
      struct string_constant *next;

      next = string_constants->next;
      free (string_constants);
      string_constants = next;
    }

  /* If any following function uses the same strings as this one, force
     them to refer those strings indirectly.  Nearby functions could
     refer them using pc-relative addressing, but it isn't safe in
     general.  For instance, some functions may be placed in sections
     other than .text, and we don't know whether they be close enough
     to this one.  In large files, even other .text functions can be
     too far away.  */
  for (string = mips16_strings; string != 0; string = XEXP (string, 1))
    SYMBOL_REF_FLAG (XEXP (string, 0)) = 0;
  free_EXPR_LIST_list (&mips16_strings);

  /* Restore the output file if optimizing the GP (optimizing the GP causes
     the text to be diverted to a tempfile, so that data decls come before
     references to the data).  */
  if (TARGET_FILE_SWITCHING)
    {
      asm_out_file = asm_out_data_file;
      data_section ();
    }
}

/* Expand the epilogue into a bunch of separate insns.  */

void
mips_expand_epilogue ()
{
  HOST_WIDE_INT tsize = cfun->machine->frame.total_size;
  rtx tsize_rtx = GEN_INT (tsize);
  rtx tmp_rtx = (rtx)0;

  if (mips_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  if (mips_entry && ! mips_can_use_return_insn ())
    tsize -= 32;

  if (tsize > 32767 && ! TARGET_MIPS16)
    {
      tmp_rtx = gen_rtx_REG (Pmode, MIPS_TEMP1_REGNUM);
      emit_move_insn (tmp_rtx, tsize_rtx);
      tsize_rtx = tmp_rtx;
    }

  if (tsize > 0)
    {
      long orig_tsize = tsize;

      if (frame_pointer_needed)
	{
	  emit_insn (gen_blockage ());

	  /* On the mips16, the frame pointer is offset from the stack
             pointer by current_function_outgoing_args_size.  We
             account for that by changing tsize.  Note that this can
             actually make tsize negative.  */
	  if (TARGET_MIPS16)
	    {
	      tsize -= current_function_outgoing_args_size;

	      /* If we have a large frame, it's easier to add to $6
                 than to $sp, since the mips16 has no instruction to
                 add a register to $sp.  */
	      if (orig_tsize > 32767)
		{
		  rtx g6_rtx = gen_rtx (REG, Pmode, GP_REG_FIRST + 6);

		  emit_move_insn (g6_rtx, GEN_INT (tsize));
		  if (Pmode == DImode)
		    emit_insn (gen_adddi3 (hard_frame_pointer_rtx,
					   hard_frame_pointer_rtx,
					   g6_rtx));
		  else
		    emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					   hard_frame_pointer_rtx,
					   g6_rtx));
		  tsize = 0;
		}

	      if (tsize && tsize != orig_tsize)
		tsize_rtx = GEN_INT (tsize);
	    }

	  if (Pmode == DImode)
	    emit_insn (gen_movdi (stack_pointer_rtx, hard_frame_pointer_rtx));
	  else
	    emit_insn (gen_movsi (stack_pointer_rtx, hard_frame_pointer_rtx));
	}

      /* The GP/PIC register is implicitly used by all SYMBOL_REFs, so if we
	 are going to restore it, then we must emit a blockage insn to
	 prevent the scheduler from moving the restore out of the epilogue.  */
      else if (TARGET_ABICALLS && mips_abi != ABI_32 && mips_abi != ABI_O64
	       && (cfun->machine->frame.mask
		   & (1L << (PIC_OFFSET_TABLE_REGNUM - GP_REG_FIRST))))
	emit_insn (gen_blockage ());

      save_restore_insns (0, tmp_rtx, orig_tsize);

      /* In mips16 mode with a large frame, we adjust the stack
         pointer before restoring the registers.  In this case, we
         should always be using a frame pointer, so everything should
         have been handled above.  */
      if (tsize > 32767 && TARGET_MIPS16)
	abort ();

      if (current_function_calls_eh_return)
	{
	  rtx eh_ofs = EH_RETURN_STACKADJ_RTX;
	  if (Pmode == DImode)
	    emit_insn (gen_adddi3 (eh_ofs, eh_ofs, tsize_rtx));
	  else
	    emit_insn (gen_addsi3 (eh_ofs, eh_ofs, tsize_rtx));
	  tsize_rtx = eh_ofs;
	}

      emit_insn (gen_blockage ());

      if (tsize != 0 || current_function_calls_eh_return)
	{
	  if (!TARGET_MIPS16)
	    {
	      if (Pmode == DImode)
		emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				       tsize_rtx));
	      else
		emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				       tsize_rtx));
	    }
	  else
	    {
	      /* We need to work around not being able to add a register
		 to the stack pointer directly. Use register $6 as an
		 intermediate step.  */

	      rtx g6_rtx = gen_rtx (REG, Pmode, GP_REG_FIRST + 6);

	      if (Pmode == DImode)
		{
		  emit_insn (gen_movdi (g6_rtx, stack_pointer_rtx));
		  emit_insn (gen_adddi3 (g6_rtx, g6_rtx, tsize_rtx));
		  emit_insn (gen_movdi (stack_pointer_rtx, g6_rtx));
		}
	      else
		{
		  emit_insn (gen_movsi (g6_rtx, stack_pointer_rtx));
		  emit_insn (gen_addsi3 (g6_rtx, g6_rtx, tsize_rtx));
		  emit_insn (gen_movsi (stack_pointer_rtx, g6_rtx));
		}
	    }

	}
    }

  /* The mips16 loads the return address into $7, not $31.  */
  if (TARGET_MIPS16 && (cfun->machine->frame.mask & RA_MASK) != 0)
    emit_jump_insn (gen_return_internal (gen_rtx (REG, Pmode,
						  GP_REG_FIRST + 7)));
  else
    emit_jump_insn (gen_return_internal (gen_rtx (REG, Pmode,
						  GP_REG_FIRST + 31)));
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

int
mips_can_use_return_insn ()
{
  tree return_type;

  if (! reload_completed)
    return 0;

  if (regs_ever_live[31] || current_function_profile)
    return 0;

  return_type = DECL_RESULT (current_function_decl);

  /* In mips16 mode, a function which returns a floating point value
     needs to arrange to copy the return value into the floating point
     registers.  */
  if (TARGET_MIPS16
      && mips16_hard_float
      && ! aggregate_value_p (return_type)
      && GET_MODE_CLASS (DECL_MODE (return_type)) == MODE_FLOAT
      && GET_MODE_SIZE (DECL_MODE (return_type)) <= UNITS_PER_FPVALUE)
    return 0;

  if (cfun->machine->frame.initialized)
    return cfun->machine->frame.total_size == 0;

  return compute_frame_size (get_frame_size ()) == 0;
}

/* Returns nonzero if X contains a SYMBOL_REF.  */

static int
symbolic_expression_p (x)
     rtx x;
{
  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  if (GET_CODE (x) == CONST)
    return symbolic_expression_p (XEXP (x, 0));

  if (GET_RTX_CLASS (GET_CODE (x)) == '1')
    return symbolic_expression_p (XEXP (x, 0));

  if (GET_RTX_CLASS (GET_CODE (x)) == 'c'
      || GET_RTX_CLASS (GET_CODE (x)) == '2')
    return (symbolic_expression_p (XEXP (x, 0))
	    || symbolic_expression_p (XEXP (x, 1)));

  return 0;
}

/* Choose the section to use for the constant rtx expression X that has
   mode MODE.  */

static void
mips_select_rtx_section (mode, x, align)
     enum machine_mode mode;
     rtx x;
     unsigned HOST_WIDE_INT align;
{
  if (TARGET_MIPS16)
    {
      /* In mips16 mode, the constant table always goes in the same section
         as the function, so that constants can be loaded using PC relative
         addressing.  */
      function_section (current_function_decl);
    }
  else if (TARGET_EMBEDDED_DATA)
    {
      /* For embedded applications, always put constants in read-only data,
	 in order to reduce RAM usage.  */
      mergeable_constant_section (mode, align, 0);
    }
  else
    {
      /* For hosted applications, always put constants in small data if
	 possible, as this gives the best performance.  */
      /* ??? Consider using mergable small data sections.  */

      if (GET_MODE_SIZE (mode) <= (unsigned) mips_section_threshold
	  && mips_section_threshold > 0)
	SMALL_DATA_SECTION ();
      else if (flag_pic && symbolic_expression_p (x))
	{
	  if (targetm.have_named_sections)
	    named_section (NULL_TREE, ".data.rel.ro", 3);
	  else
	    data_section ();
	}
      else
	mergeable_constant_section (mode, align, 0);
    }
}

/* Choose the section to use for DECL.  RELOC is true if its value contains
   any relocatable expression.

   Some of the logic used here needs to be replicated in
   mips_encode_section_info so that references to these symbols are
   done correctly.  Specifically, at least all symbols assigned here
   to rom (.text and/or .rodata) must not be referenced via
   mips_encode_section_info with %gprel, as the rom might be too far
   away.

   If you need to make a change here, you probably should check
   mips_encode_section_info to see if it needs a similar change.

   ??? This would be fixed by implementing targetm.is_small_data_p.  */

static void
mips_select_section (decl, reloc, align)
     tree decl;
     int reloc;
     unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED;
{
  int size = int_size_in_bytes (TREE_TYPE (decl));

  if ((TARGET_EMBEDDED_PIC || TARGET_MIPS16)
      && TREE_CODE (decl) == STRING_CST
      && !flag_writable_strings)
    /* For embedded position independent code, put constant strings in the
       text section, because the data section is limited to 64K in size.
       For mips16 code, put strings in the text section so that a PC
       relative load instruction can be used to get their address.  */
    text_section ();
  else if (TARGET_EMBEDDED_DATA)
    {
      /* For embedded applications, always put an object in read-only data
	 if possible, in order to reduce RAM usage.  */

      if (((TREE_CODE (decl) == VAR_DECL
	    && TREE_READONLY (decl) && !TREE_SIDE_EFFECTS (decl)
	    && DECL_INITIAL (decl)
	    && (DECL_INITIAL (decl) == error_mark_node
		|| TREE_CONSTANT (DECL_INITIAL (decl))))
	   /* Deal with calls from output_constant_def_contents.  */
	   || (TREE_CODE (decl) != VAR_DECL
	       && (TREE_CODE (decl) != STRING_CST
		   || !flag_writable_strings)))
	  && ! (flag_pic && reloc))
	readonly_data_section ();
      else if (size > 0 && size <= mips_section_threshold)
	SMALL_DATA_SECTION ();
      else
	data_section ();
    }
  else
    {
      /* For hosted applications, always put an object in small data if
	 possible, as this gives the best performance.  */

      if (size > 0 && size <= mips_section_threshold)
	SMALL_DATA_SECTION ();
      else if (((TREE_CODE (decl) == VAR_DECL
		 && TREE_READONLY (decl) && !TREE_SIDE_EFFECTS (decl)
		 && DECL_INITIAL (decl)
		 && (DECL_INITIAL (decl) == error_mark_node
		     || TREE_CONSTANT (DECL_INITIAL (decl))))
		/* Deal with calls from output_constant_def_contents.  */
		|| (TREE_CODE (decl) != VAR_DECL
		    && (TREE_CODE (decl) != STRING_CST
			|| !flag_writable_strings)))
	       && ! (flag_pic && reloc))
	readonly_data_section ();
      else
	data_section ();
    }
}

/* When optimizing for the $gp pointer, SYMBOL_REF_FLAG is set for all
   small objects.

   When generating embedded PIC code, SYMBOL_REF_FLAG is set for
   symbols which are not in the .text section.

   When generating mips16 code, SYMBOL_REF_FLAG is set for string
   constants which are put in the .text section.  We also record the
   total length of all such strings; this total is used to decide
   whether we need to split the constant table, and need not be
   precisely correct.

   When not mips16 code nor embedded PIC, if a symbol is in a
   gp addressable section, SYMBOL_REF_FLAG is set prevent gcc from
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
   is why the DECL_INITIAL macros differ from mips_select_section.  */

static void
mips_encode_section_info (decl, first)
     tree decl;
     int first;
{
  if (TARGET_MIPS16)
    {
      if (first && TREE_CODE (decl) == STRING_CST
	  && ! flag_writable_strings
	  /* If this string is from a function, and the function will
	     go in a gnu linkonce section, then we can't directly
	     access the string.  This gets an assembler error
	     "unsupported PC relative reference to different section".
	     If we modify SELECT_SECTION to put it in function_section
	     instead of text_section, it still fails because
	     DECL_SECTION_NAME isn't set until assemble_start_function.
	     If we fix that, it still fails because strings are shared
	     among multiple functions, and we have cross section
	     references again.  We force it to work by putting string
	     addresses in the constant pool and indirecting.  */
	  && (! current_function_decl
	      || ! DECL_ONE_ONLY (current_function_decl)))
	{
	  rtx symref;

	  symref = XEXP (TREE_CST_RTL (decl), 0);
	  mips16_strings = alloc_EXPR_LIST (0, symref, mips16_strings);
	  SYMBOL_REF_FLAG (symref) = 1;
	  mips_string_length += TREE_STRING_LENGTH (decl);
	}
    }

  if (TARGET_EMBEDDED_DATA
      && (TREE_CODE (decl) == VAR_DECL
	  && TREE_READONLY (decl) && !TREE_SIDE_EFFECTS (decl))
      && (!DECL_INITIAL (decl)
	  || TREE_CONSTANT (DECL_INITIAL (decl))))
    {
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 0;
    }

  else if (TARGET_EMBEDDED_PIC)
    {
      if (TREE_CODE (decl) == VAR_DECL)
	SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 0;
      else if (TREE_CODE (decl) == STRING_CST
	       && ! flag_writable_strings)
	SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (decl), 0)) = 0;
      else
	SYMBOL_REF_FLAG (XEXP (TREE_CST_RTL (decl), 0)) = 1;
    }

  else if (TREE_CODE (decl) == VAR_DECL
	   && DECL_SECTION_NAME (decl) != NULL_TREE
	   && (0 == strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			    ".sdata")
	       || 0 == strcmp (TREE_STRING_POINTER (DECL_SECTION_NAME (decl)),
			       ".sbss")))
    {
      SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
    }

  /* We can not perform GP optimizations on variables which are in
       specific sections, except for .sdata and .sbss which are
       handled above.  */
  else if (TARGET_GP_OPT && TREE_CODE (decl) == VAR_DECL
	   && DECL_SECTION_NAME (decl) == NULL_TREE
	   && ! (TARGET_MIPS16 && TREE_PUBLIC (decl)
		 && (DECL_COMMON (decl)
		     || DECL_ONE_ONLY (decl)
		     || DECL_WEAK (decl))))
    {
      int size = int_size_in_bytes (TREE_TYPE (decl));

      if (size > 0 && size <= mips_section_threshold)
	SYMBOL_REF_FLAG (XEXP (DECL_RTL (decl), 0)) = 1;
    }

}

/* Return register to use for a function return value with VALTYPE for
   function FUNC.  MODE is used instead of VALTYPE for LIBCALLs.  */

rtx
mips_function_value (valtype, func, mode)
     tree valtype;
     tree func ATTRIBUTE_UNUSED;
     enum machine_mode mode;
{
  int reg = GP_RETURN;
  enum mode_class mclass;
  int unsignedp = 1;

  if (valtype)
    {
      mode = TYPE_MODE (valtype);
      unsignedp = TREE_UNSIGNED (valtype);

      /* Since we define PROMOTE_FUNCTION_RETURN, we must promote
	 the mode just as PROMOTE_MODE does.  */
      mode = promote_mode (valtype, mode, &unsignedp, 1);
    }
  mclass = GET_MODE_CLASS (mode);

  if (mclass == MODE_FLOAT && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE)
    reg = FP_RETURN;

  else if (mclass == MODE_FLOAT && mode == TFmode)
    /* long doubles are really split between f0 and f2, not f1.  Eek.
       Use DImode for each component, since GCC wants integer modes
       for subregs.  */
    return gen_rtx_PARALLEL
      (VOIDmode,
       gen_rtvec (2,
		  gen_rtx_EXPR_LIST (VOIDmode,
				     gen_rtx_REG (DImode, FP_RETURN),
				     GEN_INT (0)),
		  gen_rtx_EXPR_LIST (VOIDmode,
				     gen_rtx_REG (DImode, FP_RETURN + 2),
				     GEN_INT (GET_MODE_SIZE (mode) / 2))));
       

  else if (mclass == MODE_COMPLEX_FLOAT
	   && GET_MODE_SIZE (mode) <= UNITS_PER_HWFPVALUE * 2)
    {
      enum machine_mode cmode = GET_MODE_INNER (mode);

      return gen_rtx_PARALLEL
	(VOIDmode,
	 gen_rtvec (2,
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG (cmode, FP_RETURN),
				       GEN_INT (0)),
		    gen_rtx_EXPR_LIST (VOIDmode,
				       gen_rtx_REG (cmode, FP_RETURN + FP_INC),
				       GEN_INT (GET_MODE_SIZE (cmode)))));
    }

  else if (valtype && TREE_CODE (valtype) == RECORD_TYPE
	   && mips_abi != ABI_32
	   && mips_abi != ABI_O64
	   && mips_abi != ABI_EABI)
    {
      /* A struct with only one or two floating point fields is returned in
	 the floating point registers.  */
      tree field, fields[2];
      int i;

      for (i = 0, field = TYPE_FIELDS (valtype); field;
	   field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (TREE_CODE (TREE_TYPE (field)) != REAL_TYPE || i >= 2)
	    break;

	  fields[i++] = field;
	}

      /* Must check i, so that we reject structures with no elements.  */
      if (! field)
	{
	  if (i == 1)
	    {
	      /* The structure has DImode, but we don't allow DImode values
		 in FP registers, so we use a PARALLEL even though it isn't
		 strictly necessary.  */
	      enum machine_mode field_mode = TYPE_MODE (TREE_TYPE (fields[0]));

	      return gen_rtx_PARALLEL
		(mode,
		 gen_rtvec (1,
			    gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (field_mode,
							    FP_RETURN),
					       const0_rtx)));
	    }

	  else if (i == 2)
	    {
	      enum machine_mode first_mode
		= TYPE_MODE (TREE_TYPE (fields[0]));
	      enum machine_mode second_mode
		= TYPE_MODE (TREE_TYPE (fields[1]));
	      HOST_WIDE_INT first_offset = int_byte_position (fields[0]);
	      HOST_WIDE_INT second_offset = int_byte_position (fields[1]);

	      return gen_rtx_PARALLEL
		(mode,
		 gen_rtvec (2,
			    gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (first_mode,
							    FP_RETURN),
					       GEN_INT (first_offset)),
			    gen_rtx_EXPR_LIST (VOIDmode,
					       gen_rtx_REG (second_mode,
							    FP_RETURN + 2),
					       GEN_INT (second_offset))));
	    }
	}
    }

  return gen_rtx_REG (mode, reg);
}

/* The implementation of FUNCTION_ARG_PASS_BY_REFERENCE.  Return
   nonzero when an argument must be passed by reference.  */

int
function_arg_pass_by_reference (cum, mode, type, named)
     const CUMULATIVE_ARGS *cum;
     enum machine_mode mode;
     tree type;
     int named ATTRIBUTE_UNUSED;
{
  int size;

  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    return 0;

  /* We must pass by reference if we would be both passing in registers
     and the stack.  This is because any subsequent partial arg would be
     handled incorrectly in this case.

     ??? This is really a kludge.  We should either fix GCC so that such
     a situation causes an abort and then do something in the MIPS port
     to prevent it, or add code to function.c to properly handle the case.  */
  /* ??? cum can be NULL when called from mips_va_arg.  The problem handled
     here hopefully is not relevant to mips_va_arg.  */
  if (cum && MUST_PASS_IN_STACK (mode, type)
      && mips_abi != ABI_MEABI
      && FUNCTION_ARG (*cum, mode, type, named) != 0)
    return 1;

  /* Otherwise, we only do this if EABI is selected.  */
  if (mips_abi != ABI_EABI)
    return 0;

  /* ??? How should SCmode be handled?  */
  if (type == NULL_TREE || mode == DImode || mode == DFmode)
    return 0;

  size = int_size_in_bytes (type);
  return size == -1 || size > UNITS_PER_WORD;
}

/* Return the class of registers for which a mode change from FROM to TO
   is invalid.

   In little-endian mode, the hi-lo registers are numbered backwards,
   so (subreg:SI (reg:DI hi) 0) gets the high word instead of the low
   word as intended.

   Similarly, when using paired floating-point registers, the first
   register holds the low word, regardless of endianness.  So in big
   endian mode, (subreg:SI (reg:DF $f0) 0) does not get the high word
   as intended.

   Also, loading a 32-bit value into a 64-bit floating-point register
   will not sign-extend the value, despite what LOAD_EXTEND_OP says.
   We can't allow 64-bit float registers to change from a 32-bit
   mode to a 64-bit mode.  */

bool
mips_cannot_change_mode_class (from, to, class)
     enum machine_mode from, to;
     enum reg_class class;
{
  if (GET_MODE_SIZE (from) != GET_MODE_SIZE (to))
    {
      if (TARGET_BIG_ENDIAN)
	return reg_classes_intersect_p (FP_REGS, class);
      if (TARGET_FLOAT64)
	return reg_classes_intersect_p (HI_AND_FP_REGS, class);
      return reg_classes_intersect_p (HI_REG, class);
    }
  return false;
}

/* This function returns the register class required for a secondary
   register when copying between one of the registers in CLASS, and X,
   using MODE.  If IN_P is nonzero, the copy is going from X to the
   register, otherwise the register is the source.  A return value of
   NO_REGS means that no secondary register is required.  */

enum reg_class
mips_secondary_reload_class (class, mode, x, in_p)
     enum reg_class class;
     enum machine_mode mode;
     rtx x;
     int in_p;
{
  enum reg_class gr_regs = TARGET_MIPS16 ? M16_REGS : GR_REGS;
  int regno = -1;
  int gp_reg_p;

  if (GET_CODE (x) == SIGN_EXTEND)
    {
      int off = 0;

      x = XEXP (x, 0);

      /* We may be called with reg_renumber NULL from regclass.
	 ??? This is probably a bug.  */
      if (reg_renumber)
	regno = true_regnum (x);
      else
	{
	  while (GET_CODE (x) == SUBREG)
	    {
	      off += subreg_regno_offset (REGNO (SUBREG_REG (x)),
					  GET_MODE (SUBREG_REG (x)),
					  SUBREG_BYTE (x),
					  GET_MODE (x));
	      x = SUBREG_REG (x);
	    }

	  if (GET_CODE (x) == REG)
	    regno = REGNO (x) + off;
	}

      /* 64-bit floating-point registers don't store 32-bit values
	 in sign-extended form.  The only way we can reload
	 (sign_extend:DI (reg:SI $f0)) is by moving $f0 into
	 an integer register using a 32-bit move.  */
      if (FP_REG_P (regno))
	return (class == GR_REGS ? NO_REGS : GR_REGS);

      /* For the same reason, we can only reload (sign_extend:DI FOO) into
	 a floating-point register when FOO is an integer register. */
      if (class == FP_REGS)
	return (GP_REG_P (regno) ? NO_REGS : GR_REGS);
    }

  else if (GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
    regno = true_regnum (x);

  gp_reg_p = TARGET_MIPS16 ? M16_REG_P (regno) : GP_REG_P (regno);

  /* We always require a general register when copying anything to
     HILO_REGNUM, except when copying an SImode value from HILO_REGNUM
     to a general register, or when copying from register 0.  */
  if (class == HILO_REG && regno != GP_REG_FIRST + 0)
    return ((! in_p
	     && gp_reg_p
	     && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (SImode))
	    ? NO_REGS : gr_regs);
  else if (regno == HILO_REGNUM)
    return ((in_p
	     && class == gr_regs
	     && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (SImode))
	    ? NO_REGS : gr_regs);

  /* Copying from HI or LO to anywhere other than a general register
     requires a general register.  */
  if (class == HI_REG || class == LO_REG || class == MD_REGS)
    {
      if (TARGET_MIPS16 && in_p)
	{
	  /* We can't really copy to HI or LO at all in mips16 mode.  */
	  return M16_REGS;
	}
      return gp_reg_p ? NO_REGS : gr_regs;
    }
  if (MD_REG_P (regno))
    {
      if (TARGET_MIPS16 && ! in_p)
	{
	  /* We can't really copy to HI or LO at all in mips16 mode.  */
	  return M16_REGS;
	}
      return class == gr_regs ? NO_REGS : gr_regs;
    }

  /* We can only copy a value to a condition code register from a
     floating point register, and even then we require a scratch
     floating point register.  We can only copy a value out of a
     condition code register into a general register.  */
  if (class == ST_REGS)
    {
      if (in_p)
	return FP_REGS;
      return GP_REG_P (regno) ? NO_REGS : GR_REGS;
    }
  if (ST_REG_P (regno))
    {
      if (! in_p)
	return FP_REGS;
      return class == GR_REGS ? NO_REGS : GR_REGS;
    }

  if (class == FP_REGS)
    {
      if (GET_CODE (x) == MEM)
	{
	  /* In this case we can use lwc1, swc1, ldc1 or sdc1. */
	  return NO_REGS;
	}
      else if (CONSTANT_P (x) && GET_MODE_CLASS (mode) == MODE_FLOAT)
	{
	  /* We can use the l.s and l.d macros to load floating-point
	     constants.  ??? For l.s, we could probably get better
	     code by returning GR_REGS here.  */
	  return NO_REGS;
	}
      else if (GP_REG_P (regno) || x == CONST0_RTX (mode))
	{
	  /* In this case we can use mtc1, mfc1, dmtc1 or dmfc1.  */
	  return NO_REGS;
	}
      else if (FP_REG_P (regno))
	{
	  /* In this case we can use mov.s or mov.d.  */
	  return NO_REGS;
	}
      else
	{
	  /* Otherwise, we need to reload through an integer register.  */
	  return GR_REGS;
	}
    }

  /* In mips16 mode, going between memory and anything but M16_REGS
     requires an M16_REG.  */
  if (TARGET_MIPS16)
    {
      if (class != M16_REGS && class != M16_NA_REGS)
	{
	  if (gp_reg_p)
	    return NO_REGS;
	  return M16_REGS;
	}
      if (! gp_reg_p)
	{
	  /* The stack pointer isn't a valid operand to an add instruction,
	     so we need to load it into M16_REGS first.  This can happen as
	     a result of register elimination and form_sum converting
	     (plus reg (plus SP CONST)) to (plus (plus reg SP) CONST).  We
	     need an extra register if the dest is the same as the other
	     register.  In that case, we can't fix the problem by loading SP
	     into the dest first.  */
	  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == REG
	      && GET_CODE (XEXP (x, 1)) == REG
	      && (XEXP (x, 0) == stack_pointer_rtx
		  || XEXP (x, 1) == stack_pointer_rtx))
	    return (class == M16_REGS ? M16_NA_REGS : M16_REGS);

	  if (class == M16_REGS || class == M16_NA_REGS)
	    return NO_REGS;
	  return M16_REGS;
	}
    }

  return NO_REGS;
}

/* This function returns the maximum number of consecutive registers
   needed to represent mode MODE in registers of class CLASS.  */

int
mips_class_max_nregs (class, mode)
     enum reg_class class;
     enum machine_mode mode;
{
  if (class == FP_REGS)
    return FP_INC;
  else
    return (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* For each mips16 function which refers to GP relative symbols, we
   use a pseudo register, initialized at the start of the function, to
   hold the $gp value.  */

rtx
mips16_gp_pseudo_reg ()
{
  if (cfun->machine->mips16_gp_pseudo_rtx == NULL_RTX)
    {
      rtx const_gp;
      rtx insn, scan;

      cfun->machine->mips16_gp_pseudo_rtx = gen_reg_rtx (Pmode);
      RTX_UNCHANGING_P (cfun->machine->mips16_gp_pseudo_rtx) = 1;

      /* We want to initialize this to a value which gcc will believe
         is constant.  */
      const_gp = gen_rtx (CONST, Pmode,
			  gen_rtx (REG, Pmode, GP_REG_FIRST + 28));

      start_sequence ();
      emit_move_insn (cfun->machine->mips16_gp_pseudo_rtx,
		      const_gp);
      insn = get_insns ();
      end_sequence ();

      push_topmost_sequence ();
      /* We need to emit the initialization after the FUNCTION_BEG
         note, so that it will be integrated.  */
      for (scan = get_insns (); scan != NULL_RTX; scan = NEXT_INSN (scan))
	if (GET_CODE (scan) == NOTE
	    && NOTE_LINE_NUMBER (scan) == NOTE_INSN_FUNCTION_BEG)
	  break;
      if (scan == NULL_RTX)
	scan = get_insns ();
      insn = emit_insn_after (insn, scan);
      pop_topmost_sequence ();
    }

  return cfun->machine->mips16_gp_pseudo_rtx;
}

/* Return an RTX which represents the signed 16 bit offset from the
   $gp register for the given symbol.  This is only used on the
   mips16.  */

rtx
mips16_gp_offset (sym)
     rtx sym;
{
  tree gp;

  if (GET_CODE (sym) != SYMBOL_REF
      || ! SYMBOL_REF_FLAG (sym))
    abort ();

  /* We use a special identifier to represent the value of the gp
     register.  */
  gp = get_identifier ("__mips16_gp_value");

  return gen_rtx (CONST, Pmode,
		  gen_rtx (MINUS, Pmode, sym,
			   gen_rtx (SYMBOL_REF, Pmode,
				    IDENTIFIER_POINTER (gp))));
}

/* Return nonzero if the given RTX represents a signed 16 bit offset
   from the $gp register.  */

int
mips16_gp_offset_p (x)
     rtx x;
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  /* It's OK to add a small integer value to a gp offset.  */
  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && SMALL_INT (XEXP (x, 1)))
	return mips16_gp_offset_p (XEXP (x, 0));
      if (GET_CODE (XEXP (x, 0)) == CONST_INT
	  && SMALL_INT (XEXP (x, 0)))
	return mips16_gp_offset_p (XEXP (x, 1));
      return 0;
    }

  /* Make sure it is in the form SYM - __mips16_gp_value.  */
  return (GET_CODE (x) == MINUS
	  && GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && SYMBOL_REF_FLAG (XEXP (x, 0))
	  && GET_CODE (XEXP (x, 1)) == SYMBOL_REF
	  && strcmp (XSTR (XEXP (x, 1), 0), "__mips16_gp_value") == 0);
}

/* Output a GP offset.  We don't want to print the subtraction of
   __mips16_gp_value; it is implicitly represented by the %gprel which
   should have been printed by the caller.  */

static void
mips16_output_gp_offset (file, x)
     FILE *file;
     rtx x;
{
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      mips16_output_gp_offset (file, XEXP (x, 0));
      fputs ("+", file);
      mips16_output_gp_offset (file, XEXP (x, 1));
      return;
    }

  if (GET_CODE (x) == MINUS
      && GET_CODE (XEXP (x, 1)) == SYMBOL_REF
      && strcmp (XSTR (XEXP (x, 1), 0), "__mips16_gp_value") == 0)
    {
      mips16_output_gp_offset (file, XEXP (x, 0));
      return;
    }

  output_addr_const (file, x);
}

/* Return nonzero if a constant should not be output until after the
   function.  This is true of most string constants, so that we can
   use a more efficient PC relative reference.  However, a static
   inline function may never call assemble_function_end to write out
   the constant pool, so don't try to postpone the constant in that
   case.

   ??? It's really a bug that a static inline function can put stuff
   in the constant pool even if the function itself is not output.

   We record which string constants we've seen, so that we know which
   ones might use the more efficient reference.  */

int
mips16_constant_after_function_p (x)
     tree x;
{
  if (TREE_CODE (x) == STRING_CST
      && ! flag_writable_strings
      && current_function_decl != 0
      && ! DECL_DEFER_OUTPUT (current_function_decl)
      && ! (DECL_INLINE (current_function_decl)
	    && ((! TREE_PUBLIC (current_function_decl)
		 && ! TREE_ADDRESSABLE (current_function_decl)
		 && ! flag_keep_inline_functions)
		|| DECL_EXTERNAL (current_function_decl))))
    {
      struct string_constant *n;

      n = (struct string_constant *) xmalloc (sizeof *n);
      n->label = XSTR (XEXP (TREE_CST_RTL (x), 0), 0);
      n->next = string_constants;
      string_constants = n;

      return 1;
    }

  return 0;
}

/* Validate a constant for the mips16.  This rejects general symbolic
   addresses, which must be loaded from memory.  If ADDR is nonzero,
   this should reject anything which is not a legal address.  If
   ADDEND is nonzero, this is being added to something else.  */

int
mips16_constant (x, mode, addr, addend)
     rtx x;
     enum machine_mode mode;
     int addr;
     int addend;
{
  while (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  switch (GET_CODE (x))
    {
    default:
      return 0;

    case PLUS:
      return (mips16_constant (XEXP (x, 0), mode, addr, 1)
	      && mips16_constant (XEXP (x, 1), mode, addr, 1));

    case SYMBOL_REF:
      if (addr && GET_MODE_SIZE (mode) != 4 && GET_MODE_SIZE (mode) != 8)
	return 0;
      if (CONSTANT_POOL_ADDRESS_P (x))
	return 1;

      /* If we aren't looking for a memory address, we can accept a GP
         relative symbol, which will have SYMBOL_REF_FLAG set; movsi
         knows how to handle this.  We can always accept a string
         constant, which is the other case in which SYMBOL_REF_FLAG
         will be set.  */
      if (! addr
	  && ! addend
	  && SYMBOL_REF_FLAG (x)
	  && mode == (enum machine_mode) Pmode)
	return 1;

      /* We can accept a string constant, which will have
         SYMBOL_REF_FLAG set but must be recognized by name to
         distinguish from a GP accessible symbol.  The name of a
         string constant will have been generated by
         ASM_GENERATE_INTERNAL_LABEL as called by output_constant_def.  */
      if (SYMBOL_REF_FLAG (x))
	{
	  const char *name = XSTR (x, 0);

	  return (name[0] == '*'
		  && strncmp (name + 1, LOCAL_LABEL_PREFIX,
			      sizeof LOCAL_LABEL_PREFIX - 1) == 0);
	}

      return 0;

    case LABEL_REF:
      if (addr && GET_MODE_SIZE (mode) != 4 && GET_MODE_SIZE (mode) != 8)
	return 0;
      return 1;

    case CONST_INT:
      if (addr && ! addend)
	return 0;
      return INTVAL (x) > - 0x10000 && INTVAL (x) <= 0xffff;

    case REG:
      /* We need to treat $gp as a legitimate constant, because
         mips16_gp_pseudo_reg assumes that.  */
      return REGNO (x) == GP_REG_FIRST + 28;
    }
}

/* Write out code to move floating point arguments in or out of
   general registers.  Output the instructions to FILE.  FP_CODE is
   the code describing which arguments are present (see the comment at
   the definition of CUMULATIVE_ARGS in mips.h).  FROM_FP_P is nonzero if
   we are copying from the floating point registers.  */

static void
mips16_fp_args (file, fp_code, from_fp_p)
     FILE *file;
     int fp_code;
     int from_fp_p;
{
  const char *s;
  int gparg, fparg;
  unsigned int f;

  /* This code only works for the original 32 bit ABI and the O64 ABI.  */
  if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    abort ();

  if (from_fp_p)
    s = "mfc1";
  else
    s = "mtc1";
  gparg = GP_ARG_FIRST;
  fparg = FP_ARG_FIRST;
  for (f = (unsigned int) fp_code; f != 0; f >>= 2)
    {
      if ((f & 3) == 1)
	{
	  if ((fparg & 1) != 0)
	    ++fparg;
	  fprintf (file, "\t%s\t%s,%s\n", s,
		   reg_names[gparg], reg_names[fparg]);
	}
      else if ((f & 3) == 2)
	{
	  if (TARGET_64BIT)
	    fprintf (file, "\td%s\t%s,%s\n", s,
		     reg_names[gparg], reg_names[fparg]);
	  else
	    {
	      if ((fparg & 1) != 0)
		++fparg;
	      if (TARGET_BIG_ENDIAN)
		fprintf (file, "\t%s\t%s,%s\n\t%s\t%s,%s\n", s,
			 reg_names[gparg], reg_names[fparg + 1], s,
			 reg_names[gparg + 1], reg_names[fparg]);
	      else
		fprintf (file, "\t%s\t%s,%s\n\t%s\t%s,%s\n", s,
			 reg_names[gparg], reg_names[fparg], s,
			 reg_names[gparg + 1], reg_names[fparg + 1]);
	      ++gparg;
	      ++fparg;
	    }
	}
      else
	abort ();

      ++gparg;
      ++fparg;
    }
}

/* Build a mips16 function stub.  This is used for functions which
   take aruments in the floating point registers.  It is 32 bit code
   that moves the floating point args into the general registers, and
   then jumps to the 16 bit code.  */

static void
build_mips16_function_stub (file)
     FILE *file;
{
  const char *fnname;
  char *secname, *stubname;
  tree stubid, stubdecl;
  int need_comma;
  unsigned int f;

  fnname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
  secname = (char *) alloca (strlen (fnname) + 20);
  sprintf (secname, ".mips16.fn.%s", fnname);
  stubname = (char *) alloca (strlen (fnname) + 20);
  sprintf (stubname, "__fn_stub_%s", fnname);
  stubid = get_identifier (stubname);
  stubdecl = build_decl (FUNCTION_DECL, stubid,
			 build_function_type (void_type_node, NULL_TREE));
  DECL_SECTION_NAME (stubdecl) = build_string (strlen (secname), secname);

  fprintf (file, "\t# Stub function for %s (", current_function_name);
  need_comma = 0;
  for (f = (unsigned int) current_function_args_info.fp_code; f != 0; f >>= 2)
    {
      fprintf (file, "%s%s",
	       need_comma ? ", " : "",
	       (f & 3) == 1 ? "float" : "double");
      need_comma = 1;
    }
  fprintf (file, ")\n");

  fprintf (file, "\t.set\tnomips16\n");
  function_section (stubdecl);
  ASM_OUTPUT_ALIGN (file, floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT));

  /* ??? If FUNCTION_NAME_ALREADY_DECLARED is defined, then we are
     within a .ent, and we can not emit another .ent.  */
#ifndef FUNCTION_NAME_ALREADY_DECLARED
  fputs ("\t.ent\t", file);
  assemble_name (file, stubname);
  fputs ("\n", file);
#endif

  assemble_name (file, stubname);
  fputs (":\n", file);

  /* We don't want the assembler to insert any nops here.  */
  fprintf (file, "\t.set\tnoreorder\n");

  mips16_fp_args (file, current_function_args_info.fp_code, 1);

  fprintf (asm_out_file, "\t.set\tnoat\n");
  fprintf (asm_out_file, "\tla\t%s,", reg_names[GP_REG_FIRST + 1]);
  assemble_name (file, fnname);
  fprintf (file, "\n");
  fprintf (asm_out_file, "\tjr\t%s\n", reg_names[GP_REG_FIRST + 1]);
  fprintf (asm_out_file, "\t.set\tat\n");

  /* Unfortunately, we can't fill the jump delay slot.  We can't fill
     with one of the mfc1 instructions, because the result is not
     available for one instruction, so if the very first instruction
     in the function refers to the register, it will see the wrong
     value.  */
  fprintf (file, "\tnop\n");

  fprintf (file, "\t.set\treorder\n");

#ifndef FUNCTION_NAME_ALREADY_DECLARED
  fputs ("\t.end\t", file);
  assemble_name (file, stubname);
  fputs ("\n", file);
#endif

  fprintf (file, "\t.set\tmips16\n");

  function_section (current_function_decl);
}

/* We keep a list of functions for which we have already built stubs
   in build_mips16_call_stub.  */

struct mips16_stub
{
  struct mips16_stub *next;
  char *name;
  int fpret;
};

static struct mips16_stub *mips16_stubs;

/* Build a call stub for a mips16 call.  A stub is needed if we are
   passing any floating point values which should go into the floating
   point registers.  If we are, and the call turns out to be to a 32
   bit function, the stub will be used to move the values into the
   floating point registers before calling the 32 bit function.  The
   linker will magically adjust the function call to either the 16 bit
   function or the 32 bit stub, depending upon where the function call
   is actually defined.

   Similarly, we need a stub if the return value might come back in a
   floating point register.

   RETVAL, FNMEM, and ARG_SIZE are the values passed to the call insn
   (RETVAL is NULL if this is call rather than call_value).  FP_CODE
   is the code built by function_arg.  This function returns a nonzero
   value if it builds the call instruction itself.  */

int
build_mips16_call_stub (retval, fnmem, arg_size, fp_code)
     rtx retval;
     rtx fnmem;
     rtx arg_size;
     int fp_code;
{
  int fpret;
  rtx fn;
  const char *fnname;
  char *secname, *stubname;
  struct mips16_stub *l;
  tree stubid, stubdecl;
  int need_comma;
  unsigned int f;

  /* We don't need to do anything if we aren't in mips16 mode, or if
     we were invoked with the -msoft-float option.  */
  if (! TARGET_MIPS16 || ! mips16_hard_float)
    return 0;

  /* Figure out whether the value might come back in a floating point
     register.  */
  fpret = (retval != 0
	   && GET_MODE_CLASS (GET_MODE (retval)) == MODE_FLOAT
	   && GET_MODE_SIZE (GET_MODE (retval)) <= UNITS_PER_FPVALUE);

  /* We don't need to do anything if there were no floating point
     arguments and the value will not be returned in a floating point
     register.  */
  if (fp_code == 0 && ! fpret)
    return 0;

  if (GET_CODE (fnmem) != MEM)
    abort ();
  fn = XEXP (fnmem, 0);

  /* We don't need to do anything if this is a call to a special
     mips16 support function.  */
  if (GET_CODE (fn) == SYMBOL_REF
      && strncmp (XSTR (fn, 0), "__mips16_", 9) == 0)
    return 0;

  /* This code will only work for o32 and o64 abis.  The other ABI's
     require more sophisticated support.  */
  if (mips_abi != ABI_32 && mips_abi != ABI_O64)
    abort ();

  /* We can only handle SFmode and DFmode floating point return
     values.  */
  if (fpret && GET_MODE (retval) != SFmode && GET_MODE (retval) != DFmode)
    abort ();

  /* If we're calling via a function pointer, then we must always call
     via a stub.  There are magic stubs provided in libgcc.a for each
     of the required cases.  Each of them expects the function address
     to arrive in register $2.  */

  if (GET_CODE (fn) != SYMBOL_REF)
    {
      char buf[30];
      tree id;
      rtx stub_fn, stub_mem, insn;

      /* ??? If this code is modified to support other ABI's, we need
         to handle PARALLEL return values here.  */

      sprintf (buf, "__mips16_call_stub_%s%d",
	       (fpret
		? (GET_MODE (retval) == SFmode ? "sf_" : "df_")
		: ""),
	       fp_code);
      id = get_identifier (buf);
      stub_fn = gen_rtx (SYMBOL_REF, Pmode, IDENTIFIER_POINTER (id));
      stub_mem = gen_rtx (MEM, Pmode, stub_fn);

      emit_move_insn (gen_rtx (REG, Pmode, 2), fn);

      if (retval == NULL_RTX)
	insn = gen_call_internal0 (stub_mem, arg_size,
				   gen_rtx (REG, SImode,
					    GP_REG_FIRST + 31));
      else
	insn = gen_call_value_internal0 (retval, stub_mem, arg_size,
					 gen_rtx (REG, SImode,
						  GP_REG_FIRST + 31));
      insn = emit_call_insn (insn);

      /* Put the register usage information on the CALL.  */
      if (GET_CODE (insn) != CALL_INSN)
	abort ();
      CALL_INSN_FUNCTION_USAGE (insn) =
	gen_rtx (EXPR_LIST, VOIDmode,
		 gen_rtx (USE, VOIDmode, gen_rtx (REG, Pmode, 2)),
		 CALL_INSN_FUNCTION_USAGE (insn));

      /* If we are handling a floating point return value, we need to
         save $18 in the function prologue.  Putting a note on the
         call will mean that regs_ever_live[$18] will be true if the
         call is not eliminated, and we can check that in the prologue
         code.  */
      if (fpret)
	CALL_INSN_FUNCTION_USAGE (insn) =
	  gen_rtx (EXPR_LIST, VOIDmode,
		   gen_rtx (USE, VOIDmode, gen_rtx (REG, word_mode, 18)),
		   CALL_INSN_FUNCTION_USAGE (insn));

      /* Return 1 to tell the caller that we've generated the call
         insn.  */
      return 1;
    }

  /* We know the function we are going to call.  If we have already
     built a stub, we don't need to do anything further.  */

  fnname = XSTR (fn, 0);
  for (l = mips16_stubs; l != NULL; l = l->next)
    if (strcmp (l->name, fnname) == 0)
      break;

  if (l == NULL)
    {
      /* Build a special purpose stub.  When the linker sees a
	 function call in mips16 code, it will check where the target
	 is defined.  If the target is a 32 bit call, the linker will
	 search for the section defined here.  It can tell which
	 symbol this section is associated with by looking at the
	 relocation information (the name is unreliable, since this
	 might be a static function).  If such a section is found, the
	 linker will redirect the call to the start of the magic
	 section.

	 If the function does not return a floating point value, the
	 special stub section is named
	     .mips16.call.FNNAME

	 If the function does return a floating point value, the stub
	 section is named
	     .mips16.call.fp.FNNAME
	 */

      secname = (char *) alloca (strlen (fnname) + 40);
      sprintf (secname, ".mips16.call.%s%s",
	       fpret ? "fp." : "",
	       fnname);
      stubname = (char *) alloca (strlen (fnname) + 20);
      sprintf (stubname, "__call_stub_%s%s",
	       fpret ? "fp_" : "",
	       fnname);
      stubid = get_identifier (stubname);
      stubdecl = build_decl (FUNCTION_DECL, stubid,
			     build_function_type (void_type_node, NULL_TREE));
      DECL_SECTION_NAME (stubdecl) = build_string (strlen (secname), secname);

      fprintf (asm_out_file, "\t# Stub function to call %s%s (",
	       (fpret
		? (GET_MODE (retval) == SFmode ? "float " : "double ")
		: ""),
	       fnname);
      need_comma = 0;
      for (f = (unsigned int) fp_code; f != 0; f >>= 2)
	{
	  fprintf (asm_out_file, "%s%s",
		   need_comma ? ", " : "",
		   (f & 3) == 1 ? "float" : "double");
	  need_comma = 1;
	}
      fprintf (asm_out_file, ")\n");

      fprintf (asm_out_file, "\t.set\tnomips16\n");
      assemble_start_function (stubdecl, stubname);

#ifndef FUNCTION_NAME_ALREADY_DECLARED
      fputs ("\t.ent\t", asm_out_file);
      assemble_name (asm_out_file, stubname);
      fputs ("\n", asm_out_file);

      assemble_name (asm_out_file, stubname);
      fputs (":\n", asm_out_file);
#endif

      /* We build the stub code by hand.  That's the only way we can
	 do it, since we can't generate 32 bit code during a 16 bit
	 compilation.  */

      /* We don't want the assembler to insert any nops here.  */
      fprintf (asm_out_file, "\t.set\tnoreorder\n");

      mips16_fp_args (asm_out_file, fp_code, 0);

      if (! fpret)
	{
	  fprintf (asm_out_file, "\t.set\tnoat\n");
	  fprintf (asm_out_file, "\tla\t%s,%s\n", reg_names[GP_REG_FIRST + 1],
		   fnname);
	  fprintf (asm_out_file, "\tjr\t%s\n", reg_names[GP_REG_FIRST + 1]);
	  fprintf (asm_out_file, "\t.set\tat\n");
	  /* Unfortunately, we can't fill the jump delay slot.  We
	     can't fill with one of the mtc1 instructions, because the
	     result is not available for one instruction, so if the
	     very first instruction in the function refers to the
	     register, it will see the wrong value.  */
	  fprintf (asm_out_file, "\tnop\n");
	}
      else
	{
	  fprintf (asm_out_file, "\tmove\t%s,%s\n",
		   reg_names[GP_REG_FIRST + 18], reg_names[GP_REG_FIRST + 31]);
	  fprintf (asm_out_file, "\tjal\t%s\n", fnname);
	  /* As above, we can't fill the delay slot.  */
	  fprintf (asm_out_file, "\tnop\n");
	  if (GET_MODE (retval) == SFmode)
	    fprintf (asm_out_file, "\tmfc1\t%s,%s\n",
		     reg_names[GP_REG_FIRST + 2], reg_names[FP_REG_FIRST + 0]);
	  else
	    {
	      if (TARGET_BIG_ENDIAN)
		{
		  fprintf (asm_out_file, "\tmfc1\t%s,%s\n",
			   reg_names[GP_REG_FIRST + 2],
			   reg_names[FP_REG_FIRST + 1]);
		  fprintf (asm_out_file, "\tmfc1\t%s,%s\n",
			   reg_names[GP_REG_FIRST + 3],
			   reg_names[FP_REG_FIRST + 0]);
		}
	      else
		{
		  fprintf (asm_out_file, "\tmfc1\t%s,%s\n",
			   reg_names[GP_REG_FIRST + 2],
			   reg_names[FP_REG_FIRST + 0]);
		  fprintf (asm_out_file, "\tmfc1\t%s,%s\n",
			   reg_names[GP_REG_FIRST + 3],
			   reg_names[FP_REG_FIRST + 1]);
		}
	    }
	  fprintf (asm_out_file, "\tj\t%s\n", reg_names[GP_REG_FIRST + 18]);
	  /* As above, we can't fill the delay slot.  */
	  fprintf (asm_out_file, "\tnop\n");
	}

      fprintf (asm_out_file, "\t.set\treorder\n");

#ifdef ASM_DECLARE_FUNCTION_SIZE
      ASM_DECLARE_FUNCTION_SIZE (asm_out_file, stubname, stubdecl);
#endif

#ifndef FUNCTION_NAME_ALREADY_DECLARED
      fputs ("\t.end\t", asm_out_file);
      assemble_name (asm_out_file, stubname);
      fputs ("\n", asm_out_file);
#endif

      fprintf (asm_out_file, "\t.set\tmips16\n");

      /* Record this stub.  */
      l = (struct mips16_stub *) xmalloc (sizeof *l);
      l->name = xstrdup (fnname);
      l->fpret = fpret;
      l->next = mips16_stubs;
      mips16_stubs = l;
    }

  /* If we expect a floating point return value, but we've built a
     stub which does not expect one, then we're in trouble.  We can't
     use the existing stub, because it won't handle the floating point
     value.  We can't build a new stub, because the linker won't know
     which stub to use for the various calls in this object file.
     Fortunately, this case is illegal, since it means that a function
     was declared in two different ways in a single compilation.  */
  if (fpret && ! l->fpret)
    error ("can not handle inconsistent calls to `%s'", fnname);

  /* If we are calling a stub which handles a floating point return
     value, we need to arrange to save $18 in the prologue.  We do
     this by marking the function call as using the register.  The
     prologue will later see that it is used, and emit code to save
     it.  */

  if (l->fpret)
    {
      rtx insn;

      if (retval == NULL_RTX)
	insn = gen_call_internal0 (fnmem, arg_size,
				   gen_rtx (REG, SImode,
					    GP_REG_FIRST + 31));
      else
	insn = gen_call_value_internal0 (retval, fnmem, arg_size,
					 gen_rtx (REG, SImode,
						  GP_REG_FIRST + 31));
      insn = emit_call_insn (insn);

      if (GET_CODE (insn) != CALL_INSN)
	abort ();

      CALL_INSN_FUNCTION_USAGE (insn) =
	gen_rtx (EXPR_LIST, VOIDmode,
		 gen_rtx (USE, VOIDmode, gen_rtx (REG, word_mode, 18)),
		 CALL_INSN_FUNCTION_USAGE (insn));

      /* Return 1 to tell the caller that we've generated the call
         insn.  */
      return 1;
    }

  /* Return 0 to let the caller generate the call insn.  */
  return 0;
}

/* This function looks through the code for a function, and tries to
   optimize the usage of the $gp register.  We arrange to copy $gp
   into a pseudo-register, and then let gcc's normal reload handling
   deal with the pseudo-register.  Unfortunately, if reload choose to
   put the pseudo-register into a call-clobbered register, it will
   emit saves and restores for that register around any function
   calls.  We don't need the saves, and it's faster to copy $gp than
   to do an actual restore.  ??? This still means that we waste a
   stack slot.

   This is an optimization, and the code which gcc has actually
   generated is correct, so we do not need to catch all cases.  */

static void
mips16_optimize_gp (first)
     rtx first;
{
  rtx gpcopy, slot, insn;

  /* Look through the instructions.  Set GPCOPY to the register which
     holds a copy of $gp.  Set SLOT to the stack slot where it is
     saved.  If we find an instruction which sets GPCOPY to anything
     other than $gp or SLOT, then we can't use it.  If we find an
     instruction which sets SLOT to anything other than GPCOPY, we
     can't use it.  */

  gpcopy = NULL_RTX;
  slot = NULL_RTX;
  for (insn = first; insn != NULL_RTX; insn = next_active_insn (insn))
    {
      rtx set;

      if (! INSN_P (insn))
	continue;

      set = PATTERN (insn);

      /* We know that all references to memory will be inside a SET,
         because there is no other way to access memory on the mips16.
         We don't have to worry about a PARALLEL here, because the
         mips.md file will never generate them for memory references.  */
      if (GET_CODE (set) != SET)
	continue;

      if (gpcopy == NULL_RTX
	  && GET_CODE (SET_SRC (set)) == CONST
	  && GET_CODE (XEXP (SET_SRC (set), 0)) == REG
	  && REGNO (XEXP (SET_SRC (set), 0)) == GP_REG_FIRST + 28
	  && GET_CODE (SET_DEST (set)) == REG
	  && GET_MODE (SET_DEST (set)) == (unsigned) Pmode)
	gpcopy = SET_DEST (set);
      else if (slot == NULL_RTX
	       && gpcopy != NULL_RTX
	       && GET_CODE (SET_DEST (set)) == MEM
	       && GET_CODE (SET_SRC (set)) == REG
	       && REGNO (SET_SRC (set)) == REGNO (gpcopy)
	       && GET_MODE (SET_DEST (set)) == (unsigned) Pmode)
	{
	  rtx base, offset;

	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_DEST (set), 0), &offset);
	  if (GET_CODE (base) == REG
	      && (REGNO (base) == STACK_POINTER_REGNUM
		  || REGNO (base) == FRAME_POINTER_REGNUM))
	    slot = SET_DEST (set);
	}
      else if (gpcopy != NULL_RTX
	       && (GET_CODE (SET_DEST (set)) == REG
		   || GET_CODE (SET_DEST (set)) == SUBREG)
	       && reg_overlap_mentioned_p (SET_DEST (set), gpcopy)
	       && (GET_CODE (SET_DEST (set)) != REG
		   || REGNO (SET_DEST (set)) != REGNO (gpcopy)
		   || GET_MODE (SET_DEST (set)) != (unsigned) Pmode
		   || ((GET_CODE (SET_SRC (set)) != CONST
			|| GET_CODE (XEXP (SET_SRC (set), 0)) != REG
			|| (REGNO (XEXP (SET_SRC (set), 0))
			    != GP_REG_FIRST + 28))
		       && ! rtx_equal_p (SET_SRC (set), slot))))
	break;
      else if (slot != NULL_RTX
	       && GET_CODE (SET_DEST (set)) == MEM
	       && rtx_equal_p (SET_DEST (set), slot)
	       && (GET_CODE (SET_SRC (set)) != REG
		   || REGNO (SET_SRC (set)) != REGNO (gpcopy)))
	break;
    }

  /* If we couldn't find a unique value for GPCOPY or SLOT, then try a
     different optimization.  Any time we find a copy of $28 into a
     register, followed by an add of a symbol_ref to that register, we
     convert it to load the value from the constant table instead.
     The copy and add will take six bytes, just as the load and
     constant table entry will take six bytes.  However, it is
     possible that the constant table entry will be shared.

     This could be a peephole optimization, but I don't know if the
     peephole code can call force_const_mem.

     Using the same register for the copy of $28 and the add of the
     symbol_ref is actually pretty likely, since the add instruction
     requires the destination and the first addend to be the same
     register.  */

  if (insn != NULL_RTX || gpcopy == NULL_RTX || slot == NULL_RTX)
    {
      rtx next;

      /* This optimization is only reasonable if the constant table
         entries are only 4 bytes.  */
      if (Pmode != SImode)
	return;

      for (insn = first; insn != NULL_RTX; insn = next)
	{
	  rtx set1, set2;

	  next = insn;
	  do
	    {
	      next = NEXT_INSN (next);
	    }
	  while (next != NULL_RTX
		 && (GET_CODE (next) == NOTE
		     || (GET_CODE (next) == INSN
			 && (GET_CODE (PATTERN (next)) == USE
			     || GET_CODE (PATTERN (next)) == CLOBBER))));

	  if (next == NULL_RTX)
	    break;

	  if (! INSN_P (insn))
	    continue;

	  if (! INSN_P (next))
	    continue;

	  set1 = PATTERN (insn);
	  if (GET_CODE (set1) != SET)
	    continue;
	  set2 = PATTERN (next);
	  if (GET_CODE (set2) != SET)
	    continue;

	  if (GET_CODE (SET_DEST (set1)) == REG
	      && GET_CODE (SET_SRC (set1)) == CONST
	      && GET_CODE (XEXP (SET_SRC (set1), 0)) == REG
	      && REGNO (XEXP (SET_SRC (set1), 0)) == GP_REG_FIRST + 28
	      && rtx_equal_p (SET_DEST (set1), SET_DEST (set2))
	      && GET_CODE (SET_SRC (set2)) == PLUS
	      && rtx_equal_p (SET_DEST (set1), XEXP (SET_SRC (set2), 0))
	      && mips16_gp_offset_p (XEXP (SET_SRC (set2), 1))
	      && GET_CODE (XEXP (XEXP (SET_SRC (set2), 1), 0)) == MINUS)
	    {
	      rtx sym;

	      /* We've found a case we can change to load from the
                 constant table.  */

	      sym = XEXP (XEXP (XEXP (SET_SRC (set2), 1), 0), 0);
	      if (GET_CODE (sym) != SYMBOL_REF)
		abort ();
	      emit_insn_after (gen_rtx (SET, VOIDmode, SET_DEST (set1),
					force_const_mem (Pmode, sym)),
			       next);

	      PUT_CODE (insn, NOTE);
	      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (insn) = 0;

	      PUT_CODE (next, NOTE);
	      NOTE_LINE_NUMBER (next) = NOTE_INSN_DELETED;
	      NOTE_SOURCE_FILE (next) = 0;
	    }
	}

      return;
    }

  /* We can safely remove all assignments to SLOT from GPCOPY, and
     replace all assignments from SLOT to GPCOPY with assignments from
     $28.  */

  for (insn = first; insn != NULL_RTX; insn = next_active_insn (insn))
    {
      rtx set;

      if (! INSN_P (insn))
	continue;

      set = PATTERN (insn);
      if (GET_CODE (set) != SET
	  || GET_MODE (SET_DEST (set)) != (unsigned) Pmode)
	continue;

      if (GET_CODE (SET_DEST (set)) == MEM
	  && rtx_equal_p (SET_DEST (set), slot)
	  && GET_CODE (SET_SRC (set)) == REG
	  && REGNO (SET_SRC (set)) == REGNO (gpcopy))
	{
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (insn) = 0;
	}
      else if (GET_CODE (SET_DEST (set)) == REG
	       && REGNO (SET_DEST (set)) == REGNO (gpcopy)
	       && GET_CODE (SET_SRC (set)) == MEM
	       && rtx_equal_p (SET_SRC (set), slot))
	{
	  emit_insn_after (gen_rtx (SET, Pmode, SET_DEST (set),
				    gen_rtx (CONST, Pmode,
					     gen_rtx (REG, Pmode,
						      GP_REG_FIRST + 28))),
			   insn);
	  PUT_CODE (insn, NOTE);
	  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (insn) = 0;
	}
    }
}

/* We keep a list of constants we which we have to add to internal
   constant tables in the middle of large functions.  */

struct constant
{
  struct constant *next;
  rtx value;
  rtx label;
  enum machine_mode mode;
};

/* Add a constant to the list in *PCONSTANTS.  */

static rtx
add_constant (pconstants, val, mode)
     struct constant **pconstants;
     rtx val;
     enum machine_mode mode;
{
  struct constant *c;

  for (c = *pconstants; c != NULL; c = c->next)
    if (mode == c->mode && rtx_equal_p (val, c->value))
      return c->label;

  c = (struct constant *) xmalloc (sizeof *c);
  c->value = val;
  c->mode = mode;
  c->label = gen_label_rtx ();
  c->next = *pconstants;
  *pconstants = c;
  return c->label;
}

/* Dump out the constants in CONSTANTS after INSN.  */

static void
dump_constants (constants, insn)
     struct constant *constants;
     rtx insn;
{
  struct constant *c;
  int align;

  c = constants;
  align = 0;
  while (c != NULL)
    {
      rtx r;
      struct constant *next;

      switch (GET_MODE_SIZE (c->mode))
	{
	case 1:
	  align = 0;
	  break;
	case 2:
	  if (align < 1)
	    insn = emit_insn_after (gen_align_2 (), insn);
	  align = 1;
	  break;
	case 4:
	  if (align < 2)
	    insn = emit_insn_after (gen_align_4 (), insn);
	  align = 2;
	  break;
	default:
	  if (align < 3)
	    insn = emit_insn_after (gen_align_8 (), insn);
	  align = 3;
	  break;
	}

      insn = emit_label_after (c->label, insn);

      switch (c->mode)
	{
	case QImode:
	  r = gen_consttable_qi (c->value);
	  break;
	case HImode:
	  r = gen_consttable_hi (c->value);
	  break;
	case SImode:
	  r = gen_consttable_si (c->value);
	  break;
	case SFmode:
	  r = gen_consttable_sf (c->value);
	  break;
	case DImode:
	  r = gen_consttable_di (c->value);
	  break;
	case DFmode:
	  r = gen_consttable_df (c->value);
	  break;
	default:
	  abort ();
	}

      insn = emit_insn_after (r, insn);

      next = c->next;
      free (c);
      c = next;
    }

  emit_barrier_after (insn);
}

/* Find the symbol in an address expression.  */

static rtx
mips_find_symbol (addr)
     rtx addr;
{
  if (GET_CODE (addr) == MEM)
    addr = XEXP (addr, 0);
  while (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    return addr;
  if (GET_CODE (addr) == PLUS)
    {
      rtx l1, l2;

      l1 = mips_find_symbol (XEXP (addr, 0));
      l2 = mips_find_symbol (XEXP (addr, 1));
      if (l1 != NULL_RTX && l2 == NULL_RTX)
	return l1;
      else if (l1 == NULL_RTX && l2 != NULL_RTX)
	return l2;
    }
  return NULL_RTX;
}

/* Exported to toplev.c.

   Do a final pass over the function, just before delayed branch
   scheduling.  */

void
machine_dependent_reorg (first)
     rtx first;
{
  int insns_len, max_internal_pool_size, pool_size, addr, first_constant_ref;
  rtx insn;
  struct constant *constants;

  if (! TARGET_MIPS16)
    return;

  /* If $gp is used, try to remove stores, and replace loads with
     copies from $gp.  */
  if (optimize)
    mips16_optimize_gp (first);

  /* Scan the function looking for PC relative loads which may be out
     of range.  All such loads will either be from the constant table,
     or be getting the address of a constant string.  If the size of
     the function plus the size of the constant table is less than
     0x8000, then all loads are in range.  */

  insns_len = 0;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      insns_len += get_attr_length (insn);

      /* ??? We put switch tables in .text, but we don't define
         JUMP_TABLES_IN_TEXT_SECTION, so get_attr_length will not
         compute their lengths correctly.  */
      if (GET_CODE (insn) == JUMP_INSN)
	{
	  rtx body;

	  body = PATTERN (insn);
	  if (GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
	    insns_len += (XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC)
			  * GET_MODE_SIZE (GET_MODE (body)));
	  insns_len += GET_MODE_SIZE (GET_MODE (body)) - 1;
	}
    }

  /* Store the original value of insns_len in cfun->machine, so
     that simple_memory_operand can look at it.  */
  cfun->machine->insns_len = insns_len;

  pool_size = get_pool_size ();
  if (insns_len + pool_size + mips_string_length < 0x8000)
    return;

  /* Loop over the insns and figure out what the maximum internal pool
     size could be.  */
  max_internal_pool_size = 0;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SET)
	{
	  rtx src;

	  src = mips_find_symbol (SET_SRC (PATTERN (insn)));
	  if (src == NULL_RTX)
	    continue;
	  if (CONSTANT_POOL_ADDRESS_P (src))
	    max_internal_pool_size += GET_MODE_SIZE (get_pool_mode (src));
	  else if (SYMBOL_REF_FLAG (src))
	    max_internal_pool_size += GET_MODE_SIZE (Pmode);
	}
    }

  constants = NULL;
  addr = 0;
  first_constant_ref = -1;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SET)
	{
	  rtx val, src;
	  enum machine_mode mode = VOIDmode;

	  val = NULL_RTX;
	  src = mips_find_symbol (SET_SRC (PATTERN (insn)));
	  if (src != NULL_RTX && CONSTANT_POOL_ADDRESS_P (src))
	    {
	      /* ??? This is very conservative, which means that we
                 will generate too many copies of the constant table.
                 The only solution would seem to be some form of
                 relaxing.  */
	      if (((insns_len - addr)
		   + max_internal_pool_size
		   + get_pool_offset (src))
		  >= 0x8000)
		{
		  val = get_pool_constant (src);
		  mode = get_pool_mode (src);
		}
	      max_internal_pool_size -= GET_MODE_SIZE (get_pool_mode (src));
	    }
	  else if (src != NULL_RTX && SYMBOL_REF_FLAG (src))
	    {
	      /* Including all of mips_string_length is conservative,
                 and so is including all of max_internal_pool_size.  */
	      if (((insns_len - addr)
		   + max_internal_pool_size
		   + pool_size
		   + mips_string_length)
		  >= 0x8000)
		{
		  val = src;
		  mode = Pmode;
		}
	      max_internal_pool_size -= Pmode;
	    }

	  if (val != NULL_RTX)
	    {
	      rtx lab, newsrc;

	      /* This PC relative load is out of range.  ??? In the
		 case of a string constant, we are only guessing that
		 it is range, since we don't know the offset of a
		 particular string constant.  */

	      lab = add_constant (&constants, val, mode);
	      newsrc = gen_rtx (MEM, mode,
				gen_rtx (LABEL_REF, VOIDmode, lab));
	      RTX_UNCHANGING_P (newsrc) = 1;
	      PATTERN (insn) = gen_rtx (SET, VOIDmode,
					SET_DEST (PATTERN (insn)),
					newsrc);
	      INSN_CODE (insn) = -1;

	      if (first_constant_ref < 0)
		first_constant_ref = addr;
	    }
	}

      addr += get_attr_length (insn);

      /* ??? We put switch tables in .text, but we don't define
         JUMP_TABLES_IN_TEXT_SECTION, so get_attr_length will not
         compute their lengths correctly.  */
      if (GET_CODE (insn) == JUMP_INSN)
	{
	  rtx body;

	  body = PATTERN (insn);
	  if (GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
	    addr += (XVECLEN (body, GET_CODE (body) == ADDR_DIFF_VEC)
			  * GET_MODE_SIZE (GET_MODE (body)));
	  addr += GET_MODE_SIZE (GET_MODE (body)) - 1;
	}

      if (GET_CODE (insn) == BARRIER)
	{
	  /* Output any constants we have accumulated.  Note that we
             don't need to change ADDR, since its only use is
             subtraction from INSNS_LEN, and both would be changed by
             the same amount.
	     ??? If the instructions up to the next barrier reuse a
	     constant, it would often be better to continue
	     accumulating.  */
	  if (constants != NULL)
	    dump_constants (constants, insn);
	  constants = NULL;
	  first_constant_ref = -1;
	}

      if (constants != NULL
	       && (NEXT_INSN (insn) == NULL
		   || (first_constant_ref >= 0
		       && (((addr - first_constant_ref)
			    + 2 /* for alignment */
			    + 2 /* for a short jump insn */
			    + pool_size)
			   >= 0x8000))))
	{
	  /* If we haven't had a barrier within 0x8000 bytes of a
             constant reference or we are at the end of the function,
             emit a barrier now.  */

	  rtx label, jump, barrier;

	  label = gen_label_rtx ();
	  jump = emit_jump_insn_after (gen_jump (label), insn);
	  JUMP_LABEL (jump) = label;
	  LABEL_NUSES (label) = 1;
	  barrier = emit_barrier_after (jump);
	  emit_label_after (label, barrier);
	  first_constant_ref = -1;
	}
     }

  /* ??? If we output all references to a constant in internal
     constants table, we don't need to output the constant in the real
     constant table, but we have no way to prevent that.  */
}

/* Return nonzero if X is a SIGN or ZERO extend operator.  */
int
extend_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (x);
  return code == SIGN_EXTEND || code == ZERO_EXTEND;
}

/* Accept any operator that can be used to shift the high half of the
   input value to the lower half, suitable for truncation.  The
   remainder (the lower half of the input, and the upper half of the
   output) will be discarded.  */
int
highpart_shift_operator (x, mode)
     rtx x;
     enum machine_mode mode ATTRIBUTE_UNUSED;
{
  enum rtx_code code = GET_CODE (x);
  return (code == LSHIFTRT
	  || code == ASHIFTRT
	  || code == ROTATERT
	  || code == ROTATE);
}

/* Return a number assessing the cost of moving a register in class
   FROM to class TO.  The classes are expressed using the enumeration
   values such as `GENERAL_REGS'.  A value of 2 is the default; other
   values are interpreted relative to that.

   It is not required that the cost always equal 2 when FROM is the
   same as TO; on some machines it is expensive to move between
   registers if they are not general registers.

   If reload sees an insn consisting of a single `set' between two
   hard registers, and if `REGISTER_MOVE_COST' applied to their
   classes returns a value of 2, reload does not check to ensure that
   the constraints of the insn are met.  Setting a cost of other than
   2 will allow reload to verify that the constraints are met.  You
   should do this if the `movM' pattern's constraints do not allow
   such copying.

   ??? We make make the cost of moving from HI/LO/HILO/MD into general
   registers the same as for one of moving general registers to
   HI/LO/HILO/MD for TARGET_MIPS16 in order to prevent allocating a
   pseudo to HI/LO/HILO/MD.  This might hurt optimizations though, it
   isn't clear if it is wise.  And it might not work in all cases.  We
   could solve the DImode LO reg problem by using a multiply, just
   like reload_{in,out}si.  We could solve the SImode/HImode HI reg
   problem by using divide instructions.  divu puts the remainder in
   the HI reg, so doing a divide by -1 will move the value in the HI
   reg for all values except -1.  We could handle that case by using a
   signed divide, e.g.  -1 / 2 (or maybe 1 / -2?).  We'd have to emit
   a compare/branch to test the input value to see which instruction
   we need to use.  This gets pretty messy, but it is feasible.  */

int
mips_register_move_cost (mode, to, from)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     enum reg_class to, from;
{
  if (from == M16_REGS && GR_REG_CLASS_P (to))
    return 2;
  else if (from == M16_NA_REGS && GR_REG_CLASS_P (to))
    return 2;
  else if (GR_REG_CLASS_P (from))
    {
      if (to == M16_REGS)
	return 2;
      else if (to == M16_NA_REGS)
	return 2;
      else if (GR_REG_CLASS_P (to))
	{
	  if (TARGET_MIPS16)
	    return 4;
	  else
	    return 2;
	}
      else if (to == FP_REGS)
	return 4;
      else if (to == HI_REG || to == LO_REG || to == MD_REGS
	       || to == HILO_REG)
	{
	  if (TARGET_MIPS16)
	    return 12;
	  else
	    return 6;
	}
      else if (COP_REG_CLASS_P (to))
	{
	  return 5;
	}
    }  /* GR_REG_CLASS_P (from) */
  else if (from == FP_REGS)
    {
      if (GR_REG_CLASS_P (to))
	return 4;
      else if (to == FP_REGS)
	return 2;
      else if (to == ST_REGS)
	return 8;
    }  /* from == FP_REGS */
  else if (from == HI_REG || from == LO_REG || from == MD_REGS
	   || from == HILO_REG)
    {
      if (GR_REG_CLASS_P (to))
	{
	  if (TARGET_MIPS16)
	    return 12;
	  else
	    return 6;
	}
    }  /* from == HI_REG, etc. */
  else if (from == ST_REGS && GR_REG_CLASS_P (to))
    return 4;
  else if (COP_REG_CLASS_P (from))
    {
      return 5;
    }  /* COP_REG_CLASS_P (from) */

  /* fallthru */

  return 12;
}

/* Return the length of INSN.  LENGTH is the initial length computed by
   attributes in the machine-description file.  */

int
mips_adjust_insn_length (insn, length)
     rtx insn;
     int length;
{
  /* A unconditional jump has an unfilled delay slot if it is not part
     of a sequence.  A conditional jump normally has a delay slot, but
     does not on MIPS16.  */
  if (simplejump_p (insn)
      || (!TARGET_MIPS16  && (GET_CODE (insn) == JUMP_INSN
			      || GET_CODE (insn) == CALL_INSN)))
    length += 4;

  /* All MIPS16 instructions are a measly two bytes.  */
  if (TARGET_MIPS16)
    length /= 2;

  return length;
}

/* Output assembly instructions to peform a conditional branch.

   INSN is the branch instruction.  OPERANDS[0] is the condition.
   OPERANDS[1] is the target of the branch.  OPERANDS[2] is the target
   of the first operand to the condition.  If TWO_OPERANDS_P is
   nonzero the comparison takes two operands; OPERANDS[3] will be the
   second operand.

   If INVERTED_P is nonzero we are to branch if the condition does
   not hold.  If FLOAT_P is nonzero this is a floating-point comparison.

   LENGTH is the length (in bytes) of the sequence we are to generate.
   That tells us whether to generate a simple conditional branch, or a
   reversed conditional branch around a `jr' instruction.  */
const char *
mips_output_conditional_branch (insn,
				operands,
				two_operands_p,
				float_p,
				inverted_p,
				length)
     rtx insn;
     rtx *operands;
     int two_operands_p;
     int float_p;
     int inverted_p;
     int length;
{
  static char buffer[200];
  /* The kind of comparison we are doing.  */
  enum rtx_code code = GET_CODE (operands[0]);
  /* Nonzero if the opcode for the comparison needs a `z' indicating
     that it is a comparision against zero.  */
  int need_z_p;
  /* A string to use in the assembly output to represent the first
     operand.  */
  const char *op1 = "%z2";
  /* A string to use in the assembly output to represent the second
     operand.  Use the hard-wired zero register if there's no second
     operand.  */
  const char *op2 = (two_operands_p ? ",%z3" : ",%.");
  /* The operand-printing string for the comparison.  */
  const char *const comp = (float_p ? "%F0" : "%C0");
  /* The operand-printing string for the inverted comparison.  */
  const char *const inverted_comp = (float_p ? "%W0" : "%N0");

  /* The MIPS processors (for levels of the ISA at least two), have
     "likely" variants of each branch instruction.  These instructions
     annul the instruction in the delay slot if the branch is not
     taken.  */
  mips_branch_likely = (final_sequence && INSN_ANNULLED_BRANCH_P (insn));

  if (!two_operands_p)
    {
      /* To compute whether than A > B, for example, we normally
	 subtract B from A and then look at the sign bit.  But, if we
	 are doing an unsigned comparison, and B is zero, we don't
	 have to do the subtraction.  Instead, we can just check to
	 see if A is nonzero.  Thus, we change the CODE here to
	 reflect the simpler comparison operation.  */
      switch (code)
	{
	case GTU:
	  code = NE;
	  break;

	case LEU:
	  code = EQ;
	  break;

	case GEU:
	  /* A condition which will always be true.  */
	  code = EQ;
	  op1 = "%.";
	  break;

	case LTU:
	  /* A condition which will always be false.  */
	  code = NE;
	  op1 = "%.";
	  break;

	default:
	  /* Not a special case.  */
	  break;
	}
    }

  /* Relative comparisons are always done against zero.  But
     equality comparisons are done between two operands, and therefore
     do not require a `z' in the assembly language output.  */
  need_z_p = (!float_p && code != EQ && code != NE);
  /* For comparisons against zero, the zero is not provided
     explicitly.  */
  if (need_z_p)
    op2 = "";

  /* Begin by terminating the buffer.  That way we can always use
     strcat to add to it.  */
  buffer[0] = '\0';

  switch (length)
    {
    case 4:
    case 8:
      /* Just a simple conditional branch.  */
      if (float_p)
	sprintf (buffer, "%%*b%s%%?\t%%Z2%%1",
		 inverted_p ? inverted_comp : comp);
      else
	sprintf (buffer, "%%*b%s%s%%?\t%s%s,%%1",
		 inverted_p ? inverted_comp : comp,
		 need_z_p ? "z" : "",
		 op1,
		 op2);
      return buffer;

    case 12:
    case 16:
    case 24:
    case 28:
      {
	/* Generate a reversed conditional branch around ` j'
	   instruction:

		.set noreorder
		.set nomacro
		bc    l
		delay_slot or #nop
		j     target
		#nop
	     l:
		.set macro
		.set reorder

	   If the original branch was a likely branch, the delay slot
	   must be executed only if the branch is taken, so generate:

		.set noreorder
		.set nomacro
		bc    l
		#nop
		j     target
		delay slot or #nop
	     l:
		.set macro
		.set reorder
	   
	   When generating non-embedded PIC, instead of:

	        j     target

	   we emit:

	        .set noat
	        la    $at, target
		jr    $at
		.set at
	*/

        rtx orig_target;
	rtx target = gen_label_rtx ();

        orig_target = operands[1];
        operands[1] = target;
	/* Generate the reversed comparison.  This takes four
	   bytes.  */
	if (float_p)
	  sprintf (buffer, "%%*b%s\t%%Z2%%1",
		   inverted_p ? comp : inverted_comp);
	else
	  sprintf (buffer, "%%*b%s%s\t%s%s,%%1",
		   inverted_p ? comp : inverted_comp,
		   need_z_p ? "z" : "",
		   op1,
		   op2);
        output_asm_insn (buffer, operands);

        if (length != 16 && length != 28 && ! mips_branch_likely)
          {
            /* Output delay slot instruction.  */
            rtx insn = final_sequence;
            final_scan_insn (XVECEXP (insn, 0, 1), asm_out_file,
                             optimize, 0, 1);
            INSN_DELETED_P (XVECEXP (insn, 0, 1)) = 1;
          }
	else
	  output_asm_insn ("%#", 0);

	if (length <= 16)
	  output_asm_insn ("j\t%0", &orig_target);
	else
	  {
	    if (Pmode == DImode)
	      output_asm_insn ("%[dla\t%@,%0\n\tjr\t%@%]", &orig_target);
	    else
	      output_asm_insn ("%[la\t%@,%0\n\tjr\t%@%]", &orig_target);
	  }

        if (length != 16 && length != 28 && mips_branch_likely)
          {
            /* Output delay slot instruction.  */
            rtx insn = final_sequence;
            final_scan_insn (XVECEXP (insn, 0, 1), asm_out_file,
                             optimize, 0, 1);
            INSN_DELETED_P (XVECEXP (insn, 0, 1)) = 1;
          }
	else
	  output_asm_insn ("%#", 0);

        ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "L",
                                   CODE_LABEL_NUMBER (target));

        return "";
      }

    /* We do not currently use this code.  It handles jumps to
       arbitrary locations, using `jr', even across a 256MB boundary.
       We could add a -mhuge switch, and then use this code instead of
       the `j' alternative above when -mhuge was used.  */
#if 0
    case 16:
    case 20:
      {
	/* Generate a reversed conditional branch around a `jr'
	   instruction:

		 .set noreorder
		 .set nomacro
		 .set noat
		 bc    l
		 la    $at, target
		 jr    $at
		 .set at
		 .set macro
		 .set reorder
	      l:

	   Not pretty, but allows a conditional branch anywhere in the
	   32-bit address space.  If the original branch is annulled,
	   then the instruction in the delay slot should be executed
	   only if the branch is taken.  The la instruction is really
	   a macro which will usually take eight bytes, but sometimes
	   takes only four, if the instruction to which we're jumping
	   gets its own entry in the global pointer table, which will
	   happen if its a case label.  The assembler will then
	   generate only a four-byte sequence, rather than eight, and
	   there seems to be no way to tell it not to.  Thus, we can't
	   just use a `.+x' addressing form; we don't know what value
	   to give for `x'.

	   So, we resort to using the explicit relocation syntax
	   available in the assembler and do:

	      lw $at,%got_page(target)($gp)
	      daddiu $at,$at,%got_ofst(target)

	   That way, this always takes up eight bytes, and we can use
	   the `.+x' form.  Of course, these explicit machinations
	   with relocation will not work with old assemblers.  Then
	   again, neither do out-of-range branches, so we haven't lost
	   anything.  */

	/* The target of the reversed branch.  */
	const char *const target
	  = ((mips_branch_likely || length == 20) ? ".+20" : ".+16");
	const char *at_register = mips_reg_names[ASSEMBLER_SCRATCH_REGNUM];
	const char *gp_register = mips_reg_names[PIC_OFFSET_TABLE_REGNUM];
	char *c;

	strcpy (buffer, "%(%<%[");
	c = strchr (buffer, '\0');
	/* Generate the reversed comparision.  This takes four
	   bytes.  */
	if (float_p)
	  sprintf (c, "%%*b%s\t%%Z2%s",
		   inverted_p ? comp : inverted_comp,
		   target);
	else
	  sprintf (c, "%%*b%s%s\t%s%s,%s",
		   inverted_p ? comp : inverted_comp,
		   need_z_p ? "z" : "",
		   op1,
		   op2,
		   target);
	c = strchr (buffer, '\0');
	/* Generate the load-address, and jump.  This takes twelve
	   bytes, for a total of 16.  */
	sprintf (c,
		 "\n\tlw\t%s,%%%%got_page(%%1)(%s)\n\tdaddiu\t%s,%s,%%%%got_ofst(%%1)\n\tjr\t%s",
		 at_register,
		 gp_register,
		 at_register,
		 at_register,
		 at_register);
	if (length == 20)
	  /* The delay slot was unfilled.  Since we're inside
	     .noreorder, the assembler will not fill in the NOP for
	     us, so we must do it ourselves.  */
	  strcat (buffer, "\n\tnop");
	strcat (buffer, "%]%>%)");
	return buffer;
      }
#endif

    default:
      abort ();
    }

  /* NOTREACHED */
  return 0;
}

/* Return true if GIVEN is the same as CANONICAL, or if it is CANONICAL
   with a final "000" replaced by "k".  Ignore case.

   Note: this function is shared between GCC and GAS.  */

static bool
mips_strict_matching_cpu_name_p (canonical, given)
     const char *canonical, *given;
{
  while (*given != 0 && TOLOWER (*given) == TOLOWER (*canonical))
    given++, canonical++;

  return ((*given == 0 && *canonical == 0)
	  || (strcmp (canonical, "000") == 0 && strcasecmp (given, "k") == 0));
}


/* Return true if GIVEN matches CANONICAL, where GIVEN is a user-supplied
   CPU name.  We've traditionally allowed a lot of variation here.

   Note: this function is shared between GCC and GAS.  */

static bool
mips_matching_cpu_name_p (canonical, given)
     const char *canonical, *given;
{
  /* First see if the name matches exactly, or with a final "000"
     turned into "k".  */
  if (mips_strict_matching_cpu_name_p (canonical, given))
    return true;

  /* If not, try comparing based on numerical designation alone.
     See if GIVEN is an unadorned number, or 'r' followed by a number.  */
  if (TOLOWER (*given) == 'r')
    given++;
  if (!ISDIGIT (*given))
    return false;

  /* Skip over some well-known prefixes in the canonical name,
     hoping to find a number there too.  */
  if (TOLOWER (canonical[0]) == 'v' && TOLOWER (canonical[1]) == 'r')
    canonical += 2;
  else if (TOLOWER (canonical[0]) == 'r' && TOLOWER (canonical[1]) == 'm')
    canonical += 2;
  else if (TOLOWER (canonical[0]) == 'r')
    canonical += 1;

  return mips_strict_matching_cpu_name_p (canonical, given);
}


/* Parse an option that takes the name of a processor as its argument.
   OPTION is the name of the option and CPU_STRING is the argument.
   Return the corresponding processor enumeration if the CPU_STRING is
   recognized, otherwise report an error and return null.

   A similar function exists in GAS.  */

static const struct mips_cpu_info *
mips_parse_cpu (option, cpu_string)
     const char *option, *cpu_string;
{
  const struct mips_cpu_info *p;
  const char *s;

  /* In the past, we allowed upper-case CPU names, but it doesn't
     work well with the multilib machinery.  */
  for (s = cpu_string; *s != 0; s++)
    if (ISUPPER (*s))
      {
	warning ("the cpu name must be lower case");
	break;
      }

  /* 'from-abi' selects the most compatible architecture for the given
     ABI: MIPS I for 32-bit ABIs and MIPS III for 64-bit ABIs.  For the
     EABIs, we have to decide whether we're using the 32-bit or 64-bit
     version.  Look first at the -mgp options, if given, otherwise base
     the choice on MASK_64BIT in TARGET_DEFAULT.  */
  if (strcasecmp (cpu_string, "from-abi") == 0)
    return mips_cpu_info_from_isa (ABI_NEEDS_32BIT_REGS ? 1
				   : ABI_NEEDS_64BIT_REGS ? 3
				   : (TARGET_64BIT ? 3 : 1));

  /* 'default' has traditionally been a no-op.  Probably not very useful.  */
  if (strcasecmp (cpu_string, "default") == 0)
    return 0;

  for (p = mips_cpu_info_table; p->name != 0; p++)
    if (mips_matching_cpu_name_p (p->name, cpu_string))
      return p;

  error ("bad value (%s) for %s", cpu_string, option);
  return 0;
}


/* Return the processor associated with the given ISA level, or null
   if the ISA isn't valid.  */

static const struct mips_cpu_info *
mips_cpu_info_from_isa (isa)
     int isa;
{
  const struct mips_cpu_info *p;

  for (p = mips_cpu_info_table; p->name != 0; p++)
    if (p->isa == isa)
      return p;

  return 0;
}

/* Adjust the cost of INSN based on the relationship between INSN that
   is dependent on DEP_INSN through the dependence LINK.  The default
   is to make no adjustment to COST.

   On the MIPS, ignore the cost of anti- and output-dependencies.  */
static int
mips_adjust_cost (insn, link, dep, cost)
     rtx insn ATTRIBUTE_UNUSED;
     rtx link;
     rtx dep ATTRIBUTE_UNUSED;
     int cost;
{
  if (REG_NOTE_KIND (link) != 0)
    return 0;	/* Anti or output dependence.  */
  return cost;
}

/* ??? This could be replaced with the default elf version if
   TARGET_IS_SMALL_DATA_P is set properly.  */

static void
mips_unique_section (decl, reloc)
     tree decl;
     int reloc;
{
  int len, size, sec;
  const char *name, *prefix;
  char *string;
  static const char *const prefixes[4][2] = {
    { ".text.", ".gnu.linkonce.t." },
    { ".rodata.", ".gnu.linkonce.r." },
    { ".data.", ".gnu.linkonce.d." },
    { ".sdata.", ".gnu.linkonce.s." }
  };

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = (* targetm.strip_name_encoding) (name);
  size = int_size_in_bytes (TREE_TYPE (decl));

  /* Determine the base section we are interested in:
     0=text, 1=rodata, 2=data, 3=sdata, [4=bss].  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    sec = 0;
  else if (DECL_INITIAL (decl) == 0
           || DECL_INITIAL (decl) == error_mark_node)
    sec = 2;
  else if ((TARGET_EMBEDDED_PIC || TARGET_MIPS16)
      && TREE_CODE (decl) == STRING_CST
      && !flag_writable_strings)
    {
      /* For embedded position independent code, put constant
	 strings in the text section, because the data section
	 is limited to 64K in size.  For mips16 code, put
	 strings in the text section so that a PC relative load
	 instruction can be used to get their address.  */
      sec = 0;
    }
  else if (TARGET_EMBEDDED_DATA)
    {
      /* For embedded applications, always put an object in
	 read-only data if possible, in order to reduce RAM
	 usage.  */

      if (decl_readonly_section (decl, reloc))
	sec = 1;
      else if (size > 0 && size <= mips_section_threshold)
	sec = 3;
      else
	sec = 2;
    }
  else
    {
      /* For hosted applications, always put an object in
	 small data if possible, as this gives the best
	 performance.  */

      if (size > 0 && size <= mips_section_threshold)
	sec = 3;
      else if (decl_readonly_section (decl, reloc))
	sec = 1;
      else
	sec = 2;
    }

  prefix = prefixes[sec][DECL_ONE_ONLY (decl)];
  len = strlen (name) + strlen (prefix);
  string = alloca (len + 1);
  sprintf (string, "%s%s", prefix, name);

  DECL_SECTION_NAME (decl) = build_string (len, string);
}

unsigned int
mips_hard_regno_nregs (regno, mode)
    int regno;
    enum machine_mode mode;
{
  if (! FP_REG_P (regno))
    return ((GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD);
  else
    return ((GET_MODE_SIZE (mode) + UNITS_PER_FPREG - 1) / UNITS_PER_FPREG);
}

int
mips_return_in_memory (type)
     tree type;
{
  /* Under the old (i.e., 32 and O64 ABIs) all BLKmode objects are
     returned in memory.  Under the new (N32 and 64-bit MIPS ABIs) small
     structures are returned in a register.  Objects with varying size
     must still be returned in memory, of course.  */

  if (mips_abi == ABI_32 || mips_abi == ABI_O64)
    return (TYPE_MODE (type) == BLKmode);
  else
    return ((int_size_in_bytes (type) > (2 * UNITS_PER_WORD))
	    || (int_size_in_bytes (type) == -1));
}

static int
mips_issue_rate ()
{
  switch (mips_tune)
    {
    case PROCESSOR_R3000: return 1;
    case PROCESSOR_R5400: return 2;
    case PROCESSOR_R5500: return 2;

    default:
      return 1;
    }

  abort ();

}

/* Implements TARGET_SCHED_USE_DFA_PIPELINE_INTERFACE.  Return true for
   processors that have a DFA pipeline description.  */

static int
mips_use_dfa_pipeline_interface ()
{
  switch (mips_tune)
    {
    case PROCESSOR_R5400:
    case PROCESSOR_R5500:
    case PROCESSOR_SR71000:
      return true;

    default:
      return false;
    }
}


const char *
mips_emit_prefetch (operands)
     rtx operands[];
{
 /* For the mips32/64 architectures the hint fields are arranged
    by operation (load/store) and locality (normal/streamed/retained).
    Irritatingly, numbers 2 and 3 are reserved leaving no simple
    algorithm for figuring the hint.  */

    int write = INTVAL (operands[1]);
    int locality = INTVAL (operands[2]);

    static const char * const alt[2][4] = {
	{
	 "pref\t4,%a0",
	 "pref\t0,%a0",
	 "pref\t0,%a0",
	 "pref\t6,%a0"
	},
	{
	 "pref\t5,%a0",
	 "pref\t1,%a0",
	 "pref\t1,%a0",
	 "pref\t7,%a0"
	}
    };

    return alt[write][locality];
}



#ifdef TARGET_IRIX6
/* Output assembly to switch to section NAME with attribute FLAGS.  */

static void
iris6_asm_named_section_1 (name, flags, align)
     const char *name;
     unsigned int flags;
     unsigned int align;
{
  unsigned int sh_type, sh_flags, sh_entsize;

  sh_flags = 0;
  if (!(flags & SECTION_DEBUG))
    sh_flags |= 2; /* SHF_ALLOC */
  if (flags & SECTION_WRITE)
    sh_flags |= 1; /* SHF_WRITE */
  if (flags & SECTION_CODE)
    sh_flags |= 4; /* SHF_EXECINSTR */
  if (flags & SECTION_SMALL)
    sh_flags |= 0x10000000; /* SHF_MIPS_GPREL */
  if (strcmp (name, ".debug_frame") == 0)
    sh_flags |= 0x08000000; /* SHF_MIPS_NOSTRIP */
  if (flags & SECTION_DEBUG)
    sh_type = 0x7000001e; /* SHT_MIPS_DWARF */
  else if (flags & SECTION_BSS)
    sh_type = 8; /* SHT_NOBITS */
  else
    sh_type = 1; /* SHT_PROGBITS */

  if (flags & SECTION_CODE)
    sh_entsize = 4;
  else
    sh_entsize = 0;

  fprintf (asm_out_file, "\t.section %s,%#x,%#x,%u,%u\n",
	   name, sh_type, sh_flags, sh_entsize, align);
}

static void
iris6_asm_named_section (name, flags)
     const char *name;
     unsigned int flags;
{
  if (TARGET_FILE_SWITCHING && (flags & SECTION_CODE))
    asm_out_file = asm_out_text_file;
  iris6_asm_named_section_1 (name, flags, 0);
}

/* In addition to emitting a .align directive, record the maximum
   alignment requested for the current section.  */

struct iris_section_align_entry
{
  const char *name;
  unsigned int log;
  unsigned int flags;
};

static htab_t iris_section_align_htab;
static FILE *iris_orig_asm_out_file;

static int
iris_section_align_entry_eq (p1, p2)
     const PTR p1;
     const PTR p2;
{
  const struct iris_section_align_entry *old = p1;
  const char *new = p2;

  return strcmp (old->name, new) == 0;
}

static hashval_t
iris_section_align_entry_hash (p)
     const PTR p;
{
  const struct iris_section_align_entry *old = p;
  return htab_hash_string (old->name);
}

void
iris6_asm_output_align (file, log)
     FILE *file;
     unsigned int log;
{
  const char *section = current_section_name ();
  struct iris_section_align_entry **slot, *entry;

  if (! section)
    abort ();

  slot = (struct iris_section_align_entry **)
    htab_find_slot_with_hash (iris_section_align_htab, section,
			      htab_hash_string (section), INSERT);
  entry = *slot;
  if (! entry)
    {
      entry = (struct iris_section_align_entry *)
	xmalloc (sizeof (struct iris_section_align_entry));
      *slot = entry;
      entry->name = section;
      entry->log = log;
      entry->flags = current_section_flags ();
    }
  else if (entry->log < log)
    entry->log = log;

  fprintf (file, "\t.align\t%u\n", log);
}

/* The Iris assembler does not record alignment from .align directives,
   but takes it from the first .section directive seen.  Play yet more
   file switching games so that we can emit a .section directive at the
   beginning of the file with the proper alignment attached.  */

void
iris6_asm_file_start (stream)
     FILE *stream;
{
  mips_asm_file_start (stream);

  iris_orig_asm_out_file = asm_out_file;
  stream = tmpfile ();
  asm_out_file = stream;
  asm_out_data_file = stream;
  if (! TARGET_FILE_SWITCHING)
    asm_out_text_file = stream;

  iris_section_align_htab = htab_create (31, iris_section_align_entry_hash,
					 iris_section_align_entry_eq, NULL);
}

static int
iris6_section_align_1 (slot, data)
     void **slot;
     void *data ATTRIBUTE_UNUSED;
{
  const struct iris_section_align_entry *entry
    = *(const struct iris_section_align_entry **) slot;

  iris6_asm_named_section_1 (entry->name, entry->flags, 1 << entry->log);
  return 1;
}

void
iris6_asm_file_end (stream)
     FILE *stream;
{
  /* Emit section directives with the proper alignment at the top of the
     real output file.  */
  asm_out_file = iris_orig_asm_out_file;
  htab_traverse (iris_section_align_htab, iris6_section_align_1, NULL);

  /* Copy the data emitted to the temp file to the real output file.  */
  copy_file_data (asm_out_file, stream);

  mips_asm_file_end (stream);
}
#endif /* TARGET_IRIX6 */

#include "gt-mips.h"
