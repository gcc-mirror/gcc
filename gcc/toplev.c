/* Top level of GNU C compiler
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */
#include "system.h"
#include <signal.h>

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#include "input.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-attr.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "output.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "expr.h"
#include "basic-block.h"
#include "intl.h"
#include "ggc.h"
#include "graph.h"
#include "loop.h"
#include "regs.h"
#include "timevar.h"
#include "diagnostic.h"
#include "ssa.h"
#include "params.h"
#include "reload.h"
#include "dwarf2asm.h"
#include "integrate.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "langhooks.h"
#include "cfglayout.h"

#if defined (DWARF2_UNWIND_INFO) || defined (DWARF2_DEBUGGING_INFO)
#include "dwarf2out.h"
#endif

#if defined(DBX_DEBUGGING_INFO) || defined(XCOFF_DEBUGGING_INFO)
#include "dbxout.h"
#endif

#ifdef SDB_DEBUGGING_INFO
#include "sdbout.h"
#endif

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"		/* Needed for external data
				   declarations for e.g. AIX 4.x.  */
#endif

/* Carry information from ASM_DECLARE_OBJECT_NAME
   to ASM_FINISH_DECLARE_OBJECT.  */

extern int size_directive_output;
extern tree last_assemble_variable_decl;

extern void reg_alloc PARAMS ((void));

static void general_init PARAMS ((char *));
static void parse_options_and_default_flags PARAMS ((int, char **));
static void do_compile PARAMS ((void));
static void process_options PARAMS ((void));
static void backend_init PARAMS ((void));
static int lang_dependent_init PARAMS ((const char *));
static void init_asm_output PARAMS ((const char *));
static void finalize PARAMS ((void));

static void set_target_switch PARAMS ((const char *));

static void crash_signal PARAMS ((int)) ATTRIBUTE_NORETURN;
static void compile_file PARAMS ((void));
static void display_help PARAMS ((void));
static void display_target_options PARAMS ((void));

static void decode_d_option PARAMS ((const char *));
static int decode_f_option PARAMS ((const char *));
static int decode_W_option PARAMS ((const char *));
static int decode_g_option PARAMS ((const char *));
static unsigned int independent_decode_option PARAMS ((int, char **));

static void print_version PARAMS ((FILE *, const char *));
static int print_single_switch PARAMS ((FILE *, int, int, const char *,
				      const char *, const char *,
				      const char *, const char *));
static void print_switch_values PARAMS ((FILE *, int, int, const char *,
				       const char *, const char *));

/* Nonzero to dump debug info whilst parsing (-dy option).  */
static int set_yydebug;

/* Length of line when printing switch values.  */
#define MAX_LINE 75

/* Name of program invoked, sans directories.  */

const char *progname;

/* Copy of arguments to toplev_main.  */
int save_argc;
char **save_argv;

/* Name of current original source file (what was input to cpp).
   This comes from each #-command in the actual input.  */

const char *input_filename;

/* Name of top-level original source file (what was input to cpp).
   This comes from the #-command at the beginning of the actual input.
   If there isn't any there, then this is the cc1 input file name.  */

const char *main_input_filename;

/* Current line number in real source file.  */

int lineno;

/* Nonzero if it is unsafe to create any new pseudo registers.  */
int no_new_pseudos;

/* Stack of currently pending input files.  */

struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
int input_file_stack_tick;

/* Name to use as base of names for dump output files.  */

const char *dump_base_name;

/* Name to use as a base for auxiliary output files.  */

const char *aux_base_name;

/* Format to use to print dumpfile index value */
#ifndef DUMPFILE_FORMAT
#define DUMPFILE_FORMAT ".%02d."
#endif

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm.h file
   and set by `-m...' switches.  Must be defined in rtlanal.c.  */

extern int target_flags;

/* A mask of target_flags that includes bit X if X was set or cleared
   on the command line.  */

int target_flags_explicit;

/* Debug hooks - dependent upon command line options.  */

const struct gcc_debug_hooks *debug_hooks = &do_nothing_debug_hooks;

/* Describes a dump file.  */

struct dump_file_info
{
  /* The unique extension to apply, e.g. ".jump".  */
  const char *const extension;

  /* The -d<c> character that enables this dump file.  */
  char const debug_switch;

  /* True if there is a corresponding graph dump file.  */
  char const graph_dump_p;

  /* True if the user selected this dump.  */
  char enabled;

  /* True if the files have been initialized (ie truncated).  */
  char initialized;
};

/* Enumerate the extant dump files.  */

enum dump_file_index
{
  DFI_rtl,
  DFI_sibling,
  DFI_eh,
  DFI_jump,
  DFI_ssa,
  DFI_ssa_ccp,
  DFI_ssa_dce,
  DFI_ussa,
  DFI_null,
  DFI_cse,
  DFI_addressof,
  DFI_gcse,
  DFI_loop,
  DFI_cfg,
  DFI_bp,
  DFI_ce1,
  DFI_tracer,
  DFI_cse2,
  DFI_life,
  DFI_combine,
  DFI_ce2,
  DFI_regmove,
  DFI_sched,
  DFI_lreg,
  DFI_greg,
  DFI_postreload,
  DFI_flow2,
  DFI_peephole2,
  DFI_rnreg,
  DFI_ce3,
  DFI_sched2,
  DFI_stack,
  DFI_bbro,
  DFI_mach,
  DFI_dbr,
  DFI_MAX
};

/* Describes all the dump files.  Should be kept in order of the
   pass and in sync with dump_file_index above.

   Remaining -d letters:

	"              o q         "
	"       H JK   OPQ  TUV  YZ"
*/

static struct dump_file_info dump_file[DFI_MAX] =
{
  { "rtl",	'r', 0, 0, 0 },
  { "sibling",  'i', 0, 0, 0 },
  { "eh",	'h', 0, 0, 0 },
  { "jump",	'j', 0, 0, 0 },
  { "ssa",	'e', 1, 0, 0 },
  { "ssaccp",	'W', 1, 0, 0 },
  { "ssadce",	'X', 1, 0, 0 },
  { "ussa",	'e', 1, 0, 0 },	/* Yes, duplicate enable switch.  */
  { "null",	'u', 0, 0, 0 },
  { "cse",	's', 0, 0, 0 },
  { "addressof", 'F', 0, 0, 0 },
  { "gcse",	'G', 1, 0, 0 },
  { "loop",	'L', 1, 0, 0 },
  { "cfg",	'f', 1, 0, 0 },
  { "bp",	'b', 1, 0, 0 },
  { "ce1",	'C', 1, 0, 0 },
  { "tracer",	'T', 1, 0, 0 },
  { "cse2",	't', 1, 0, 0 },
  { "life",	'f', 1, 0, 0 },	/* Yes, duplicate enable switch.  */
  { "combine",	'c', 1, 0, 0 },
  { "ce2",	'C', 1, 0, 0 },
  { "regmove",	'N', 1, 0, 0 },
  { "sched",	'S', 1, 0, 0 },
  { "lreg",	'l', 1, 0, 0 },
  { "greg",	'g', 1, 0, 0 },
  { "postreload", 'o', 1, 0, 0 },
  { "flow2",	'w', 1, 0, 0 },
  { "peephole2", 'z', 1, 0, 0 },
  { "rnreg",	'n', 1, 0, 0 },
  { "ce3",	'E', 1, 0, 0 },
  { "sched2",	'R', 1, 0, 0 },
  { "stack",	'k', 1, 0, 0 },
  { "bbro",	'B', 1, 0, 0 },
  { "mach",	'M', 1, 0, 0 },
  { "dbr",	'd', 0, 0, 0 },
};

static int open_dump_file PARAMS ((enum dump_file_index, tree));
static void close_dump_file PARAMS ((enum dump_file_index,
				     void (*) (FILE *, rtx), rtx));

/* Other flags saying which kinds of debugging dump have been requested.  */

int rtl_dump_and_exit;
int flag_print_asm_name;
static int version_flag;
static char *filename;
enum graph_dump_types graph_dump_format;

/* Name for output file of assembly code, specified with -o.  */

char *asm_file_name;

/* Value of the -G xx switch, and whether it was passed or not.  */
int g_switch_value;
int g_switch_set;

/* Type(s) of debugging information we are producing (if any).
   See flags.h for the definitions of the different possible
   types of debugging information.  */
enum debug_info_type write_symbols = NO_DEBUG;

/* Level of debugging information we are producing.  See flags.h
   for the definitions of the different possible levels.  */
enum debug_info_level debug_info_level = DINFO_LEVEL_NONE;

/* Nonzero means use GNU-only extensions in the generated symbolic
   debugging information.  */
/* Currently, this only has an effect when write_symbols is set to
   DBX_DEBUG, XCOFF_DEBUG, or DWARF_DEBUG.  */
int use_gnu_debug_info_extensions = 0;

/* Nonzero means do optimizations.  -O.
   Particular numeric values stand for particular amounts of optimization;
   thus, -O2 stores 2 here.  However, the optimizations beyond the basic
   ones are not controlled directly by this variable.  Instead, they are
   controlled by individual `flag_...' variables that are defaulted
   based on this variable.  */

int optimize = 0;

/* Nonzero means optimize for size.  -Os.
   The only valid values are zero and nonzero. When optimize_size is
   nonzero, optimize defaults to 2, but certain individual code
   bloating optimizations are disabled.  */

int optimize_size = 0;

/* Nonzero if we should exit after parsing options.  */
static int exit_after_options = 0;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to the FUNC_BEGIN label of the current function, or NULL_TREE
   if none.  */
tree current_function_func_begin_label;

/* Nonzero if doing dwarf2 duplicate elimination.  */

int flag_eliminate_dwarf2_dups = 0;

/* Nonzero if generating code to do profiling.  */

int profile_flag = 0;

/* Nonzero if generating code to profile program flow graph arcs.  */

int profile_arc_flag = 0;

/* Nonzero if generating info for gcov to calculate line test coverage.  */

int flag_test_coverage = 0;

/* Nonzero indicates that branch taken probabilities should be calculated.  */

int flag_branch_probabilities = 0;

/* Nonzero if basic blocks should be reordered.  */

int flag_reorder_blocks = 0;

/* Nonzero if functions should be reordered.  */

int flag_reorder_functions = 0;

/* Nonzero if registers should be renamed.  */

int flag_rename_registers = 0;
int flag_cprop_registers = 0;

/* Nonzero for -pedantic switch: warn about anything
   that standard spec forbids.  */

int pedantic = 0;

/* Temporarily suppress certain warnings.
   This is set while reading code from a system header file.  */

int in_system_header = 0;

/* Don't print functions as they are compiled.  -quiet.  */

int quiet_flag = 0;

/* Print times taken by the various passes.  -ftime-report.  */

int time_report = 0;

/* Print memory still in use at end of compilation (which may have little
   to do with peak memory consumption).  -fmem-report.  */

int mem_report = 0;

/* Non-zero means to collect statistics which might be expensive
   and to print them when we are done.  */
int flag_detailed_statistics = 0;


/* -f flags.  */

/* Nonzero means `char' should be signed.  */

int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */

int flag_short_enums;

/* Nonzero for -fcaller-saves: allocate values in regs that need to
   be saved across function calls, if that produces overall better code.
   Optional now, so people can test it.  */

#ifdef DEFAULT_CALLER_SAVES
int flag_caller_saves = 1;
#else
int flag_caller_saves = 0;
#endif

/* Nonzero if structures and unions should be returned in memory.

   This should only be defined if compatibility with another compiler or
   with an ABI is needed, because it results in slower code.  */

#ifndef DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 1
#endif

/* Nonzero for -fpcc-struct-return: return values the same way PCC does.  */

int flag_pcc_struct_return = DEFAULT_PCC_STRUCT_RETURN;

/* Nonzero for -fforce-mem: load memory value into a register
   before arithmetic on it.  This makes better cse but slower compilation.  */

int flag_force_mem = 0;

/* Nonzero for -fforce-addr: load memory address into a register before
   reference to memory.  This makes better cse but slower compilation.  */

int flag_force_addr = 0;

/* Nonzero for -fdefer-pop: don't pop args after each function call;
   instead save them up to pop many calls' args with one insns.  */

int flag_defer_pop = 0;

/* Nonzero for -ffloat-store: don't allocate floats and doubles
   in extended-precision registers.  */

int flag_float_store = 0;

/* Nonzero for -fcse-follow-jumps:
   have cse follow jumps to do a more extensive job.  */

int flag_cse_follow_jumps;

/* Nonzero for -fcse-skip-blocks:
   have cse follow a branch around a block.  */
int flag_cse_skip_blocks;

/* Nonzero for -fexpensive-optimizations:
   perform miscellaneous relatively-expensive optimizations.  */
int flag_expensive_optimizations;

/* Nonzero for -fthread-jumps:
   have jump optimize output of loop.  */

int flag_thread_jumps;

/* Nonzero enables strength-reduction in loop.c.  */

int flag_strength_reduce = 0;

/* Nonzero enables loop unrolling in unroll.c.  Only loops for which the
   number of iterations can be calculated at compile-time (UNROLL_COMPLETELY,
   UNROLL_MODULO) or at run-time (preconditioned to be UNROLL_MODULO) are
   unrolled.  */

int flag_unroll_loops;

/* Nonzero enables loop unrolling in unroll.c.  All loops are unrolled.
   This is generally not a win.  */

int flag_unroll_all_loops;

/* Nonzero enables prefetch optimizations for arrays in loops.  */

int flag_prefetch_loop_arrays;

/* Nonzero forces all invariant computations in loops to be moved
   outside the loop.  */

int flag_move_all_movables = 0;

/* Nonzero forces all general induction variables in loops to be
   strength reduced.  */

int flag_reduce_all_givs = 0;

/* Nonzero to perform full register move optimization passes.  This is the
   default for -O2.  */

int flag_regmove = 0;

/* Nonzero for -fwritable-strings:
   store string constants in data segment and don't uniquize them.  */

int flag_writable_strings = 0;

/* Nonzero means don't put addresses of constant functions in registers.
   Used for compiling the Unix kernel, where strange substitutions are
   done on the assembly output.  */

int flag_no_function_cse = 0;

/* Nonzero for -fomit-frame-pointer:
   don't make a frame pointer in simple functions that don't require one.  */

int flag_omit_frame_pointer = 0;

/* Nonzero means place each function into its own section on those platforms
   which support arbitrary section names and unlimited numbers of sections.  */

int flag_function_sections = 0;

/* ... and similar for data.  */

int flag_data_sections = 0;

/* Nonzero to inhibit use of define_optimization peephole opts.  */

int flag_no_peephole = 0;

/* Nonzero allows GCC to optimize sibling and tail recursive calls.  */

int flag_optimize_sibling_calls = 0;

/* Nonzero means the front end generally wants `errno' maintained by math
   operations, like built-in SQRT.  */

int flag_errno_math = 1;

/* Nonzero means that unsafe floating-point math optimizations are allowed
   for the sake of speed.  IEEE compliance is not guaranteed, and operations
   are allowed to assume that their arguments and results are "normal"
   (e.g., nonnegative for SQRT).  */

int flag_unsafe_math_optimizations = 0;

/* Nonzero means that no NaNs or +-Infs are expected.  */

int flag_finite_math_only = 0;

/* Zero means that floating-point math operations cannot generate a
   (user-visible) trap.  This is the case, for example, in nonstop
   IEEE 754 arithmetic.  Trapping conditions include division by zero,
   overflow, underflow, invalid and inexact, but does not include 
   operations on signaling NaNs (see below).  */

int flag_trapping_math = 1;

/* Nonzero means disable transformations observable by signaling NaNs.
   This option implies that any operation on a IEEE signaling NaN can
   generate a (user-visible) trap.  */

int flag_signaling_nans = 0;

/* 0 means straightforward implementation of complex divide acceptable.
   1 means wide ranges of inputs must work for complex divide.
   2 means C99-like requirements for complex divide (not yet implemented).  */

int flag_complex_divide_method = 0;

/* Nonzero means all references through pointers are volatile.  */

int flag_volatile;

/* Nonzero means treat all global and extern variables as volatile.  */

int flag_volatile_global;

/* Nonzero means treat all static variables as volatile.  */

int flag_volatile_static;

/* Nonzero means just do syntax checking; don't output anything.  */

int flag_syntax_only = 0;

/* Nonzero means perform global cse.  */

static int flag_gcse;

/* Nonzero means perform loop optimizer.  */

static int flag_loop_optimize;

/* Nonzero means perform crossjumping.  */

static int flag_crossjumping;

/* Nonzero means perform if conversion.  */

static int flag_if_conversion;

/* Nonzero means perform if conversion after reload.  */

static int flag_if_conversion2;

/* Nonzero means to use global dataflow analysis to eliminate
   useless null pointer tests.  */

static int flag_delete_null_pointer_checks;

/* Nonzero means to do the enhanced load motion during gcse, which trys
   to hoist loads by not killing them when a store to the same location
   is seen.  */

int flag_gcse_lm = 1;

/* Nonzero means to perform store motion after gcse, which will try to
   move stores closer to the exit block.  Its not very effective without
   flag_gcse_lm.  */

int flag_gcse_sm = 1;

/* Nonzero means to rerun cse after loop optimization.  This increases
   compilation time about 20% and picks up a few more common expressions.  */

static int flag_rerun_cse_after_loop;

/* Nonzero means to run loop optimizations twice.  */

int flag_rerun_loop_opt;

/* Nonzero for -finline-functions: ok to inline functions that look like
   good inline candidates.  */

int flag_inline_functions;

/* Nonzero for -fkeep-inline-functions: even if we make a function
   go inline everywhere, keep its definition around for debugging
   purposes.  */

int flag_keep_inline_functions;

/* Nonzero means that functions will not be inlined.  */

int flag_no_inline = 2;

/* Nonzero means that we don't want inlining by virtue of -fno-inline,
   not just because the tree inliner turned us off.  */

int flag_really_no_inline = 2;

/* Nonzero means that we should emit static const variables
   regardless of whether or not optimization is turned on.  */

int flag_keep_static_consts = 1;

/* Nonzero means we should be saving declaration info into a .X file.  */

int flag_gen_aux_info = 0;

/* Specified name of aux-info file.  */

static char *aux_info_file_name;

/* Nonzero means make the text shared if supported.  */

int flag_shared_data;

/* Nonzero means schedule into delayed branch slots if supported.  */

int flag_delayed_branch;

/* Nonzero if we are compiling pure (sharable) code.
   Value is 1 if we are doing "small" pic; value is 2 if we're doing
   "large" pic.  */

int flag_pic;

/* Set to the default thread-local storage (tls) model to use.  */

enum tls_model flag_tls_default = TLS_MODEL_GLOBAL_DYNAMIC;

/* Nonzero means generate extra code for exception handling and enable
   exception handling.  */

int flag_exceptions;

/* Nonzero means generate frame unwind info table when supported.  */

int flag_unwind_tables = 0;

/* Nonzero means generate frame unwind info table exact at each insn boundary */

int flag_asynchronous_unwind_tables = 0;

/* Nonzero means don't place uninitialized global data in common storage
   by default.  */

int flag_no_common;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */

int flag_pedantic_errors = 0;

/* flag_schedule_insns means schedule insns within basic blocks (before
   local_alloc).
   flag_schedule_insns_after_reload means schedule insns after
   global_alloc.  */

int flag_schedule_insns = 0;
int flag_schedule_insns_after_reload = 0;

/* The following flags have effect only for scheduling before register
   allocation:

   flag_schedule_interblock means schedule insns accross basic blocks.
   flag_schedule_speculative means allow speculative motion of non-load insns.
   flag_schedule_speculative_load means allow speculative motion of some
   load insns.
   flag_schedule_speculative_load_dangerous allows speculative motion of more
   load insns.  */

int flag_schedule_interblock = 1;
int flag_schedule_speculative = 1;
int flag_schedule_speculative_load = 0;
int flag_schedule_speculative_load_dangerous = 0;

int flag_single_precision_constant;

/* flag_branch_on_count_reg means try to replace add-1,compare,branch tupple
   by a cheaper branch on a count register.  */
int flag_branch_on_count_reg = 1;

/* -finhibit-size-directive inhibits output of .size for ELF.
   This is used only for compiling crtstuff.c,
   and it may be extended to other effects
   needed for crtstuff.c on other systems.  */
int flag_inhibit_size_directive = 0;

/* -fverbose-asm causes extra commentary information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).
   -fno-verbose-asm, the default, causes the extra information
   to be omitted and is useful when comparing two assembler files.  */

int flag_verbose_asm = 0;

/* -dA causes debug commentary information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).
   Currently, this switch is only used by dwarfout.c; however, it is intended
   to be a catchall for printing debug information in the assembler file.  */

int flag_debug_asm = 0;

/* -dP causes the rtl to be emitted as a comment in assembly.  */

int flag_dump_rtl_in_asm = 0;

/* -fgnu-linker specifies use of the GNU linker for initializations.
   (Or, more generally, a linker that handles initializations.)
   -fno-gnu-linker says that collect2 will be used.  */
#ifdef USE_COLLECT2
int flag_gnu_linker = 0;
#else
int flag_gnu_linker = 1;
#endif

/* Nonzero means put zero initialized data in the bss section.  */
int flag_zero_initialized_in_bss = 1;

/* Enable SSA.  */
int flag_ssa = 0;

/* Enable ssa conditional constant propagation.  */
int flag_ssa_ccp = 0;

/* Enable ssa aggressive dead code elimination.  */
int flag_ssa_dce = 0;

/* Tag all structures with __attribute__(packed).  */
int flag_pack_struct = 0;

/* Emit code to check for stack overflow; also may cause large objects
   to be allocated dynamically.  */
int flag_stack_check;

/* When non-NULL, indicates that whenever space is allocated on the
   stack, the resulting stack pointer must not pass this
   address---that is, for stacks that grow downward, the stack pointer
   must always be greater than or equal to this address; for stacks
   that grow upward, the stack pointer must be less than this address.
   At present, the rtx may be either a REG or a SYMBOL_REF, although
   the support provided depends on the backend.  */
rtx stack_limit_rtx;

/* 0 if pointer arguments may alias each other.  True in C.
   1 if pointer arguments may not alias each other but may alias
   global variables.
   2 if pointer arguments may not alias each other and may not
   alias global variables.  True in Fortran.
   This defaults to 0 for C.  */
int flag_argument_noalias = 0;

/* Nonzero if we should do (language-dependent) alias analysis.
   Typically, this analysis will assume that expressions of certain
   types do not alias expressions of certain other types.  Only used
   if alias analysis (in general) is enabled.  */
int flag_strict_aliasing = 0;

/* Instrument functions with calls at entry and exit, for profiling.  */
int flag_instrument_function_entry_exit = 0;

/* Nonzero means ignore `#ident' directives.  0 means handle them.
   On SVR4 targets, it also controls whether or not to emit a
   string identifying the compiler.  */

int flag_no_ident = 0;

/* This will perform a peephole pass before sched2.  */
int flag_peephole2 = 0;

/* This will try to guess branch probabilities.  */
int flag_guess_branch_prob = 0;

/* -fcheck-bounds causes gcc to generate array bounds checks.
   For C, C++, ObjC: defaults to off.
   For Java: defaults to on.
   For Fortran: defaults to off.  */
int flag_bounds_check = 0;

/* This will attempt to merge constant section constants, if 1 only
   string constants and constants from constant pool, if 2 also constant
   variables.  */
int flag_merge_constants = 1;

/* If one, renumber instruction UIDs to reduce the number of
   unused UIDs if there are a lot of instructions.  If greater than
   one, unconditionally renumber instruction UIDs.  */
int flag_renumber_insns = 1;

/* If nonzero, use the graph coloring register allocator.  */
int flag_new_regalloc = 0;

/* Nonzero if we perform superblock formation.  */

int flag_tracer = 0;

/* Values of the -falign-* flags: how much to align labels in code.
   0 means `use default', 1 means `don't align'.
   For each variable, there is an _log variant which is the power
   of two not less than the variable, for .align output.  */

int align_loops;
int align_loops_log;
int align_loops_max_skip;
int align_jumps;
int align_jumps_log;
int align_jumps_max_skip;
int align_labels;
int align_labels_log;
int align_labels_max_skip;
int align_functions;
int align_functions_log;

/* Like align_functions_log above, but used by front-ends to force the
   minimum function alignment.  Zero means no alignment is forced.  */
int force_align_functions_log;

/* Table of supported debugging formats.  */
static const struct
{
  const char *const arg;
  /* Since PREFERRED_DEBUGGING_TYPE isn't necessarily a
     constant expression, we use NO_DEBUG in its place.  */
  const enum debug_info_type debug_type;
  const int use_extensions_p;
  const char *const description;
} *da,
debug_args[] =
{
  { "",       NO_DEBUG, DEFAULT_GDB_EXTENSIONS,
    N_("Generate debugging info in default format") },
  { "gdb",    NO_DEBUG, 1, N_("Generate debugging info in default extended format") },
#ifdef DBX_DEBUGGING_INFO
  { "stabs",  DBX_DEBUG, 0, N_("Generate STABS format debug info") },
  { "stabs+", DBX_DEBUG, 1, N_("Generate extended STABS format debug info") },
#endif
#ifdef DWARF_DEBUGGING_INFO
  { "dwarf",  DWARF_DEBUG, 0, N_("Generate DWARF-1 format debug info") },
  { "dwarf+", DWARF_DEBUG, 1,
    N_("Generate extended DWARF-1 format debug info") },
#endif
#ifdef DWARF2_DEBUGGING_INFO
  { "dwarf-2", DWARF2_DEBUG, 0, N_("Generate DWARF-2 debug info") },
#endif
#ifdef XCOFF_DEBUGGING_INFO
  { "xcoff",  XCOFF_DEBUG, 0, N_("Generate XCOFF format debug info") },
  { "xcoff+", XCOFF_DEBUG, 1, N_("Generate extended XCOFF format debug info") },
#endif
#ifdef SDB_DEBUGGING_INFO
  { "coff", SDB_DEBUG, 0, N_("Generate COFF format debug info") },
#endif
#ifdef VMS_DEBUGGING_INFO
  { "vms", VMS_DEBUG, 0, N_("Generate VMS format debug info") },
#endif
  { 0, 0, 0, 0 }
};

typedef struct
{
  const char *const string;
  int *const variable;
  const int on_value;
  const char *const description;
}
lang_independent_options;

int flag_trapv = 0;

/* Add or remove a leading underscore from user symbols.  */
int flag_leading_underscore = -1;

/* The user symbol prefix after having resolved same.  */
const char *user_label_prefix;

static const param_info lang_independent_params[] = {
#define DEFPARAM(ENUM, OPTION, HELP, DEFAULT) \
  { OPTION, DEFAULT, HELP },
#include "params.def"
#undef DEFPARAM
  { NULL, 0, NULL }
};

/* Table of language-independent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

static const lang_independent_options f_options[] =
{
  {"eliminate-dwarf2-dups", &flag_eliminate_dwarf2_dups, 1,
   N_("Perform DWARF2 duplicate elimination") },
  {"float-store", &flag_float_store, 1,
   N_("Do not store floats in registers") },
  {"volatile", &flag_volatile, 1,
   N_("Consider all mem refs through pointers as volatile") },
  {"volatile-global", &flag_volatile_global, 1,
   N_("Consider all mem refs to global data to be volatile") },
  {"volatile-static", &flag_volatile_static, 1,
   N_("Consider all mem refs to static data to be volatile") },
  {"defer-pop", &flag_defer_pop, 1,
   N_("Defer popping functions args from stack until later") },
  {"omit-frame-pointer", &flag_omit_frame_pointer, 1,
   N_("When possible do not generate stack frames") },
  {"optimize-sibling-calls", &flag_optimize_sibling_calls, 1,
   N_("Optimize sibling and tail recursive calls") },
  {"tracer", &flag_tracer, 1,
   N_("Perform superblock formation via tail duplication") },
  {"cse-follow-jumps", &flag_cse_follow_jumps, 1,
   N_("When running CSE, follow jumps to their targets") },
  {"cse-skip-blocks", &flag_cse_skip_blocks, 1,
   N_("When running CSE, follow conditional jumps") },
  {"expensive-optimizations", &flag_expensive_optimizations, 1,
   N_("Perform a number of minor, expensive optimizations") },
  {"thread-jumps", &flag_thread_jumps, 1,
   N_("Perform jump threading optimizations") },
  {"strength-reduce", &flag_strength_reduce, 1,
   N_("Perform strength reduction optimizations") },
  {"unroll-loops", &flag_unroll_loops, 1,
   N_("Perform loop unrolling when iteration count is known") },
  {"unroll-all-loops", &flag_unroll_all_loops, 1,
   N_("Perform loop unrolling for all loops") },
  {"prefetch-loop-arrays", &flag_prefetch_loop_arrays, 1,
   N_("Generate prefetch instructions, if available, for arrays in loops") },
  {"move-all-movables", &flag_move_all_movables, 1,
   N_("Force all loop invariant computations out of loops") },
  {"reduce-all-givs", &flag_reduce_all_givs, 1,
   N_("Strength reduce all loop general induction variables") },
  {"writable-strings", &flag_writable_strings, 1,
   N_("Store strings in writable data section") },
  {"peephole", &flag_no_peephole, 0,
   N_("Enable machine specific peephole optimizations") },
  {"force-mem", &flag_force_mem, 1,
   N_("Copy memory operands into registers before using") },
  {"force-addr", &flag_force_addr, 1,
   N_("Copy memory address constants into regs before using") },
  {"function-cse", &flag_no_function_cse, 0,
   N_("Allow function addresses to be held in registers") },
  {"inline-functions", &flag_inline_functions, 1,
   N_("Integrate simple functions into their callers") },
  {"keep-inline-functions", &flag_keep_inline_functions, 1,
   N_("Generate code for funcs even if they are fully inlined") },
  {"inline", &flag_no_inline, 0,
   N_("Pay attention to the 'inline' keyword") },
  {"keep-static-consts", &flag_keep_static_consts, 1,
   N_("Emit static const variables even if they are not used") },
  {"syntax-only", &flag_syntax_only, 1,
   N_("Check for syntax errors, then stop") },
  {"shared-data", &flag_shared_data, 1,
   N_("Mark data as shared rather than private") },
  {"caller-saves", &flag_caller_saves, 1,
   N_("Enable saving registers around function calls") },
  {"pcc-struct-return", &flag_pcc_struct_return, 1,
   N_("Return 'short' aggregates in memory, not registers") },
  {"reg-struct-return", &flag_pcc_struct_return, 0,
   N_("Return 'short' aggregates in registers") },
  {"delayed-branch", &flag_delayed_branch, 1,
   N_("Attempt to fill delay slots of branch instructions") },
  {"gcse", &flag_gcse, 1,
   N_("Perform the global common subexpression elimination") },
  {"gcse-lm", &flag_gcse_lm, 1,
   N_("Perform enhanced load motion during global subexpression elimination") },
  {"gcse-sm", &flag_gcse_sm, 1,
   N_("Perform store motion after global subexpression elimination") },
  {"loop-optimize", &flag_loop_optimize, 1,
   N_("Perform the loop optimizations") },
  {"crossjumping", &flag_crossjumping, 1,
   N_("Perform cross-jumping optimization") },
  {"if-conversion", &flag_if_conversion, 1,
   N_("Perform conversion of conditional jumps to branchless equivalents") },
  {"if-conversion2", &flag_if_conversion2, 1,
   N_("Perform conversion of conditional jumps to conditional execution") },
  {"rerun-cse-after-loop", &flag_rerun_cse_after_loop, 1,
   N_("Run CSE pass after loop optimizations") },
  {"rerun-loop-opt", &flag_rerun_loop_opt, 1,
   N_("Run the loop optimizer twice") },
  {"delete-null-pointer-checks", &flag_delete_null_pointer_checks, 1,
   N_("Delete useless null pointer checks") },
  {"schedule-insns", &flag_schedule_insns, 1,
   N_("Reschedule instructions before register allocation") },
  {"schedule-insns2", &flag_schedule_insns_after_reload, 1,
   N_("Reschedule instructions after register allocation") },
  {"sched-interblock",&flag_schedule_interblock, 1,
   N_("Enable scheduling across basic blocks") },
  {"sched-spec",&flag_schedule_speculative, 1,
   N_("Allow speculative motion of non-loads") },
  {"sched-spec-load",&flag_schedule_speculative_load, 1,
   N_("Allow speculative motion of some loads") },
  {"sched-spec-load-dangerous",&flag_schedule_speculative_load_dangerous, 1,
   N_("Allow speculative motion of more loads") },
  {"branch-count-reg",&flag_branch_on_count_reg, 1,
   N_("Replace add,compare,branch with branch on count reg") },
  {"pic", &flag_pic, 1,
   N_("Generate position independent code, if possible") },
  {"PIC", &flag_pic, 2, ""},
  {"exceptions", &flag_exceptions, 1,
   N_("Enable exception handling") },
  {"unwind-tables", &flag_unwind_tables, 1,
   N_("Just generate unwind tables for exception handling") },
  {"asynchronous-unwind-tables", &flag_asynchronous_unwind_tables, 1,
   N_("Generate unwind tables exact at each instruction boundary") },
  {"non-call-exceptions", &flag_non_call_exceptions, 1,
   N_("Support synchronous non-call exceptions") },
  {"profile-arcs", &profile_arc_flag, 1,
   N_("Insert arc based program profiling code") },
  {"test-coverage", &flag_test_coverage, 1,
   N_("Create data files needed by gcov") },
  {"branch-probabilities", &flag_branch_probabilities, 1,
   N_("Use profiling information for branch probabilities") },
  {"profile", &profile_flag, 1,
   N_("Enable basic program profiling code") },
  {"reorder-blocks", &flag_reorder_blocks, 1,
   N_("Reorder basic blocks to improve code placement") },
  {"reorder-functions", &flag_reorder_functions, 1,
   N_("Reorder functions to improve code placement") },
  {"rename-registers", &flag_rename_registers, 1,
   N_("Do the register renaming optimization pass") },
  {"cprop-registers", &flag_cprop_registers, 1,
   N_("Do the register copy-propagation optimization pass") },
  {"common", &flag_no_common, 0,
   N_("Do not put uninitialized globals in the common section") },
  {"inhibit-size-directive", &flag_inhibit_size_directive, 1,
   N_("Do not generate .size directives") },
  {"function-sections", &flag_function_sections, 1,
   N_("place each function into its own section") },
  {"data-sections", &flag_data_sections, 1,
   N_("place data items into their own section") },
  {"verbose-asm", &flag_verbose_asm, 1,
   N_("Add extra commentary to assembler output") },
  {"gnu-linker", &flag_gnu_linker, 1,
   N_("Output GNU ld formatted global initializers") },
  {"regmove", &flag_regmove, 1,
   N_("Enables a register move optimization") },
  {"optimize-register-move", &flag_regmove, 1,
   N_("Do the full regmove optimization pass") },
  {"pack-struct", &flag_pack_struct, 1,
   N_("Pack structure members together without holes") },
  {"stack-check", &flag_stack_check, 1,
   N_("Insert stack checking code into the program") },
  {"argument-alias", &flag_argument_noalias, 0,
   N_("Specify that arguments may alias each other & globals") },
  {"argument-noalias", &flag_argument_noalias, 1,
   N_("Assume arguments may alias globals but not each other") },
  {"argument-noalias-global", &flag_argument_noalias, 2,
   N_("Assume arguments do not alias each other or globals") },
  {"strict-aliasing", &flag_strict_aliasing, 1,
   N_("Assume strict aliasing rules apply") },
  {"align-loops", &align_loops, 0,
   N_("Align the start of loops") },
  {"align-jumps", &align_jumps, 0,
   N_("Align labels which are only reached by jumping") },
  {"align-labels", &align_labels, 0,
   N_("Align all labels") },
  {"align-functions", &align_functions, 0,
   N_("Align the start of functions") },
  {"merge-constants", &flag_merge_constants, 1,
   N_("Attempt to merge identical constants across compilation units") },
  {"merge-all-constants", &flag_merge_constants, 2,
   N_("Attempt to merge identical constants and constant variables") },
  {"dump-unnumbered", &flag_dump_unnumbered, 1,
   N_("Suppress output of instruction numbers and line number notes in debugging dumps") },
  {"instrument-functions", &flag_instrument_function_entry_exit, 1,
   N_("Instrument function entry/exit with profiling calls") },
  {"zero-initialized-in-bss", &flag_zero_initialized_in_bss, 1,
   N_("Put zero initialized data in the bss section") },
  {"ssa", &flag_ssa, 1,
   N_("Enable SSA optimizations") },
  {"ssa-ccp", &flag_ssa_ccp, 1,
   N_("Enable SSA conditional constant propagation") },
  {"ssa-dce", &flag_ssa_dce, 1,
   N_("Enable aggressive SSA dead code elimination") },
  {"leading-underscore", &flag_leading_underscore, 1,
   N_("External symbols have a leading underscore") },
  {"ident", &flag_no_ident, 0,
   N_("Process #ident directives") },
  { "peephole2", &flag_peephole2, 1,
   N_("Enables an rtl peephole pass run before sched2") },
  {"finite-math-only", &flag_finite_math_only, 1,
   N_("Assume no NaNs or +-Infs are generated") },
  { "guess-branch-probability", &flag_guess_branch_prob, 1,
   N_("Enables guessing of branch probabilities") },
  {"math-errno", &flag_errno_math, 1,
   N_("Set errno after built-in math functions") },
  {"trapping-math", &flag_trapping_math, 1,
   N_("Floating-point operations can trap") },
  {"unsafe-math-optimizations", &flag_unsafe_math_optimizations, 1,
   N_("Allow math optimizations that may violate IEEE or ANSI standards") },
  {"signaling-nans", &flag_signaling_nans, 1,
   N_("Disable optimizations observable by IEEE signaling NaNs") },
  {"bounds-check", &flag_bounds_check, 1,
   N_("Generate code to check bounds before indexing arrays") },
  {"single-precision-constant", &flag_single_precision_constant, 1,
   N_("Convert floating point constant to single precision constant") },
  {"time-report", &time_report, 1,
   N_("Report time taken by each compiler pass at end of run") },
  {"mem-report", &mem_report, 1,
   N_("Report on permanent memory allocation at end of run") },
  { "trapv", &flag_trapv, 1,
   N_("Trap for signed overflow in addition / subtraction / multiplication") },
  { "new-ra", &flag_new_regalloc, 1,
   N_("Use graph coloring register allocation.") },
};

/* Table of language-specific options.  */

static const struct lang_opt
{
  const char *const option;
  const char *const description;
}
documented_lang_options[] =
{
  /* In order not to overload the --help output, the convention
     used here is to only describe those options which are not
     enabled by default.  */

  { "-ansi",
    N_("Compile just for ISO C90") },
  { "-std= ",
    N_("Determine language standard") },

  { "-fsigned-bitfields", "" },
  { "-funsigned-bitfields",
    N_("Make bit-fields by unsigned by default") },
  { "-fno-signed-bitfields", "" },
  { "-fno-unsigned-bitfields","" },
  { "-fsigned-char",
    N_("Make 'char' be signed by default") },
  { "-funsigned-char",
    N_("Make 'char' be unsigned by default") },
  { "-fno-signed-char", "" },
  { "-fno-unsigned-char", "" },

  { "-fasm", "" },
  { "-fno-asm",
    N_("Do not recognize the 'asm' keyword") },
  { "-fbuiltin", "" },
  { "-fno-builtin",
    N_("Do not recognize any built in functions") },
  { "-fhosted",
    N_("Assume normal C execution environment") },
  { "-fno-hosted", "" },
  { "-ffreestanding",
    N_("Assume that standard libraries & main might not exist") },
  { "-fno-freestanding", "" },
  { "-fcond-mismatch",
    N_("Allow different types as args of ? operator") },
  { "-fno-cond-mismatch", "" },
  { "-fdollars-in-identifiers",
    N_("Allow the use of $ inside identifiers") },
  { "-fno-dollars-in-identifiers", "" },
  { "-fpreprocessed", "" },
  { "-fno-preprocessed", "" },
  { "-fshort-double",
    N_("Use the same size for double as for float") },
  { "-fno-short-double", "" },
  { "-fshort-enums",
    N_("Use the smallest fitting integer to hold enums") },
  { "-fno-short-enums", "" },
  { "-fshort-wchar",
    N_("Override the underlying type for wchar_t to `unsigned short'") },
  { "-fno-short-wchar", "" },

  { "-Wall",
    N_("Enable most warning messages") },
  { "-Wbad-function-cast",
    N_("Warn about casting functions to incompatible types") },
  { "-Wno-bad-function-cast", "" },
  { "-Wmissing-format-attribute",
    N_("Warn about functions which might be candidates for format attributes") },
  { "-Wno-missing-format-attribute", "" },
  { "-Wcast-qual",
    N_("Warn about casts which discard qualifiers") },
  { "-Wno-cast-qual", "" },
  { "-Wchar-subscripts",
    N_("Warn about subscripts whose type is 'char'") },
  { "-Wno-char-subscripts", "" },
  { "-Wcomment",
    N_("Warn if nested comments are detected") },
  { "-Wno-comment", "" },
  { "-Wcomments",
    N_("Warn if nested comments are detected") },
  { "-Wno-comments", "" },
  { "-Wconversion",
    N_("Warn about possibly confusing type conversions") },
  { "-Wno-conversion", "" },
  { "-Wdiv-by-zero", "" },
  { "-Wno-div-by-zero",
    N_("Do not warn about compile-time integer division by zero") },
  { "-Wfloat-equal",
    N_("Warn about testing equality of floating point numbers") },
  { "-Wno-float-equal", "" },
  { "-Wformat",
    N_("Warn about printf/scanf/strftime/strfmon format anomalies") },
  { "-Wno-format", "" },
  { "-Wformat-extra-args", "" },
  { "-Wno-format-extra-args",
    N_("Don't warn about too many arguments to format functions") },
  { "-Wformat-nonliteral",
    N_("Warn about non-string-literal format strings") },
  { "-Wno-format-nonliteral", "" },
  { "-Wformat-security",
    N_("Warn about possible security problems with format functions") },
  { "-Wno-format-security", "" },
  { "-Wformat-y2k", "" },
  { "-Wno-format-y2k",
    N_("Don't warn about strftime formats yielding 2 digit years") },
  { "-Wimplicit-function-declaration",
    N_("Warn about implicit function declarations") },
  { "-Wno-implicit-function-declaration", "" },
  { "-Werror-implicit-function-declaration", "" },
  { "-Wimplicit-int",
    N_("Warn when a declaration does not specify a type") },
  { "-Wno-implicit-int", "" },
  { "-Wimplicit", "" },
  { "-Wno-implicit", "" },
  { "-Wimport",
    N_("Warn about the use of the #import directive") },
  { "-Wno-import", "" },
  { "-Wlong-long","" },
  { "-Wno-long-long",
    N_("Do not warn about using 'long long' when -pedantic") },
  { "-Wmain",
    N_("Warn about suspicious declarations of main") },
  { "-Wno-main", "" },
  { "-Wmissing-braces",
    N_("Warn about possibly missing braces around initializers") },
  { "-Wno-missing-braces", "" },
  { "-Wmissing-declarations",
    N_("Warn about global funcs without previous declarations") },
  { "-Wno-missing-declarations", "" },
  { "-Wmissing-prototypes",
    N_("Warn about global funcs without prototypes") },
  { "-Wno-missing-prototypes", "" },
  { "-Wmultichar",
    N_("Warn about use of multicharacter literals") },
  { "-Wno-multichar", "" },
  { "-Wnested-externs",
    N_("Warn about externs not at file scope level") },
  { "-Wno-nested-externs", "" },
  { "-Wparentheses",
    N_("Warn about possible missing parentheses") },
  { "-Wno-parentheses", "" },
  { "-Wpointer-arith",
    N_("Warn about function pointer arithmetic") },
  { "-Wno-pointer-arith", "" },
  { "-Wredundant-decls",
    N_("Warn about multiple declarations of the same object") },
  { "-Wno-redundant-decls", "" },
  { "-Wreturn-type",
    N_("Warn whenever a function's return-type defaults to int") },
  { "-Wno-return-type", "" },
  { "-Wsequence-point",
    N_("Warn about possible violations of sequence point rules") },
  { "-Wno-sequence-point", "" },
  { "-Wsign-compare",
    N_("Warn about signed/unsigned comparisons") },
  { "-Wno-sign-compare", "" },
  { "-Wstrict-prototypes",
    N_("Warn about non-prototyped function decls") },
  { "-Wno-strict-prototypes", "" },
  { "-Wtraditional",
    N_("Warn about constructs whose meanings change in ISO C") },
  { "-Wno-traditional", "" },
  { "-Wtrigraphs",
    N_("Warn when trigraphs are encountered") },
  { "-Wno-trigraphs", "" },
  { "-Wundef", "" },
  { "-Wno-undef", "" },
  { "-Wunknown-pragmas",
    N_("Warn about unrecognized pragmas") },
  { "-Wno-unknown-pragmas", "" },
  { "-Wwrite-strings",
    N_("Mark strings as 'const char *'") },
  { "-Wno-write-strings", "" },

#define DEFINE_LANG_NAME(NAME) { NULL, NAME },

#include "options.h"

};

/* Here is a table, controlled by the tm.h file, listing each -m switch
   and which bits in `target_switches' it should set or clear.
   If VALUE is positive, it is bits to set.
   If VALUE is negative, -VALUE is bits to clear.
   (The sign bit is not used so there is no confusion.)  */

static const struct
{
  const char *const name;
  const int value;
  const char *const description;
}
target_switches[] = TARGET_SWITCHES;

/* This table is similar, but allows the switch to have a value.  */

#ifdef TARGET_OPTIONS
static const struct
{
  const char *const prefix;
  const char **const variable;
  const char *const description;
}
target_options[] = TARGET_OPTIONS;
#endif

/* Options controlling warnings.  */

/* Don't print warning messages.  -w.  */

int inhibit_warnings = 0;

/* Don't suppress warnings from system headers.  -Wsystem-headers.  */

int warn_system_headers = 0;

/* Print various extra warnings.  -W.  */

int extra_warnings = 0;

/* Treat warnings as errors.  -Werror.  */

int warnings_are_errors = 0;

/* Nonzero to warn about unused variables, functions et.al.  */

int warn_unused_function;
int warn_unused_label;
int warn_unused_parameter;
int warn_unused_variable;
int warn_unused_value;

/* Nonzero to warn about code which is never reached.  */

int warn_notreached;

/* Nonzero to warn about variables used before they are initialized.  */

int warn_uninitialized;

/* Nonzero means warn about all declarations which shadow others.  */

int warn_shadow;

/* Warn if a switch on an enum, that does not have a default case,
   fails to have a case for every enum value.  */

int warn_switch;

/* Warn if a switch does not have a default case.  */

int warn_switch_default;

/* Warn if a switch on an enum fails to have a case for every enum
   value (regardless of the presence or otherwise of a default case).  */

int warn_switch_enum;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

int warn_return_type;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */

int warn_cast_align;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */

int warn_larger_than;
HOST_WIDE_INT larger_than_size;

/* Nonzero means warn if inline function is too large.  */

int warn_inline;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */

int warn_aggregate_return;

/* Warn if packed attribute on struct is unnecessary and inefficient.  */

int warn_packed;

/* Warn when gcc pads a structure to an alignment boundary.  */

int warn_padded;

/* Warn when an optimization pass is disabled.  */

int warn_disabled_optimization;

/* Warn about functions which might be candidates for attribute noreturn.  */

int warn_missing_noreturn;

/* Nonzero means warn about uses of __attribute__((deprecated))
   declarations.  */

int warn_deprecated_decl = 1;

/* Nonzero means warn about constructs which might not be
   strict-aliasing safe.  */

int warn_strict_aliasing;

/* Likewise for -W.  */

static const lang_independent_options W_options[] =
{
  {"unused-function", &warn_unused_function, 1,
   N_("Warn when a function is unused") },
  {"unused-label", &warn_unused_label, 1,
   N_("Warn when a label is unused") },
  {"unused-parameter", &warn_unused_parameter, 1,
   N_("Warn when a function parameter is unused") },
  {"unused-variable", &warn_unused_variable, 1,
   N_("Warn when a variable is unused") },
  {"unused-value", &warn_unused_value, 1,
   N_("Warn when an expression value is unused") },
  {"system-headers", &warn_system_headers, 1,
   N_("Do not suppress warnings from system headers") },
  {"error", &warnings_are_errors, 1,
   N_("Treat all warnings as errors") },
  {"shadow", &warn_shadow, 1,
   N_("Warn when one local variable shadows another") },
  {"switch", &warn_switch, 1,
   N_("Warn about enumerated switches, with no default, missing a case") },
  {"switch-default", &warn_switch_default, 1,
   N_("Warn about enumerated switches missing a default case") },
  {"switch-enum", &warn_switch_enum, 1,
   N_("Warn about all enumerated switches missing a specific case") },
  {"aggregate-return", &warn_aggregate_return, 1,
   N_("Warn about returning structures, unions or arrays") },
  {"cast-align", &warn_cast_align, 1,
   N_("Warn about pointer casts which increase alignment") },
  {"unreachable-code", &warn_notreached, 1,
   N_("Warn about code that will never be executed") },
  {"uninitialized", &warn_uninitialized, 1,
   N_("Warn about uninitialized automatic variables") },
  {"inline", &warn_inline, 1,
   N_("Warn when an inlined function cannot be inlined") },
  {"packed", &warn_packed, 1,
   N_("Warn when the packed attribute has no effect on struct layout") },
  {"padded", &warn_padded, 1,
   N_("Warn when padding is required to align struct members") },
  {"disabled-optimization", &warn_disabled_optimization, 1,
   N_("Warn when an optimization pass is disabled") },
  {"deprecated-declarations", &warn_deprecated_decl, 1,
   N_("Warn about uses of __attribute__((deprecated)) declarations") },
  {"missing-noreturn", &warn_missing_noreturn, 1,
   N_("Warn about functions which might be candidates for attribute noreturn") },
  {"strict-aliasing", &warn_strict_aliasing, 1,
   N_ ("Warn about code which might break the strict aliasing rules") }
};

void
set_Wunused (setting)
     int setting;
{
  warn_unused_function = setting;
  warn_unused_label = setting;
  /* Unused function parameter warnings are reported when either ``-W
     -Wunused'' or ``-Wunused-parameter'' is specified.  Differentiate
     -Wunused by setting WARN_UNUSED_PARAMETER to -1.  */
  if (!setting)
    warn_unused_parameter = 0;
  else if (!warn_unused_parameter)
    warn_unused_parameter = -1;
  warn_unused_variable = setting;
  warn_unused_value = setting;
}

/* The following routines are useful in setting all the flags that
   -ffast-math and -fno-fast-math imply.  */

void
set_fast_math_flags (set)
     int set;
{
  flag_trapping_math = !set;
  flag_unsafe_math_optimizations = set;
  flag_finite_math_only = set;
  flag_errno_math = !set;
  if (set)
    flag_signaling_nans = 0;
}

/* Return true iff flags are set as if -ffast-math.  */
bool
fast_math_flags_set_p ()
{
  return (!flag_trapping_math
	  && flag_unsafe_math_optimizations
	  && flag_finite_math_only
	  && !flag_errno_math);
}


/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
FILE *rtl_dump_file = NULL;

/* Decode the string P as an integral parameter.
   If the string is indeed an integer return its numeric value else
   issue an Invalid Option error for the option PNAME and return DEFVAL.
   If PNAME is zero just return DEFVAL, do not call error.  */

int
read_integral_parameter (p, pname, defval)
     const char *p;
     const char *pname;
     const int  defval;
{
  const char *endp = p;

  while (*endp)
    {
      if (ISDIGIT (*endp))
	endp++;
      else
	break;
    }

  if (*endp != 0)
    {
      if (pname != 0)
	error ("invalid option `%s'", pname);
      return defval;
    }

  return atoi (p);
}

/* This calls abort and is used to avoid problems when abort is a macro.
   It is used when we need to pass the address of abort.  */

void
do_abort ()
{
  abort ();
}

/* When `malloc.c' is compiled with `rcheck' defined,
   it calls this function to report clobberage.  */

void
botch (s)
     const char *s ATTRIBUTE_UNUSED;
{
  abort ();
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.

   This should be used via the `exact_log2' macro.  */

int
exact_log2_wide (x)
     unsigned HOST_WIDE_INT x;
{
  int log = 0;
  /* Test for 0 or a power of 2.  */
  if (x == 0 || x != (x & -x))
    return -1;
  while ((x >>= 1) != 0)
    log++;
  return log;
}

/* Given X, an unsigned number, return the largest int Y such that 2**Y <= X.
   If X is 0, return -1.

   This should be used via the floor_log2 macro.  */

int
floor_log2_wide (x)
     unsigned HOST_WIDE_INT x;
{
  int log = -1;
  while (x != 0)
    log++,
    x >>= 1;
  return log;
}

/* Handler for fatal signals, such as SIGSEGV.  These are transformed
   into ICE messages, which is much more user friendly.  */

static void
crash_signal (signo)
     int signo;
{
  internal_error ("%s", strsignal (signo));
}

/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

void
strip_off_ending (name, len)
     char *name;
     int len;
{
  int i;
  for (i = 2; i < 6 && len > i; i++)
    {
      if (name[len - i] == '.')
	{
	  name[len - i] = '\0';
	  break;
	}
    }
}

/* Output a quoted string.  */

void
output_quoted_string (asm_file, string)
     FILE *asm_file;
     const char *string;
{
#ifdef OUTPUT_QUOTED_STRING
  OUTPUT_QUOTED_STRING (asm_file, string);
#else
  char c;

  putc ('\"', asm_file);
  while ((c = *string++) != 0)
    {
      if (ISPRINT (c))
	{
	  if (c == '\"' || c == '\\')
	    putc ('\\', asm_file);
	  putc (c, asm_file);
	}
      else
	fprintf (asm_file, "\\%03o", (unsigned char) c);
    }
  putc ('\"', asm_file);
#endif
}

/* Output NAME into FILE after having turned it into something
   usable as an identifier in a target's assembly file.  */
void
output_clean_symbol_name (file, name)
     FILE *file;
     const char *name;
{
  /* Make a copy of NAME.  */
  char *id = xstrdup (name);

  /* Make it look like a valid identifier for an assembler.  */
  clean_symbol_name (id);

  fputs (id, file);
  free (id);
}


/* Output a file name in the form wanted by System V.  */

void
output_file_directive (asm_file, input_name)
     FILE *asm_file;
     const char *input_name;
{
  int len = strlen (input_name);
  const char *na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (IS_DIR_SEPARATOR (na[-1]))
	break;
      na--;
    }

#ifdef ASM_OUTPUT_MAIN_SOURCE_FILENAME
  ASM_OUTPUT_MAIN_SOURCE_FILENAME (asm_file, na);
#else
#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
#else
  fprintf (asm_file, "\t.file\t");
  output_quoted_string (asm_file, na);
  fputc ('\n', asm_file);
#endif
#endif
}

/* Routine to open a dump file.  Return true if the dump file is enabled.  */

static int
open_dump_file (index, decl)
     enum dump_file_index index;
     tree decl;
{
  char *dump_name;
  const char *open_arg;
  char seq[16];

  if (! dump_file[index].enabled)
    return 0;

  timevar_push (TV_DUMP);
  if (rtl_dump_file != NULL)
    fclose (rtl_dump_file);

  sprintf (seq, DUMPFILE_FORMAT, index);

  if (! dump_file[index].initialized)
    {
      /* If we've not initialized the files, do so now.  */
      if (graph_dump_format != no_graph
	  && dump_file[index].graph_dump_p)
	{
	  dump_name = concat (seq, dump_file[index].extension, NULL);
	  clean_graph_dump_file (dump_base_name, dump_name);
	  free (dump_name);
	}
      dump_file[index].initialized = 1;
      open_arg = "w";
    }
  else
    open_arg = "a";

  dump_name = concat (dump_base_name, seq,
		      dump_file[index].extension, NULL);

  rtl_dump_file = fopen (dump_name, open_arg);
  if (rtl_dump_file == NULL)
    fatal_io_error ("can't open %s", dump_name);

  free (dump_name);

  if (decl)
    fprintf (rtl_dump_file, "\n;; Function %s%s\n\n",
	     (*lang_hooks.decl_printable_name) (decl, 2),
	     cfun->function_frequency == FUNCTION_FREQUENCY_HOT
	     ? " (hot)"
	     : cfun->function_frequency == FUNCTION_FREQUENCY_UNLIKELY_EXECUTED
	     ? " (unlikely executed)"
	     : "");

  timevar_pop (TV_DUMP);
  return 1;
}

/* Routine to close a dump file.  */

static void
close_dump_file (index, func, insns)
     enum dump_file_index index;
     void (*func) PARAMS ((FILE *, rtx));
     rtx insns;
{
  if (! rtl_dump_file)
    return;

  timevar_push (TV_DUMP);
  if (insns
      && graph_dump_format != no_graph
      && dump_file[index].graph_dump_p)
    {
      char seq[16];
      char *suffix;

      sprintf (seq, DUMPFILE_FORMAT, index);
      suffix = concat (seq, dump_file[index].extension, NULL);
      print_rtl_graph_with_bb (dump_base_name, suffix, insns);
      free (suffix);
    }

  if (func && insns)
    func (rtl_dump_file, insns);

  fflush (rtl_dump_file);
  fclose (rtl_dump_file);

  rtl_dump_file = NULL;
  timevar_pop (TV_DUMP);
}

/* Do any final processing required for the declarations in VEC, of
   which there are LEN.  We write out inline functions and variables
   that have been deferred until this point, but which are required.
   Returns nonzero if anything was put out.  */

int
wrapup_global_declarations (vec, len)
     tree *vec;
     int len;
{
  tree decl;
  int i;
  int reconsider;
  int output_something = 0;

  for (i = 0; i < len; i++)
    {
      decl = vec[i];

      /* We're not deferring this any longer.  Assignment is
	 conditional to avoid needlessly dirtying PCH pages. */
      if (DECL_DEFER_OUTPUT (decl) != 0)
	DECL_DEFER_OUTPUT (decl) = 0;

      if (TREE_CODE (decl) == VAR_DECL && DECL_SIZE (decl) == 0)
	(*lang_hooks.finish_incomplete_decl) (decl);
    }

  /* Now emit any global variables or functions that we have been
     putting off.  We need to loop in case one of the things emitted
     here references another one which comes earlier in the list.  */
  do
    {
      reconsider = 0;
      for (i = 0; i < len; i++)
	{
	  decl = vec[i];

	  if (TREE_ASM_WRITTEN (decl) || DECL_EXTERNAL (decl))
	    continue;

	  /* Don't write out static consts, unless we still need them.

	     We also keep static consts if not optimizing (for debugging),
	     unless the user specified -fno-keep-static-consts.
	     ??? They might be better written into the debug information.
	     This is possible when using DWARF.

	     A language processor that wants static constants to be always
	     written out (even if it is not used) is responsible for
	     calling rest_of_decl_compilation itself.  E.g. the C front-end
	     calls rest_of_decl_compilation from finish_decl.
	     One motivation for this is that is conventional in some
	     environments to write things like:
	     static const char rcsid[] = "... version string ...";
	     intending to force the string to be in the executable.

	     A language processor that would prefer to have unneeded
	     static constants "optimized away" would just defer writing
	     them out until here.  E.g. C++ does this, because static
	     constants are often defined in header files.

	     ??? A tempting alternative (for both C and C++) would be
	     to force a constant to be written if and only if it is
	     defined in a main file, as opposed to an include file.  */

	  if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl))
	    {
	      bool needed = 1;

	      if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
		/* needed */;
	      else if (DECL_COMDAT (decl))
		needed = 0;
	      else if (TREE_READONLY (decl) && !TREE_PUBLIC (decl)
		       && (optimize || !flag_keep_static_consts
			   || DECL_ARTIFICIAL (decl)))
		needed = 0;

	      if (needed)
		{
		  reconsider = 1;
		  rest_of_decl_compilation (decl, NULL, 1, 1);
		}
	    }

	  if (TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_INITIAL (decl) != 0
	      && DECL_SAVED_INSNS (decl) != 0
	      && (flag_keep_inline_functions
		  || (TREE_PUBLIC (decl) && !DECL_COMDAT (decl))
		  || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	    {
	      reconsider = 1;
	      output_inline_function (decl);
	    }
	}

      if (reconsider)
	output_something = 1;
    }
  while (reconsider);

  return output_something;
}

/* Issue appropriate warnings for the global declarations in VEC (of
   which there are LEN).  Output debugging information for them.  */

void
check_global_declarations (vec, len)
     tree *vec;
     int len;
{
  tree decl;
  int i;

  for (i = 0; i < len; i++)
    {
      decl = vec[i];

      if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl)
	  && ! TREE_ASM_WRITTEN (decl))
	/* Cancel the RTL for this decl so that, if debugging info
	   output for global variables is still to come,
	   this one will be omitted.  */
	SET_DECL_RTL (decl, NULL_RTX);

      /* Warn about any function
	 declared static but not defined.
	 We don't warn about variables,
	 because many programs have static variables
	 that exist only to get some text into the object file.  */
      if (TREE_CODE (decl) == FUNCTION_DECL
	  && (warn_unused_function
	      || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	  && DECL_INITIAL (decl) == 0
	  && DECL_EXTERNAL (decl)
	  && ! DECL_ARTIFICIAL (decl)
	  && ! TREE_PUBLIC (decl))
	{
	  if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	    pedwarn_with_decl (decl,
			       "`%s' used but never defined");
	  else
	    warning_with_decl (decl,
			       "`%s' declared `static' but never defined");
	  /* This symbol is effectively an "extern" declaration now.  */
	  TREE_PUBLIC (decl) = 1;
	  assemble_external (decl);
	}

      /* Warn about static fns or vars defined but not used.  */
      if (((warn_unused_function && TREE_CODE (decl) == FUNCTION_DECL)
	   /* We don't warn about "static const" variables because the
	      "rcs_id" idiom uses that construction.  */
	   || (warn_unused_variable
	       && TREE_CODE (decl) == VAR_DECL && ! TREE_READONLY (decl)))
	  && ! DECL_IN_SYSTEM_HEADER (decl)
	  && ! TREE_USED (decl)
	  /* The TREE_USED bit for file-scope decls is kept in the identifier,
	     to handle multiple external decls in different scopes.  */
	  && ! TREE_USED (DECL_NAME (decl))
	  && ! DECL_EXTERNAL (decl)
	  && ! TREE_PUBLIC (decl)
	  /* Global register variables must be declared to reserve them.  */
	  && ! (TREE_CODE (decl) == VAR_DECL && DECL_REGISTER (decl))
	  /* Otherwise, ask the language.  */
	  && (*lang_hooks.decls.warn_unused_global) (decl))
	warning_with_decl (decl, "`%s' defined but not used");

      /* Avoid confusing the debug information machinery when there are
	 errors.  */
      if (errorcount == 0 && sorrycount == 0)
	{
	  timevar_push (TV_SYMOUT);
	  (*debug_hooks->global_decl) (decl);
	  timevar_pop (TV_SYMOUT);
	}
    }
}

/* Save the current INPUT_FILENAME and LINENO on the top entry in the
   INPUT_FILE_STACK.  Push a new entry for FILE and LINE, and set the
   INPUT_FILENAME and LINENO accordingly.  */

void
push_srcloc (file, line)
     const char *file;
     int line;
{
  struct file_stack *fs;

  if (input_file_stack)
    {
      input_file_stack->name = input_filename;
      input_file_stack->line = lineno;
    }

  fs = (struct file_stack *) xmalloc (sizeof (struct file_stack));
  fs->name = input_filename = file;
  fs->line = lineno = line;
  fs->next = input_file_stack;
  input_file_stack = fs;
  input_file_stack_tick++;
}

/* Pop the top entry off the stack of presently open source files.
   Restore the INPUT_FILENAME and LINENO from the new topmost entry on
   the stack.  */

void
pop_srcloc ()
{
  struct file_stack *fs;

  fs = input_file_stack;
  input_file_stack = fs->next;
  free (fs);
  input_file_stack_tick++;
  /* The initial source file is never popped.  */
  if (!input_file_stack)
    abort ();
  input_filename = input_file_stack->name;
  lineno = input_file_stack->line;
}

/* Compile an entire translation unit.  Write a file of assembly
   output and various debugging dumps.  */

static void
compile_file ()
{
  /* Initialize yet another pass.  */

  init_final (main_input_filename);
  init_branch_prob (aux_base_name);

  timevar_push (TV_PARSE);

  /* Call the parser, which parses the entire file (calling
     rest_of_compilation for each function).  */
  (*lang_hooks.parse_file) (set_yydebug);

  /* In case there were missing block closers,
     get us back to the global binding level.  */
  (*lang_hooks.clear_binding_stack) ();

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */
  timevar_pop (TV_PARSE);

  if (flag_syntax_only)
    return;

  (*lang_hooks.decls.final_write_globals)();

    /* This must occur after the loop to output deferred functions.  Else
       the profiler initializer would not be emitted if all the functions
       in this compilation unit were deferred.

       output_func_start_profiler can not cause any additional functions or
       data to need to be output, so it need not be in the deferred function
       loop above.  */
    output_func_start_profiler ();

  /* Write out any pending weak symbol declarations.  */

  weak_finish ();

  /* Do dbx symbols.  */
  timevar_push (TV_SYMOUT);

#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_finish ();
#endif

  (*debug_hooks->finish) (main_input_filename);
  timevar_pop (TV_SYMOUT);

  /* Output some stuff at end of file if nec.  */

  dw2_output_indirect_constants ();

  end_final (aux_base_name);

  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      timevar_push (TV_DUMP);
      open_dump_file (DFI_bp, NULL);

      end_branch_prob ();

      close_dump_file (DFI_bp, NULL, NULL_RTX);
      timevar_pop (TV_DUMP);
    }

#ifdef ASM_FILE_END
  ASM_FILE_END (asm_out_file);
#endif

  /* Attach a special .ident directive to the end of the file to identify
     the version of GCC which compiled this code.  The format of the .ident
     string is patterned after the ones produced by native SVR4 compilers.  */
#ifdef IDENT_ASM_OP
  if (!flag_no_ident)
    fprintf (asm_out_file, "%s\"GCC: (GNU) %s\"\n",
	     IDENT_ASM_OP, version_string);
#endif

  if (optimize > 0 && open_dump_file (DFI_combine, NULL))
    {
      timevar_push (TV_DUMP);
      dump_combine_total_stats (rtl_dump_file);
      close_dump_file (DFI_combine, NULL, NULL_RTX);
      timevar_pop (TV_DUMP);
    }
}

/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables, unless the
   variable is a register variable with an ASMSPEC.  In that case, or
   if the variable is not an automatic, it sets up the RTL and
   outputs any assembler code (label definition, storage allocation
   and initialization).

   DECL is the declaration.  If ASMSPEC is nonzero, it specifies
   the assembler symbol name to be used.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (decl, asmspec, top_level, at_end)
     tree decl;
     const char *asmspec;
     int top_level;
     int at_end;
{
  /* Declarations of variables, and of functions defined elsewhere.  */

/* The most obvious approach, to put an #ifndef around where
   this macro is used, doesn't work since it's inside a macro call.  */
#ifndef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP, END)
#endif

  /* We deferred calling assemble_alias so that we could collect
     other attributes such as visibility.  Emit the alias now.  */
  {
    tree alias;
    alias = lookup_attribute ("alias", DECL_ATTRIBUTES (decl));
    if (alias)
      {
	alias = TREE_VALUE (TREE_VALUE (alias));
	alias = get_identifier (TREE_STRING_POINTER (alias));
	assemble_alias (decl, alias);
      }
  }

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    {
      timevar_push (TV_VARCONST);

      if (asmspec)
	make_decl_rtl (decl, asmspec);

      /* Don't output anything when a tentative file-scope definition
	 is seen.  But at end of compilation, do output code for them.  */
      if (at_end || !DECL_DEFER_OUTPUT (decl))
	assemble_variable (decl, top_level, at_end, 0);
      if (decl == last_assemble_variable_decl)
	{
	  ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
				     top_level, at_end);
	}

      timevar_pop (TV_VARCONST);
    }
  else if (DECL_REGISTER (decl) && asmspec != 0)
    {
      if (decode_reg_name (asmspec) >= 0)
	{
	  SET_DECL_RTL (decl, NULL_RTX);
	  make_decl_rtl (decl, asmspec);
	}
      else
	{
	  error ("invalid register name `%s' for register variable", asmspec);
	  DECL_REGISTER (decl) = 0;
	  if (!top_level)
	    expand_decl (decl);
	}
    }
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  else if ((write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	   && TREE_CODE (decl) == TYPE_DECL)
    {
      timevar_push (TV_SYMOUT);
      dbxout_symbol (decl, 0);
      timevar_pop (TV_SYMOUT);
    }
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (write_symbols == SDB_DEBUG && top_level
	   && TREE_CODE (decl) == TYPE_DECL)
    {
      timevar_push (TV_SYMOUT);
      sdbout_symbol (decl, 0);
      timevar_pop (TV_SYMOUT);
    }
#endif
#ifdef DWARF2_DEBUGGING_INFO
  else if ((write_symbols == DWARF2_DEBUG
	   || write_symbols == VMS_AND_DWARF2_DEBUG)
	   && top_level
	   && TREE_CODE (decl) == TYPE_DECL)
    {
      timevar_push (TV_SYMOUT);
      dwarf2out_decl (decl);
      timevar_pop (TV_SYMOUT);
    }
#endif
}

/* Called after finishing a record, union or enumeral type.  */

void
rest_of_type_compilation (type, toplev)
#if defined(DBX_DEBUGGING_INFO) || defined(XCOFF_DEBUGGING_INFO) || defined (SDB_DEBUGGING_INFO)
     tree type;
     int toplev;
#else
     tree type ATTRIBUTE_UNUSED;
     int toplev ATTRIBUTE_UNUSED;
#endif
{
  /* Avoid confusing the debug information machinery when there are
     errors.  */
  if (errorcount != 0 || sorrycount != 0)
    return;

  timevar_push (TV_SYMOUT);
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
    dbxout_symbol (TYPE_STUB_DECL (type), !toplev);
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_symbol (TYPE_STUB_DECL (type), !toplev);
#endif
#ifdef DWARF2_DEBUGGING_INFO
  if ((write_symbols == DWARF2_DEBUG
       || write_symbols == VMS_AND_DWARF2_DEBUG)
      && toplev)
    dwarf2out_decl (TYPE_STUB_DECL (type));
#endif
  timevar_pop (TV_SYMOUT);
}

/* This is called from finish_function (within langhooks.parse_file)
   after each top-level definition is parsed.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (decl)
     tree decl;
{
  rtx insns;
  int tem;
  int failure = 0;
  int rebuild_label_notes_after_reload;
  int register_life_up_to_date;

  timevar_push (TV_REST_OF_COMPILATION);

  /* Now that we're out of the frontend, we shouldn't have any more
     CONCATs anywhere.  */
  generating_concat_p = 0;

  /* When processing delayed functions, prepare_function_start() won't
     have been run to re-initialize it.  */
  cse_not_expected = ! optimize;

  /* First, make sure that NOTE_BLOCK is set correctly for each
     NOTE_INSN_BLOCK_BEG/NOTE_INSN_BLOCK_END note.  */
  if (!cfun->x_whole_function_mode_p)
    identify_blocks ();

  /* In function-at-a-time mode, we do not attempt to keep the BLOCK
     tree in sensible shape.  So, we just recalculate it here.  */
  if (cfun->x_whole_function_mode_p)
    reorder_blocks ();

  init_flow ();

  /* If we are reconsidering an inline function
     at the end of compilation, skip the stuff for making it inline.  */

  if (DECL_SAVED_INSNS (decl) == 0)
    {
      int inlinable = 0;
      tree parent;
      const char *lose;

      /* If this is nested inside an inlined external function, pretend
	 it was only declared.  Since we cannot inline such functions,
	 generating code for this one is not only not necessary but will
	 confuse some debugging output writers.  */
      for (parent = DECL_CONTEXT (current_function_decl);
	   parent != NULL_TREE;
	   parent = get_containing_scope (parent))
	if (TREE_CODE (parent) == FUNCTION_DECL
	    && DECL_INLINE (parent) && DECL_EXTERNAL (parent))
	  {
	    DECL_INITIAL (decl) = 0;
	    goto exit_rest_of_compilation;
	  }
	else if (TYPE_P (parent))
	  /* A function in a local class should be treated normally.  */
	  break;

      /* If requested, consider whether to make this function inline.  */
      if ((DECL_INLINE (decl) && !flag_no_inline)
	  || flag_inline_functions)
	{
	  timevar_push (TV_INTEGRATION);
	  lose = function_cannot_inline_p (decl);
	  timevar_pop (TV_INTEGRATION);
	  if (lose || ! optimize)
	    {
	      if (warn_inline && DECL_INLINE (decl))
		warning_with_decl (decl, lose);
	      DECL_ABSTRACT_ORIGIN (decl) = 0;
	      /* Don't really compile an extern inline function.
		 If we can't make it inline, pretend
		 it was only declared.  */
	      if (DECL_EXTERNAL (decl))
		{
		  DECL_INITIAL (decl) = 0;
		  goto exit_rest_of_compilation;
		}
	    }
          else 
            {
	      /* ??? Note that we used to just make it look like if
		 the "inline" keyword was specified when we decide
		 to inline it (because of -finline-functions).
		 garloff at suse dot de, 2002-04-24: Add another flag to
		 actually record this piece of information.  */
	      if (!DECL_INLINE (decl))
		DID_INLINE_FUNC (decl) = 1;
	      inlinable = DECL_INLINE (decl) = 1;
	    }
	}

      insns = get_insns ();

      /* Dump the rtl code if we are dumping rtl.  */

      if (open_dump_file (DFI_rtl, decl))
	{
	  if (DECL_SAVED_INSNS (decl))
	    fprintf (rtl_dump_file, ";; (integrable)\n\n");
	  close_dump_file (DFI_rtl, print_rtl, insns);
	}

      /* Convert from NOTE_INSN_EH_REGION style notes, and do other
	 sorts of eh initialization.  Delay this until after the
         initial rtl dump so that we can see the original nesting.  */
      convert_from_eh_region_ranges ();

      /* If function is inline, and we don't yet know whether to
         compile it by itself, defer decision till end of compilation.
         wrapup_global_declarations will (indirectly) call
         rest_of_compilation again for those functions that need to
         be output.  Also defer those functions that we are supposed
         to defer.  */

      if (inlinable
	  || (DECL_INLINE (decl)
	      && ((! TREE_PUBLIC (decl) && ! TREE_ADDRESSABLE (decl)
		   && ! TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))
		   && ! flag_keep_inline_functions)
		  || DECL_EXTERNAL (decl))))
	DECL_DEFER_OUTPUT (decl) = 1;

      if (DECL_INLINE (decl))
	/* DWARF wants separate debugging info for abstract and
	   concrete instances of all inline functions, including those
	   declared inline but not inlined, and those inlined even
	   though they weren't declared inline.  Conveniently, that's
	   what DECL_INLINE means at this point.  */
	(*debug_hooks->deferred_inline_function) (decl);

      if (DECL_DEFER_OUTPUT (decl))
	{
	  /* If -Wreturn-type, we have to do a bit of compilation.  We just
	     want to call cleanup the cfg to figure out whether or not we can
	     fall off the end of the function; we do the minimum amount of
	     work necessary to make that safe.  */
	  if (warn_return_type)
	    {
	      int saved_optimize = optimize;

	      optimize = 0;
	      rebuild_jump_labels (insns);
	      find_exception_handler_labels ();
	      find_basic_blocks (insns, max_reg_num (), rtl_dump_file);
	      cleanup_cfg (CLEANUP_PRE_SIBCALL | CLEANUP_PRE_LOOP);
	      optimize = saved_optimize;

	      /* CFG is no longer maintained up-to-date.  */
	      free_bb_for_insn ();
	    }

	  set_nothrow_function_flags ();
	  if (current_function_nothrow)
	    /* Now we know that this can't throw; set the flag for the benefit
	       of other functions later in this translation unit.  */
	    TREE_NOTHROW (current_function_decl) = 1;

	  timevar_push (TV_INTEGRATION);
	  save_for_inline (decl);
	  timevar_pop (TV_INTEGRATION);
	  DECL_SAVED_INSNS (decl)->inlinable = inlinable;
	  goto exit_rest_of_compilation;
	}

      /* If specified extern inline but we aren't inlining it, we are
	 done.  This goes for anything that gets here with DECL_EXTERNAL
	 set, not just things with DECL_INLINE.  */
      if (DECL_EXTERNAL (decl))
	goto exit_rest_of_compilation;
    }

  /* If we're emitting a nested function, make sure its parent gets
     emitted as well.  Doing otherwise confuses debug info.  */
  {
    tree parent;
    for (parent = DECL_CONTEXT (current_function_decl);
	 parent != NULL_TREE;
	 parent = get_containing_scope (parent))
      if (TREE_CODE (parent) == FUNCTION_DECL)
	TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (parent)) = 1;
  }

  /* We are now committed to emitting code for this function.  Do any
     preparation, such as emitting abstract debug info for the inline
     before it gets mangled by optimization.  */
  if (DECL_INLINE (decl))
    (*debug_hooks->outlining_inline_function) (decl);

  /* Remove any notes we don't need.  That will make iterating
     over the instruction sequence faster, and allow the garbage
     collector to reclaim the memory used by the notes.  */
  remove_unnecessary_notes ();
  reorder_blocks ();

  ggc_collect ();

  /* Initialize some variables used by the optimizers.  */
  init_function_for_compilation ();

  if (! DECL_DEFER_OUTPUT (decl))
    TREE_ASM_WRITTEN (decl) = 1;

  /* Now that integrate will no longer see our rtl, we need not
     distinguish between the return value of this function and the
     return value of called functions.  Also, we can remove all SETs
     of subregs of hard registers; they are only here because of
     integrate.  Also, we can now initialize pseudos intended to
     carry magic hard reg data throughout the function.  */
  rtx_equal_function_value_matters = 0;
  purge_hard_subreg_sets (get_insns ());

  /* Early return if there were errors.  We can run afoul of our
     consistency checks, and there's not really much point in fixing them.
     Don't return yet if -Wreturn-type; we need to do cleanup_cfg.  */
  if (((rtl_dump_and_exit || flag_syntax_only) && !warn_return_type)
      || errorcount || sorrycount)
    goto exit_rest_of_compilation;

  timevar_push (TV_JUMP);
  open_dump_file (DFI_sibling, decl);
  insns = get_insns ();
  rebuild_jump_labels (insns);
  find_exception_handler_labels ();
  find_basic_blocks (insns, max_reg_num (), rtl_dump_file);

  delete_unreachable_blocks ();

  /* We have to issue these warnings now already, because CFG cleanups
     further down may destroy the required information.  */
  check_function_return_warnings ();

  /* Turn NOTE_INSN_PREDICTIONs into branch predictions.  */
  if (flag_guess_branch_prob)
    {
      timevar_push (TV_BRANCH_PROB);
      note_prediction_to_br_prob ();
      timevar_pop (TV_BRANCH_PROB);
    }

  /* We may have potential sibling or tail recursion sites.  Select one
     (of possibly multiple) methods of performing the call.  */
  if (flag_optimize_sibling_calls)
    {
      rtx insn;
      optimize_sibling_and_tail_recursive_calls ();

      /* Recompute the CFG as sibling optimization clobbers it randomly.  */
      free_bb_for_insn ();
      find_exception_handler_labels ();
      rebuild_jump_labels (insns);
      find_basic_blocks (insns, max_reg_num (), rtl_dump_file);

      /* There is pass ordering problem - we must lower NOTE_INSN_PREDICTION
         notes before simplifying cfg and we must do lowering after sibcall
         that unhides parts of RTL chain and cleans up the CFG.

         Until sibcall is replaced by tree-level optimizer, lets just
         sweep away the NOTE_INSN_PREDICTION notes that leaked out.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == NOTE
	    && NOTE_LINE_NUMBER (insn) == NOTE_INSN_PREDICTION)
	  delete_insn (insn);
    }
  close_dump_file (DFI_sibling, print_rtl, get_insns ());
  timevar_pop (TV_JUMP);

  scope_to_insns_initialize ();
  /* Complete generation of exception handling code.  */
  if (doing_eh (0))
    {
      timevar_push (TV_JUMP);
      open_dump_file (DFI_eh, decl);

      finish_eh_generation ();

      close_dump_file (DFI_eh, print_rtl, get_insns ());
      timevar_pop (TV_JUMP);
    }

  /* Delay emitting hard_reg_initial_value sets until after EH landing pad
     generation, which might create new sets.  */
  emit_initial_value_sets ();

#ifdef FINALIZE_PIC
  /* If we are doing position-independent code generation, now
     is the time to output special prologues and epilogues.
     We do not want to do this earlier, because it just clutters
     up inline functions with meaningless insns.  */
  if (flag_pic)
    FINALIZE_PIC;
#endif

  insns = get_insns ();

  /* Copy any shared structure that should not be shared.  */
  unshare_all_rtl (current_function_decl, insns);

#ifdef SETJMP_VIA_SAVE_AREA
  /* This must be performed before virtual register instantiation.
     Please be aware the everything in the compiler that can look
     at the RTL up to this point must understand that REG_SAVE_AREA
     is just like a use of the REG contained inside.  */
  if (current_function_calls_alloca)
    optimize_save_area_alloca (insns);
#endif

  /* Instantiate all virtual registers.  */
  instantiate_virtual_regs (current_function_decl, insns);

  open_dump_file (DFI_jump, decl);

  /* Always do one jump optimization pass to ensure that JUMP_LABEL fields
     are initialized and to compute whether control can drop off the end
     of the function.  */

  timevar_push (TV_JUMP);
  /* Turn NOTE_INSN_EXPECTED_VALUE into REG_BR_PROB.  Do this
     before jump optimization switches branch directions.  */
  if (flag_guess_branch_prob)
    expected_value_to_br_prob ();

  reg_scan (insns, max_reg_num (), 0);
  rebuild_jump_labels (insns);
  find_basic_blocks (insns, max_reg_num (), rtl_dump_file);
  delete_trivially_dead_insns (insns, max_reg_num ());
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_PRE_LOOP
	       | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  /* CFG is no longer maintained up-to-date.  */
  if (optimize)
    {
      free_bb_for_insn ();
      copy_loop_headers (insns);
      find_basic_blocks (insns, max_reg_num (), rtl_dump_file);
    }
  purge_line_number_notes (insns);

  timevar_pop (TV_JUMP);
  close_dump_file (DFI_jump, print_rtl, insns);

  /* Now is when we stop if -fsyntax-only and -Wreturn-type.  */
  if (rtl_dump_and_exit || flag_syntax_only || DECL_DEFER_OUTPUT (decl))
    {
      goto exit_rest_of_compilation;
    }

  /* Long term, this should probably move before the jump optimizer too,
     but I didn't want to disturb the rtl_dump_and_exit and related
     stuff at this time.  */
  if (optimize > 0 && flag_ssa)
    {
      /* Convert to SSA form.  */

      timevar_push (TV_TO_SSA);
      open_dump_file (DFI_ssa, decl);

      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
      convert_to_ssa ();

      close_dump_file (DFI_ssa, print_rtl_with_bb, insns);
      timevar_pop (TV_TO_SSA);

      /* Perform sparse conditional constant propagation, if requested.  */
      if (flag_ssa_ccp)
	{
	  timevar_push (TV_SSA_CCP);
	  open_dump_file (DFI_ssa_ccp, decl);

	  ssa_const_prop ();

	  close_dump_file (DFI_ssa_ccp, print_rtl_with_bb, get_insns ());
	  timevar_pop (TV_SSA_CCP);
	}

      /* It would be useful to cleanup the CFG at this point, but block
	 merging and possibly other transformations might leave a PHI
	 node in the middle of a basic block, which is a strict no-no.  */

      /* The SSA implementation uses basic block numbers in its phi
	 nodes.  Thus, changing the control-flow graph or the basic
	 blocks, e.g., calling find_basic_blocks () or cleanup_cfg (),
	 may cause problems.  */

      if (flag_ssa_dce)
	{
	  /* Remove dead code.  */

	  timevar_push (TV_SSA_DCE);
	  open_dump_file (DFI_ssa_dce, decl);

	  insns = get_insns ();
	  ssa_eliminate_dead_code ();

	  close_dump_file (DFI_ssa_dce, print_rtl_with_bb, insns);
	  timevar_pop (TV_SSA_DCE);
	}

      /* Convert from SSA form.  */

      timevar_push (TV_FROM_SSA);
      open_dump_file (DFI_ussa, decl);

      convert_from_ssa ();
      /* New registers have been created.  Rescan their usage.  */
      reg_scan (insns, max_reg_num (), 1);

      close_dump_file (DFI_ussa, print_rtl_with_bb, insns);
      timevar_pop (TV_FROM_SSA);

      ggc_collect ();
    }

  timevar_push (TV_JUMP);
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  /* Try to identify useless null pointer tests and delete them.  */
  if (flag_delete_null_pointer_checks)
    {
      open_dump_file (DFI_null, decl);
      if (rtl_dump_file)
	dump_flow_info (rtl_dump_file);

      if (delete_null_pointer_checks (insns))
        cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

      close_dump_file (DFI_null, print_rtl_with_bb, insns);
    }

  /* Jump optimization, and the removal of NULL pointer checks, may
     have reduced the number of instructions substantially.  CSE, and
     future passes, allocate arrays whose dimensions involve the
     maximum instruction UID, so if we can reduce the maximum UID
     we'll save big on memory.  */
  renumber_insns (rtl_dump_file);
  timevar_pop (TV_JUMP);

  close_dump_file (DFI_jump, print_rtl_with_bb, insns);

  ggc_collect ();

  /* Perform common subexpression elimination.
     Nonzero value from `cse_main' means that jumps were simplified
     and some code may now be unreachable, so do
     jump optimization again.  */

  if (optimize > 0)
    {
      open_dump_file (DFI_cse, decl);
      if (rtl_dump_file)
	dump_flow_info (rtl_dump_file);
      timevar_push (TV_CSE);

      reg_scan (insns, max_reg_num (), 1);

      tem = cse_main (insns, max_reg_num (), 0, rtl_dump_file);
      if (tem)
	rebuild_jump_labels (insns);
      purge_all_dead_edges (0);

      delete_trivially_dead_insns (insns, max_reg_num ());

      /* If we are not running more CSE passes, then we are no longer
	 expecting CSE to be run.  But always rerun it in a cheap mode.  */
      cse_not_expected = !flag_rerun_cse_after_loop && !flag_gcse;

      if (tem || optimize > 1)
	cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
      /* Try to identify useless null pointer tests and delete them.  */
      if (flag_delete_null_pointer_checks)
	{
	  timevar_push (TV_JUMP);

	  if (delete_null_pointer_checks (insns))
	    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
	  timevar_pop (TV_JUMP);
	}

      /* The second pass of jump optimization is likely to have
         removed a bunch more instructions.  */
      renumber_insns (rtl_dump_file);

      timevar_pop (TV_CSE);
      close_dump_file (DFI_cse, print_rtl_with_bb, insns);
    }

  open_dump_file (DFI_addressof, decl);

  purge_addressof (insns);
  if (optimize && purge_all_dead_edges (0))
    delete_unreachable_blocks ();
  reg_scan (insns, max_reg_num (), 1);

  close_dump_file (DFI_addressof, print_rtl, insns);

  ggc_collect ();

  /* Perform global cse.  */

  if (optimize > 0 && flag_gcse)
    {
      int save_csb, save_cfj;
      int tem2 = 0;

      timevar_push (TV_GCSE);
      open_dump_file (DFI_gcse, decl);

      tem = gcse_main (insns, rtl_dump_file);
      rebuild_jump_labels (insns);
      delete_trivially_dead_insns (insns, max_reg_num ());

      save_csb = flag_cse_skip_blocks;
      save_cfj = flag_cse_follow_jumps;
      flag_cse_skip_blocks = flag_cse_follow_jumps = 0;

      /* If -fexpensive-optimizations, re-run CSE to clean up things done
	 by gcse.  */
      if (flag_expensive_optimizations)
	{
	  timevar_push (TV_CSE);
	  reg_scan (insns, max_reg_num (), 1);
	  tem2 = cse_main (insns, max_reg_num (), 0, rtl_dump_file);
	  purge_all_dead_edges (0);
	  delete_trivially_dead_insns (insns, max_reg_num ());
	  timevar_pop (TV_CSE);
	  cse_not_expected = !flag_rerun_cse_after_loop;
	}

      /* If gcse or cse altered any jumps, rerun jump optimizations to clean
	 things up.  Then possibly re-run CSE again.  */
      while (tem || tem2)
	{
	  tem = tem2 = 0;
	  timevar_push (TV_JUMP);
	  rebuild_jump_labels (insns);
	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
	  timevar_pop (TV_JUMP);

	  if (flag_expensive_optimizations)
	    {
	      timevar_push (TV_CSE);
	      reg_scan (insns, max_reg_num (), 1);
	      tem2 = cse_main (insns, max_reg_num (), 0, rtl_dump_file);
	      purge_all_dead_edges (0);
	      delete_trivially_dead_insns (insns, max_reg_num ());
	      timevar_pop (TV_CSE);
	    }
	}

      close_dump_file (DFI_gcse, print_rtl_with_bb, insns);
      timevar_pop (TV_GCSE);

      ggc_collect ();
      flag_cse_skip_blocks = save_csb;
      flag_cse_follow_jumps = save_cfj;
#ifdef ENABLE_CHECKING
      verify_flow_info ();
#endif
    }

  /* Move constant computations out of loops.  */

  if (optimize > 0 && flag_loop_optimize)
    {
      int do_unroll, do_prefetch;

      timevar_push (TV_LOOP);
      delete_dead_jumptables ();
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
      open_dump_file (DFI_loop, decl);
      /* CFG is no longer maintained up-to-date.  */
      free_bb_for_insn ();

      do_unroll = flag_unroll_loops ? LOOP_UNROLL : LOOP_AUTO_UNROLL;
      do_prefetch = flag_prefetch_loop_arrays ? LOOP_PREFETCH : 0;
      if (flag_rerun_loop_opt)
	{
	  cleanup_barriers ();

	  /* We only want to perform unrolling once.  */
	  loop_optimize (insns, rtl_dump_file, do_unroll);
	  do_unroll = 0;

	  /* The first call to loop_optimize makes some instructions
	     trivially dead.  We delete those instructions now in the
	     hope that doing so will make the heuristics in loop work
	     better and possibly speed up compilation.  */
	  delete_trivially_dead_insns (insns, max_reg_num ());

	  /* The regscan pass is currently necessary as the alias
		  analysis code depends on this information.  */
	  reg_scan (insns, max_reg_num (), 1);
	}
      cleanup_barriers ();
      loop_optimize (insns, rtl_dump_file, do_unroll | LOOP_BCT | do_prefetch);

      /* Loop can create trivially dead instructions.  */
      delete_trivially_dead_insns (insns, max_reg_num ());
      close_dump_file (DFI_loop, print_rtl, insns);
      timevar_pop (TV_LOOP);
      find_basic_blocks (insns, max_reg_num (), rtl_dump_file);

      ggc_collect ();
    }

  /* Do control and data flow analysis; wrote some of the results to
     the dump file.  */

  timevar_push (TV_FLOW);
  open_dump_file (DFI_cfg, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  /* It may make more sense to mark constant functions after dead code is
     eliminated by life_analyzis, but we need to do it early, as -fprofile-arcs
     may insert code making function non-constant, but we still must consider
     it as constant, otherwise -fbranch-probabilities will not read data back.

     life_analyzis rarely eliminates modification of external memory.
   */
  if (optimize)
    mark_constant_function ();

  close_dump_file (DFI_cfg, print_rtl_with_bb, insns);

  /* Do branch profiling and static profile estimation passes.  */
  if (optimize > 0 || cfun->arc_profile || flag_branch_probabilities)
    {
      struct loops loops;

      timevar_push (TV_BRANCH_PROB);
      open_dump_file (DFI_bp, decl);
      if (cfun->arc_profile || flag_branch_probabilities)
	branch_prob ();

      /* Discover and record the loop depth at the head of each basic
	 block.  The loop infrastructure does the real job for us.  */
      flow_loops_find (&loops, LOOP_TREE);

      if (rtl_dump_file)
	flow_loops_dump (&loops, rtl_dump_file, NULL, 0);

      /* Estimate using heuristics if no profiling info is available.  */
      if (flag_guess_branch_prob)
	estimate_probability (&loops);

      flow_loops_free (&loops);
      close_dump_file (DFI_bp, print_rtl_with_bb, insns);
      timevar_pop (TV_BRANCH_PROB);
    }
  if (optimize > 0)
    {
      open_dump_file (DFI_ce1, decl);
      if (flag_if_conversion)
	{
	  timevar_push (TV_IFCVT);
	  if (rtl_dump_file)
	    dump_flow_info (rtl_dump_file);
	  cleanup_cfg (CLEANUP_EXPENSIVE);
	  reg_scan (insns, max_reg_num (), 0);
	  if_convert (0);
	  timevar_pop (TV_IFCVT);
	}
      timevar_push (TV_JUMP);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      reg_scan (insns, max_reg_num (), 0);
      timevar_pop (TV_JUMP);
      close_dump_file (DFI_ce1, print_rtl_with_bb, get_insns ());
    }
  if (flag_tracer)
    {
      timevar_push (TV_TRACER);
      open_dump_file (DFI_tracer, decl);
      if (rtl_dump_file)
	dump_flow_info (rtl_dump_file);
      tracer ();
      cleanup_cfg (CLEANUP_EXPENSIVE);
      reg_scan (insns, max_reg_num (), 0);
      close_dump_file (DFI_tracer, print_rtl_with_bb, get_insns ());
      timevar_pop (TV_TRACER);
    }

  if (flag_rerun_cse_after_loop)
    {
      timevar_push (TV_CSE2);
      open_dump_file (DFI_cse2, decl);
      if (rtl_dump_file)
	dump_flow_info (rtl_dump_file);
      /* CFG is no longer maintained up-to-date.  */
      tem = cse_main (insns, max_reg_num (), 1, rtl_dump_file);
      purge_all_dead_edges (0);
      delete_trivially_dead_insns (insns, max_reg_num ());

      if (tem)
	{
	  timevar_push (TV_JUMP);
	  rebuild_jump_labels (insns);
	  cleanup_cfg (CLEANUP_EXPENSIVE);
	  timevar_pop (TV_JUMP);
	}
      reg_scan (insns, max_reg_num (), 0);
      close_dump_file (DFI_cse2, print_rtl_with_bb, insns);
      ggc_collect ();
      timevar_pop (TV_CSE2);
    }

  cse_not_expected = 1;

  open_dump_file (DFI_life, decl);
  regclass_init ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  life_analysis (insns, rtl_dump_file, PROP_FINAL);
  if (optimize)
    cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_UPDATE_LIFE
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));
  timevar_pop (TV_FLOW);

  if (warn_uninitialized || extra_warnings)
    {
      uninitialized_vars_warning (DECL_INITIAL (decl));
      if (extra_warnings)
	setjmp_args_warning ();
    }

  if (optimize)
    {
      if (!flag_new_regalloc && initialize_uninitialized_subregs ())
	{
	  /* Insns were inserted, and possibly pseudos created, so
	     things might look a bit different.  */
	  insns = get_insns ();
	  allocate_reg_life_data ();
	  update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
			    PROP_LOG_LINKS | PROP_REG_INFO | PROP_DEATH_NOTES);
	}
    }

  no_new_pseudos = 1;

  close_dump_file (DFI_life, print_rtl_with_bb, insns);

  ggc_collect ();

  /* If -opt, try combining insns through substitution.  */

  if (optimize > 0)
    {
      int rebuild_jump_labels_after_combine = 0;

      timevar_push (TV_COMBINE);
      open_dump_file (DFI_combine, decl);

      rebuild_jump_labels_after_combine
	= combine_instructions (insns, max_reg_num ());

      /* Combining insns may have turned an indirect jump into a
	 direct jump.  Rebuid the JUMP_LABEL fields of jumping
	 instructions.  */
      if (rebuild_jump_labels_after_combine)
	{
	  timevar_push (TV_JUMP);
	  rebuild_jump_labels (insns);
	  timevar_pop (TV_JUMP);

	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
	}

      close_dump_file (DFI_combine, print_rtl_with_bb, insns);
      timevar_pop (TV_COMBINE);

      ggc_collect ();
    }

  /* Rerun if-conversion, as combine may have simplified things enough to
     now meet sequence length restrictions.  */
  if (flag_if_conversion)
    {
      timevar_push (TV_IFCVT);
      open_dump_file (DFI_ce2, decl);

      no_new_pseudos = 0;
      if_convert (1);
      no_new_pseudos = 1;

      close_dump_file (DFI_ce2, print_rtl_with_bb, insns);
      timevar_pop (TV_IFCVT);
    }

  /* Register allocation pre-pass, to reduce number of moves
     necessary for two-address machines.  */
  if (optimize > 0 && (flag_regmove || flag_expensive_optimizations))
    {
      timevar_push (TV_REGMOVE);
      open_dump_file (DFI_regmove, decl);

      regmove_optimize (insns, max_reg_num (), rtl_dump_file);

      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
      close_dump_file (DFI_regmove, print_rtl_with_bb, insns);
      timevar_pop (TV_REGMOVE);

      ggc_collect ();
    }

  /* Do unconditional splitting before register allocation to allow machine
     description to add extra information not needed previously.  */
  split_all_insns (1);

  /* Any of the several passes since flow1 will have munged register
     lifetime data a bit.  */
  register_life_up_to_date = 0;

#ifdef OPTIMIZE_MODE_SWITCHING
  timevar_push (TV_MODE_SWITCH);

  no_new_pseudos = 0;
  optimize_mode_switching (NULL);
  no_new_pseudos = 1;

  timevar_pop (TV_MODE_SWITCH);
#endif

  timevar_push (TV_SCHED);

#ifdef INSN_SCHEDULING

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */
  if (optimize > 0 && flag_schedule_insns)
    {
      open_dump_file (DFI_sched, decl);

      /* Do control and data sched analysis,
	 and write some of the results to dump file.  */

      schedule_insns (rtl_dump_file);

      close_dump_file (DFI_sched, print_rtl_with_bb, insns);

      /* Register lifetime information was updated as part of verifying
	 the schedule.  */
      register_life_up_to_date = 1;
    }
#endif
  timevar_pop (TV_SCHED);

  ggc_collect ();

  /* Determine if the current function is a leaf before running reload
     since this can impact optimizations done by the prologue and
     epilogue thus changing register elimination offsets.  */
  current_function_is_leaf = leaf_function_p ();

  timevar_push (TV_LOCAL_ALLOC);
  open_dump_file (DFI_lreg, decl);

  /* Allocate pseudo-regs that are used only within 1 basic block.

     RUN_JUMP_AFTER_RELOAD records whether or not we need to rerun the
     jump optimizer after register allocation and reloading are finished.  */

  if (! register_life_up_to_date)
    recompute_reg_usage (insns, ! optimize_size);

  if (flag_new_regalloc)
    {
      delete_trivially_dead_insns (insns, max_reg_num ());
      reg_alloc ();

      timevar_pop (TV_LOCAL_ALLOC);
      if (dump_file[DFI_lreg].enabled)
        {
          timevar_push (TV_DUMP);

          close_dump_file (DFI_lreg, NULL, NULL);
          timevar_pop (TV_DUMP);
        }

      /* XXX clean up the whole mess to bring live info in shape again.  */
      timevar_push (TV_GLOBAL_ALLOC);
      open_dump_file (DFI_greg, decl);

      build_insn_chain (insns);
      failure = reload (insns, 0);

      timevar_pop (TV_GLOBAL_ALLOC);

      if (dump_file[DFI_greg].enabled)
        {
          timevar_push (TV_DUMP);

          dump_global_regs (rtl_dump_file);

          close_dump_file (DFI_greg, print_rtl_with_bb, insns);
          timevar_pop (TV_DUMP);
        }

      if (failure)
        goto exit_rest_of_compilation;
      reload_completed = 1;
      rebuild_label_notes_after_reload = 0;
    }
  else
    {
      /* Allocate the reg_renumber array.  */
      allocate_reg_info (max_regno, FALSE, TRUE);

      /* And the reg_equiv_memory_loc array.  */
      reg_equiv_memory_loc = (rtx *) xcalloc (max_regno, sizeof (rtx));

      allocate_initial_values (reg_equiv_memory_loc);

      regclass (insns, max_reg_num (), rtl_dump_file);
      rebuild_label_notes_after_reload = local_alloc ();

      timevar_pop (TV_LOCAL_ALLOC);

      if (dump_file[DFI_lreg].enabled)
	{
	  timevar_push (TV_DUMP);

	  dump_flow_info (rtl_dump_file);
	  dump_local_alloc (rtl_dump_file);

	  close_dump_file (DFI_lreg, print_rtl_with_bb, insns);
	  timevar_pop (TV_DUMP);
	}

      ggc_collect ();

      timevar_push (TV_GLOBAL_ALLOC);
      open_dump_file (DFI_greg, decl);

      /* If optimizing, allocate remaining pseudo-regs.  Do the reload
	 pass fixing up any insns that are invalid.  */

      if (optimize)
	failure = global_alloc (rtl_dump_file);
      else
	{
	  build_insn_chain (insns);
	  failure = reload (insns, 0);
	}

      timevar_pop (TV_GLOBAL_ALLOC);

      if (dump_file[DFI_greg].enabled)
	{
	  timevar_push (TV_DUMP);

	  dump_global_regs (rtl_dump_file);

	  close_dump_file (DFI_greg, print_rtl_with_bb, insns);
	  timevar_pop (TV_DUMP);
	}

      if (failure)
	goto exit_rest_of_compilation;
    }

  ggc_collect ();

  open_dump_file (DFI_postreload, decl);

  /* Do a very simple CSE pass over just the hard registers.  */
  if (optimize > 0)
    {
      timevar_push (TV_RELOAD_CSE_REGS);
      reload_cse_regs (insns);
      timevar_pop (TV_RELOAD_CSE_REGS);
    }

  /* Register allocation and reloading may have turned an indirect jump into
     a direct jump.  If so, we must rebuild the JUMP_LABEL fields of
     jumping instructions.  */
  if (rebuild_label_notes_after_reload)
    {
      timevar_push (TV_JUMP);

      rebuild_jump_labels (insns);
      purge_all_dead_edges (0);

      timevar_pop (TV_JUMP);
    }

  close_dump_file (DFI_postreload, print_rtl_with_bb, insns);

  /* Re-create the death notes which were deleted during reload.  */
  timevar_push (TV_FLOW2);
  open_dump_file (DFI_flow2, decl);

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif

  /* If optimizing, then go ahead and split insns now.  */
#ifndef STACK_REGS
  if (optimize > 0)
#endif
    split_all_insns (0);

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);

  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */
  thread_prologue_and_epilogue_insns (insns);

  if (optimize)
    {
      life_analysis (insns, rtl_dump_file, PROP_FINAL);
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE
		   | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));

      /* This is kind of a heuristic.  We need to run combine_stack_adjustments
         even for machines with possibly nonzero RETURN_POPS_ARGS
         and ACCUMULATE_OUTGOING_ARGS.  We expect that only ports having
         push instructions will have popping returns.  */
#ifndef PUSH_ROUNDING
      if (!ACCUMULATE_OUTGOING_ARGS)
#endif
	combine_stack_adjustments ();

      ggc_collect ();
    }

  flow2_completed = 1;

  close_dump_file (DFI_flow2, print_rtl_with_bb, insns);
  timevar_pop (TV_FLOW2);

#ifdef HAVE_peephole2
  if (optimize > 0 && flag_peephole2)
    {
      timevar_push (TV_PEEPHOLE2);
      open_dump_file (DFI_peephole2, decl);

      peephole2_optimize (rtl_dump_file);

      close_dump_file (DFI_peephole2, print_rtl_with_bb, insns);
      timevar_pop (TV_PEEPHOLE2);
    }
#endif

  if (optimize > 0 && (flag_rename_registers || flag_cprop_registers))
    {
      timevar_push (TV_RENAME_REGISTERS);
      open_dump_file (DFI_rnreg, decl);

      if (flag_rename_registers)
	regrename_optimize ();
      if (flag_cprop_registers)
	copyprop_hardreg_forward ();

      close_dump_file (DFI_rnreg, print_rtl_with_bb, insns);
      timevar_pop (TV_RENAME_REGISTERS);
    }

  if (flag_if_conversion2)
    {
      timevar_push (TV_IFCVT2);
      open_dump_file (DFI_ce3, decl);

      if_convert (1);

      close_dump_file (DFI_ce3, print_rtl_with_bb, insns);
      timevar_pop (TV_IFCVT2);
    }

#ifdef INSN_SCHEDULING
  if (optimize > 0 && flag_schedule_insns_after_reload)
    {
      timevar_push (TV_SCHED2);
      open_dump_file (DFI_sched2, decl);

      /* Do control and data sched analysis again,
	 and write some more of the results to dump file.  */

      split_all_insns (1);

      schedule_insns (rtl_dump_file);

      close_dump_file (DFI_sched2, print_rtl_with_bb, insns);
      timevar_pop (TV_SCHED2);

      ggc_collect ();
    }
#endif

#ifdef LEAF_REGISTERS
  current_function_uses_only_leaf_regs
    = optimize > 0 && only_leaf_regs_used () && leaf_function_p ();
#endif

#ifdef STACK_REGS
  timevar_push (TV_REG_STACK);
  open_dump_file (DFI_stack, decl);

  reg_to_stack (insns, rtl_dump_file);

  close_dump_file (DFI_stack, print_rtl_with_bb, insns);
  timevar_pop (TV_REG_STACK);

  ggc_collect ();
#endif
  if (optimize > 0)
    {
      timevar_push (TV_REORDER_BLOCKS);
      open_dump_file (DFI_bbro, decl);

      /* Last attempt to optimize CFG, as scheduling, peepholing and insn
	 splitting possibly introduced more crossjumping oppurtuntities.
	 Except that we can't actually run crossjumping without running
	 another DCE pass, which we can't do after reg-stack.  */
      cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK
		   | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));
      if (flag_reorder_blocks)
	{
	  reorder_basic_blocks ();
	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK);
	}

      close_dump_file (DFI_bbro, print_rtl_with_bb, insns);
      timevar_pop (TV_REORDER_BLOCKS);
    }
  compute_alignments ();

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  /* If a machine dependent reorganization is needed, call it.  */
#ifdef MACHINE_DEPENDENT_REORG
  timevar_push (TV_MACH_DEP);
  open_dump_file (DFI_mach, decl);

  MACHINE_DEPENDENT_REORG (insns);

  close_dump_file (DFI_mach, print_rtl, insns);
  timevar_pop (TV_MACH_DEP);

  ggc_collect ();
#endif

  purge_line_number_notes (insns);
  cleanup_barriers ();

  /* If a scheduling pass for delayed branches is to be done,
     call the scheduling code.  */

#ifdef DELAY_SLOTS
  if (optimize > 0 && flag_delayed_branch)
    {
      timevar_push (TV_DBR_SCHED);
      open_dump_file (DFI_dbr, decl);

      dbr_schedule (insns, rtl_dump_file);

      close_dump_file (DFI_dbr, print_rtl, insns);
      timevar_pop (TV_DBR_SCHED);

      ggc_collect ();
    }
#endif

#if defined (HAVE_ATTR_length) && !defined (STACK_REGS)
  timevar_push (TV_SHORTEN_BRANCH);
  split_all_insns_noflow ();
  timevar_pop (TV_SHORTEN_BRANCH);
#endif

  convert_to_eh_region_ranges ();

  /* Shorten branches.  */
  timevar_push (TV_SHORTEN_BRANCH);
  shorten_branches (get_insns ());
  timevar_pop (TV_SHORTEN_BRANCH);

  set_nothrow_function_flags ();
  if (current_function_nothrow)
    /* Now we know that this can't throw; set the flag for the benefit
       of other functions later in this translation unit.  */
    TREE_NOTHROW (current_function_decl) = 1;

  /* Now turn the rtl into assembler code.  */

  timevar_push (TV_FINAL);
  {
    rtx x;
    const char *fnname;

    /* Get the function's name, as described by its RTL.  This may be
       different from the DECL_NAME name used in the source file.  */

    x = DECL_RTL (decl);
    if (GET_CODE (x) != MEM)
      abort ();
    x = XEXP (x, 0);
    if (GET_CODE (x) != SYMBOL_REF)
      abort ();
    fnname = XSTR (x, 0);

    assemble_start_function (decl, fnname);
    final_start_function (insns, asm_out_file, optimize);
    final (insns, asm_out_file, optimize, 0);
    final_end_function ();

#ifdef IA64_UNWIND_INFO
    /* ??? The IA-64 ".handlerdata" directive must be issued before
       the ".endp" directive that closes the procedure descriptor.  */
    output_function_exception_table ();
#endif

    assemble_end_function (decl, fnname);

#ifndef IA64_UNWIND_INFO
    /* Otherwise, it feels unclean to switch sections in the middle.  */
    output_function_exception_table ();
#endif

    if (! quiet_flag)
      fflush (asm_out_file);

    /* Release all memory allocated by flow.  */
    free_basic_block_vars (0);

    /* Release all memory held by regsets now.  */
    regset_release_memory ();
  }
  timevar_pop (TV_FINAL);

  ggc_collect ();

  /* Write DBX symbols if requested.  */

  /* Note that for those inline functions where we don't initially
     know for certain that we will be generating an out-of-line copy,
     the first invocation of this routine (rest_of_compilation) will
     skip over this code by doing a `goto exit_rest_of_compilation;'.
     Later on, wrapup_global_declarations will (indirectly) call
     rest_of_compilation again for those inline functions that need
     to have out-of-line copies generated.  During that call, we
     *will* be routed past here.  */

  timevar_push (TV_SYMOUT);
  (*debug_hooks->function_decl) (decl);
  timevar_pop (TV_SYMOUT);

 exit_rest_of_compilation:

  /* In case the function was not output,
     don't leave any temporary anonymous types
     queued up for sdb output.  */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_types (NULL_TREE);
#endif

  reload_completed = 0;
  flow2_completed = 0;
  no_new_pseudos = 0;

  timevar_push (TV_FINAL);

  /* Clear out the insn_length contents now that they are no
     longer valid.  */
  init_insn_lengths ();

  /* Show no temporary slots allocated.  */
  init_temp_slots ();

  free_basic_block_vars (0);
  free_bb_for_insn ();

  timevar_pop (TV_FINAL);

  /* Make sure volatile mem refs aren't considered valid operands for
     arithmetic insns.  We must call this here if this is a nested inline
     function, since the above code leaves us in the init_recog state
     (from final.c), and the function context push/pop code does not
     save/restore volatile_ok.

     ??? Maybe it isn't necessary for expand_start_function to call this
     anymore if we do it here?  */

  init_recog_no_volatile ();

  /* We're done with this function.  Free up memory if we can.  */
  free_after_parsing (cfun);
  if (! DECL_DEFER_OUTPUT (decl))
    {
      free_after_compilation (cfun);

      /* Clear integrate.c's pointer to the cfun structure we just
	 destroyed.  */
      DECL_SAVED_INSNS (decl) = 0;
    }
  cfun = 0;

  ggc_collect ();

  timevar_pop (TV_REST_OF_COMPILATION);
}

static void
display_help ()
{
  int undoc;
  unsigned long i;
  const char *lang;

  printf (_("  -ffixed-<register>      Mark <register> as being unavailable to the compiler\n"));
  printf (_("  -fcall-used-<register>  Mark <register> as being corrupted by function calls\n"));
  printf (_("  -fcall-saved-<register> Mark <register> as being preserved across functions\n"));
  printf (_("  -finline-limit=<number> Limits the size of inlined functions to <number>\n"));
  printf (_("  -fmessage-length=<number> Limits diagnostics messages lengths to <number> characters per line.  0 suppresses line-wrapping\n"));
  printf (_("  -fdiagnostics-show-location=[once | every-line] Indicates how often source location information should be emitted, as prefix, at the beginning of diagnostics when line-wrapping\n"));
  printf (_("  -ftls-model=[global-dynamic | local-dynamic | initial-exec | local-exec] Indicates the default thread-local storage code generation model\n"));
  printf (_("  -fstack-limit-register=<register>  Trap if the stack goes past <register>\n"));
  printf (_("  -fstack-limit-symbol=<name>  Trap if the stack goes past symbol <name>\n"));
  printf (_("  -frandom-seed=<string>  Make compile reproducible using <string>\n"));
  

  for (i = ARRAY_SIZE (f_options); i--;)
    {
      const char *description = f_options[i].description;

      if (description != NULL && *description != 0)
	printf ("  -f%-21s %s\n",
		f_options[i].string, _(description));
    }

  printf (_("  -O[number]              Set optimization level to [number]\n"));
  printf (_("  -Os                     Optimize for space rather than speed\n"));
  for (i = LAST_PARAM; i--;)
    {
      const char *description = compiler_params[i].help;
      const int length = 21 - strlen (compiler_params[i].option);

      if (description != NULL && *description != 0)
	printf ("  --param %s=<value>%.*s%s\n",
		compiler_params[i].option,
		length > 0 ? length : 1, "                     ",
		_(description));
    }
  printf (_("  -pedantic               Issue warnings needed by strict compliance to ISO C\n"));
  printf (_("  -pedantic-errors        Like -pedantic except that errors are produced\n"));
  printf (_("  -w                      Suppress warnings\n"));
  printf (_("  -W                      Enable extra warnings\n"));

  for (i = ARRAY_SIZE (W_options); i--;)
    {
      const char *description = W_options[i].description;

      if (description != NULL && *description != 0)
	printf ("  -W%-21s %s\n",
		W_options[i].string, _(description));
    }

  printf (_("  -Wunused                Enable unused warnings\n"));
  printf (_("  -Wlarger-than-<number>  Warn if an object is larger than <number> bytes\n"));
  printf (_("  -p                      Enable function profiling\n"));
  printf (_("  -o <file>               Place output into <file> \n"));
  printf (_("\
  -G <number>             Put global and static data smaller than <number>\n\
                          bytes into a special section (on some targets)\n"));

  for (i = ARRAY_SIZE (debug_args); i--;)
    {
      if (debug_args[i].description != NULL)
	printf ("  -g%-21s %s\n",
		debug_args[i].arg, _(debug_args[i].description));
    }

  printf (_("  -aux-info <file>        Emit declaration info into <file>\n"));
  printf (_("  -quiet                  Do not display functions compiled or elapsed time\n"));
  printf (_("  -version                Display the compiler's version\n"));
  printf (_("  -d[letters]             Enable dumps from specific passes of the compiler\n"));
  printf (_("  -dumpbase <file>        Base name to be used for dumps from specific passes\n"));
#if defined INSN_SCHEDULING
  printf (_("  -fsched-verbose=<number> Set the verbosity level of the scheduler\n"));
#endif
  printf (_("  --help                  Display this information\n"));

  undoc = 0;
  lang  = "language";

  /* Display descriptions of language specific options.
     If there is no description, note that there is an undocumented option.
     If the description is empty, do not display anything.  (This allows
     options to be deliberately undocumented, for whatever reason).
     If the option string is missing, then this is a marker, indicating
     that the description string is in fact the name of a language, whose
     language specific options are to follow.  */

  if (ARRAY_SIZE (documented_lang_options) > 1)
    {
      printf (_("\nLanguage specific options:\n"));

      for (i = 0; i < ARRAY_SIZE (documented_lang_options); i++)
	{
	  const char *description = documented_lang_options[i].description;
	  const char *option      = documented_lang_options[i].option;

	  if (description == NULL)
	    {
	      undoc = 1;

	      if (extra_warnings)
		printf (_("  %-23.23s [undocumented]\n"), option);
	    }
	  else if (*description == 0)
	    continue;
	  else if (option == NULL)
	    {
	      if (undoc)
		printf
		  (_("\nThere are undocumented %s specific options as well.\n"),
			lang);
	      undoc = 0;

	      printf (_("\n Options for %s:\n"), description);

	      lang = description;
	    }
	  else
	    printf ("  %-23.23s %s\n", option, _(description));
	}
    }

  if (undoc)
    printf (_("\nThere are undocumented %s specific options as well.\n"),
	    lang);

  display_target_options ();
}

static void
display_target_options ()
{
  int undoc, i;
  static bool displayed = false;

  /* Avoid double printing for --help --target-help.  */
  if (displayed)
    return;
  displayed = true;

  if (ARRAY_SIZE (target_switches) > 1
#ifdef TARGET_OPTIONS
      || ARRAY_SIZE (target_options) > 1
#endif
      )
    {
      int doc = 0;

      undoc = 0;

      printf (_("\nTarget specific options:\n"));

      for (i = ARRAY_SIZE (target_switches); i--;)
	{
	  const char *option      = target_switches[i].name;
	  const char *description = target_switches[i].description;

	  if (option == NULL || *option == 0)
	    continue;
	  else if (description == NULL)
	    {
	      undoc = 1;

	      if (extra_warnings)
		printf (_("  -m%-23.23s [undocumented]\n"), option);
	    }
	  else if (*description != 0)
	    doc += printf ("  -m%-23.23s %s\n", option, _(description));
	}

#ifdef TARGET_OPTIONS
      for (i = ARRAY_SIZE (target_options); i--;)
	{
	  const char *option      = target_options[i].prefix;
	  const char *description = target_options[i].description;

	  if (option == NULL || *option == 0)
	    continue;
	  else if (description == NULL)
	    {
	      undoc = 1;

	      if (extra_warnings)
		printf (_("  -m%-23.23s [undocumented]\n"), option);
	    }
	  else if (*description != 0)
	    doc += printf ("  -m%-23.23s %s\n", option, _(description));
	}
#endif
      if (undoc)
	{
	  if (doc)
	    printf (_("\nThere are undocumented target specific options as well.\n"));
	  else
	    printf (_("  They exist, but they are not documented.\n"));
	}
    }
}

/* Parse a -d... command line switch.  */

static void
decode_d_option (arg)
     const char *arg;
{
  int i, c, matched;

  while (*arg)
    switch (c = *arg++)
      {
      case 'a':
	for (i = 0; i < (int) DFI_MAX; ++i)
	  dump_file[i].enabled = 1;
	break;
      case 'A':
	flag_debug_asm = 1;
	break;
      case 'p':
	flag_print_asm_name = 1;
	break;
      case 'P':
	flag_dump_rtl_in_asm = 1;
	flag_print_asm_name = 1;
	break;
      case 'v':
	graph_dump_format = vcg;
	break;
      case 'x':
	rtl_dump_and_exit = 1;
	break;
      case 'y':
	set_yydebug = 1;
	break;
      case 'D':	/* These are handled by the preprocessor.  */
      case 'I':
	break;

      default:
	matched = 0;
	for (i = 0; i < (int) DFI_MAX; ++i)
	  if (c == dump_file[i].debug_switch)
	    {
	      dump_file[i].enabled = 1;
	      matched = 1;
	    }

	if (! matched)
	  warning ("unrecognized gcc debugging option: %c", c);
	break;
      }
}

/* Parse a -f... command line switch.  ARG is the value after the -f.
   It is safe to access 'ARG - 2' to generate the full switch name.
   Return the number of strings consumed.  */

static int
decode_f_option (arg)
     const char *arg;
{
  int j;
  const char *option_value = NULL;

  /* Search for the option in the table of binary f options.  */
  for (j = ARRAY_SIZE (f_options); j--;)
    {
      if (!strcmp (arg, f_options[j].string))
	{
	  *f_options[j].variable = f_options[j].on_value;
	  return 1;
	}

      if (arg[0] == 'n' && arg[1] == 'o' && arg[2] == '-'
	  && ! strcmp (arg + 3, f_options[j].string))
	{
	  *f_options[j].variable = ! f_options[j].on_value;
	  return 1;
	}
    }

  if (!strcmp (arg, "fast-math"))
    set_fast_math_flags (1);
  else if (!strcmp (arg, "no-fast-math"))
    set_fast_math_flags (0);
  else if ((option_value = skip_leading_substring (arg, "inline-limit-"))
	   || (option_value = skip_leading_substring (arg, "inline-limit=")))
    {
      int val =
	read_integral_parameter (option_value, arg - 2,
				 MAX_INLINE_INSNS);
      set_param_value ("max-inline-insns", val);
      set_param_value ("max-inline-insns-single", val/2);
      set_param_value ("max-inline-insns-auto", val/2);
      set_param_value ("max-inline-insns-rtl", val);
      if (val/4 < MIN_INLINE_INSNS)
	{
	  if (val/4 > 10)
	    set_param_value ("min-inline-insns", val/4);
	  else
	    set_param_value ("min-inline-insns", 10);
	}
    }
  else if ((option_value = skip_leading_substring (arg, "tls-model=")))
    {
      if (strcmp (option_value, "global-dynamic") == 0)
	flag_tls_default = TLS_MODEL_GLOBAL_DYNAMIC;
      else if (strcmp (option_value, "local-dynamic") == 0)
	flag_tls_default = TLS_MODEL_LOCAL_DYNAMIC;
      else if (strcmp (option_value, "initial-exec") == 0)
	flag_tls_default = TLS_MODEL_INITIAL_EXEC;
      else if (strcmp (option_value, "local-exec") == 0)
	flag_tls_default = TLS_MODEL_LOCAL_EXEC;
      else
	warning ("`%s': unknown tls-model option", arg - 2);
    }
#ifdef INSN_SCHEDULING
  else if ((option_value = skip_leading_substring (arg, "sched-verbose=")))
    fix_sched_param ("verbose", option_value);
#endif
  else if ((option_value = skip_leading_substring (arg, "fixed-")))
    fix_register (option_value, 1, 1);
  else if ((option_value = skip_leading_substring (arg, "call-used-")))
    fix_register (option_value, 0, 1);
  else if ((option_value = skip_leading_substring (arg, "call-saved-")))
    fix_register (option_value, 0, 0);
  else if ((option_value = skip_leading_substring (arg, "align-loops=")))
    align_loops = read_integral_parameter (option_value, arg - 2, align_loops);
  else if ((option_value = skip_leading_substring (arg, "align-functions=")))
    align_functions
      = read_integral_parameter (option_value, arg - 2, align_functions);
  else if ((option_value = skip_leading_substring (arg, "align-jumps=")))
    align_jumps = read_integral_parameter (option_value, arg - 2, align_jumps);
  else if ((option_value = skip_leading_substring (arg, "align-labels=")))
    align_labels
      = read_integral_parameter (option_value, arg - 2, align_labels);
  else if ((option_value
	    = skip_leading_substring (arg, "stack-limit-register=")))
    {
      int reg = decode_reg_name (option_value);
      if (reg < 0)
	error ("unrecognized register name `%s'", option_value);
      else
	stack_limit_rtx = gen_rtx_REG (Pmode, reg);
    }
  else if ((option_value
	    = skip_leading_substring (arg, "stack-limit-symbol=")))
    {
      const char *nm;
      nm = ggc_strdup (option_value);
      stack_limit_rtx = gen_rtx_SYMBOL_REF (Pmode, nm);
    }
  else if ((option_value
	    = skip_leading_substring (arg, "message-length=")))
    output_set_maximum_length
      (&global_dc->buffer, read_integral_parameter
       (option_value, arg - 2, diagnostic_line_cutoff (global_dc)));
  else if ((option_value
	    = skip_leading_substring (arg, "diagnostics-show-location=")))
    {
      if (!strcmp (option_value, "once"))
	diagnostic_prefixing_rule (global_dc) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
      else if (!strcmp (option_value, "every-line"))
	diagnostic_prefixing_rule (global_dc)
	  = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
      else
	error ("unrecognized option `%s'", arg - 2);
    }
  else if (!strcmp (arg, "no-stack-limit"))
    stack_limit_rtx = NULL_RTX;
  else if ((option_value = skip_leading_substring (arg, "random-seed=")))
    flag_random_seed = option_value;
  else if (!strcmp (arg, "no-random-seed"))
    flag_random_seed = NULL;
  else if (!strcmp (arg, "preprocessed"))
    /* Recognize this switch but do nothing.  This prevents warnings
       about an unrecognized switch if cpplib has not been linked in.  */
    ;
  else
    return 0;

  return 1;
}

/* Parse a -W... command line switch.  ARG is the value after the -W.
   It is safe to access 'ARG - 2' to generate the full switch name.
   Return the number of strings consumed.  */

static int
decode_W_option (arg)
     const char *arg;
{
  const char *option_value = NULL;
  int j;

  /* Search for the option in the table of binary W options.  */

  for (j = ARRAY_SIZE (W_options); j--;)
    {
      if (!strcmp (arg, W_options[j].string))
	{
	  *W_options[j].variable = W_options[j].on_value;
	  return 1;
	}

      if (arg[0] == 'n' && arg[1] == 'o' && arg[2] == '-'
	  && ! strcmp (arg + 3, W_options[j].string))
	{
	  *W_options[j].variable = ! W_options[j].on_value;
	  return 1;
	}
    }

  if ((option_value = skip_leading_substring (arg, "id-clash-")))
    warning ("-Wid-clash-LEN is no longer supported");
  else if ((option_value = skip_leading_substring (arg, "larger-than-")))
    {
      larger_than_size = read_integral_parameter (option_value, arg - 2, -1);

      warn_larger_than = larger_than_size != -1;
    }
  else if (!strcmp (arg, "unused"))
    {
      set_Wunused (1);
    }
  else if (!strcmp (arg, "no-unused"))
    {
      set_Wunused (0);
    }
  else
    return 0;

  return 1;
}

/* Parse a -g... command line switch.  ARG is the value after the -g.
   It is safe to access 'ARG - 2' to generate the full switch name.
   Return the number of strings consumed.  */

static int
decode_g_option (arg)
     const char *arg;
{
  static unsigned level = 0;
  /* A lot of code assumes write_symbols == NO_DEBUG if the
     debugging level is 0 (thus -gstabs1 -gstabs0 would lose track
     of what debugging type has been selected).  This records the
     selected type.  It is an error to specify more than one
     debugging type.  */
  static enum debug_info_type selected_debug_type = NO_DEBUG;
  /* Non-zero if debugging format has been explicitly set.
     -g and -ggdb don't explicitly set the debugging format so
     -gdwarf -g3 is equivalent to -gdwarf3.  */
  static int type_explicitly_set_p = 0;
  /* Indexed by enum debug_info_type.  */
  static const char *const debug_type_names[] =
  {
    "none", "stabs", "coff", "dwarf-1", "dwarf-2", "xcoff", "vms"
  };

  /* The maximum admissible debug level value.  */
  static const unsigned max_debug_level = 3;

  /* Look up ARG in the table.  */
  for (da = debug_args; da->arg; da++)
    {
      const int da_len = strlen (da->arg);

      if (da_len == 0 || ! strncmp (arg, da->arg, da_len))
	{
	  enum debug_info_type type = da->debug_type;
	  const char *p = arg + da_len;

	  if (*p && ! ISDIGIT (*p))
	    continue;

	  /* A debug flag without a level defaults to level 2.
	     Note we do not want to call read_integral_parameter
	     for that case since it will call atoi which
	     will return zero.

	     ??? We may want to generalize the interface to
	     read_integral_parameter to better handle this case
	     if this case shows up often.  */
	  if (*p)
	    level = read_integral_parameter (p, 0, max_debug_level + 1);
	  else
	    level = (level == 0) ? 2 : level;

	  if (da_len > 1 && *p && !strncmp (arg, "dwarf", da_len))
	    {
	      error ("use -gdwarf -g%d for DWARF v1, level %d",
		     level, level);
	      if (level == 2)
		error ("use -gdwarf-2   for DWARF v2");
	    }

	  if (level > max_debug_level)
	    {
	      warning ("\
ignoring option `%s' due to invalid debug level specification",
		       arg - 2);
	      level = debug_info_level;
	    }

	  if (type == NO_DEBUG)
	    {
	      type = PREFERRED_DEBUGGING_TYPE;

	      if (da_len > 1 && strncmp (arg, "gdb", da_len) == 0)
		{
#ifdef DWARF2_DEBUGGING_INFO
		  type = DWARF2_DEBUG;
#else
#ifdef DBX_DEBUGGING_INFO
		  type = DBX_DEBUG;
#endif
#endif
		}
	    }

	  if (type == NO_DEBUG)
	    warning ("`%s': unknown or unsupported -g option", arg - 2);

	  /* Does it conflict with an already selected type?  */
	  if (type_explicitly_set_p
	      /* -g/-ggdb don't conflict with anything.  */
	      && da->debug_type != NO_DEBUG
	      && type != selected_debug_type)
	    warning ("`%s' ignored, conflicts with `-g%s'",
		     arg - 2, debug_type_names[(int) selected_debug_type]);
	  else
	    {
	      /* If the format has already been set, -g/-ggdb
		 only change the debug level.  */
	      if (type_explicitly_set_p && da->debug_type == NO_DEBUG)
		/* Don't change debugging type.  */
		;
	      else
		{
		  selected_debug_type = type;
		  type_explicitly_set_p = da->debug_type != NO_DEBUG;
		}

	      write_symbols = (level == 0
			       ? NO_DEBUG
			       : selected_debug_type);
	      use_gnu_debug_info_extensions = da->use_extensions_p;
	      debug_info_level = (enum debug_info_level) level;
	    }

	  break;
	}
    }

  if (! da->arg)
    return 0;

  return 1;
}

/* Decode the first argument in the argv as a language-independent option.
   Return the number of strings consumed.  */

static unsigned int
independent_decode_option (argc, argv)
     int argc;
     char **argv;
{
  char *arg = argv[0];

  if (arg[0] != '-' || arg[1] == 0)
    {
      if (arg[0] == '+')
	return 0;

      filename = arg;

      return 1;
    }

  arg++;

  if (!strcmp (arg, "-help"))
    {
      display_help ();
      exit_after_options = 1;
      return 1;
    }

  if (!strcmp (arg, "-target-help"))
    {
      display_target_options ();
      exit_after_options = 1;
      return 1;
    }

  if (!strcmp (arg, "-version"))
    {
      print_version (stderr, "");
      exit_after_options = 1;
      return 1;
    }

  /* Handle '--param <name>=<value>'.  */
  if (strcmp (arg, "-param") == 0)
    {
      char *equal;

      if (argc == 1)
	{
	  error ("-param option missing argument");
	  return 1;
	}

      /* Get the '<name>=<value>' parameter.  */
      arg = argv[1];
      /* Look for the `='.  */
      equal = strchr (arg, '=');
      if (!equal)
	error ("invalid --param option: %s", arg);
      else
	{
	  int val;

	  /* Zero out the `=' sign so that we get two separate strings.  */
	  *equal = '\0';
	  /* Figure out what value is specified.  */
	  val = read_integral_parameter (equal + 1, NULL, INVALID_PARAM_VAL);
	  if (val != INVALID_PARAM_VAL)
	    set_param_value (arg, val);
	  else
	    error ("invalid parameter value `%s'", equal + 1);
	}

      return 2;
    }

  if (*arg == 'Y')
    arg++;

  switch (*arg)
    {
    default:
      return 0;

    case 'O':
      /* Already been treated in main (). Do nothing.  */
      break;

    case 'm':
      set_target_switch (arg + 1);
      break;

    case 'f':
      return decode_f_option (arg + 1);

    case 'g':
      return decode_g_option (arg + 1);

    case 'd':
      if (!strcmp (arg, "dumpbase"))
	{
	  if (argc == 1)
	    return 0;

	  if (argv[1][0])
	    dump_base_name = argv[1];
	  
	  return 2;
	}
      else
	decode_d_option (arg + 1);
      break;

    case 'p':
      if (!strcmp (arg, "pedantic"))
	pedantic = 1;
      else if (!strcmp (arg, "pedantic-errors"))
	flag_pedantic_errors = pedantic = 1;
      else if (arg[1] == 0)
	profile_flag = 1;
      else
	return 0;
      break;

    case 'q':
      if (!strcmp (arg, "quiet"))
	quiet_flag = 1;
      else
	return 0;
      break;

    case 'v':
      if (!strcmp (arg, "version"))
	version_flag = 1;
      else
	return 0;
      break;

    case 'w':
      if (arg[1] == 0)
	inhibit_warnings = 1;
      else
	return 0;
      break;

    case 'W':
      if (arg[1] == 0)
	{
	  extra_warnings = 1;
	  /* We save the value of warn_uninitialized, since if they put
	     -Wuninitialized on the command line, we need to generate a
	     warning about not using it without also specifying -O.  */
	  if (warn_uninitialized != 1)
	    warn_uninitialized = 2;
	}
      else
	return decode_W_option (arg + 1);
      break;

    case 'a':
      if (!strncmp (arg, "aux-info", 8))
	{
	  if (arg[8] == '\0')
	    {
	      if (argc == 1)
		return 0;

	      aux_info_file_name = argv[1];
	      flag_gen_aux_info = 1;
	      return 2;
	    }
	  else if (arg[8] == '=')
	    {
	      aux_info_file_name = arg + 9;
	      flag_gen_aux_info = 1;
	    }
	  else
	    return 0;
	}
      else if (!strcmp (arg, "auxbase"))
	{
	  if (argc == 1)
	    return 0;

	  if (argv[1][0])
	    aux_base_name = argv[1];
	  
	  return 2;
	}
      else if (!strcmp (arg, "auxbase-strip"))
	{
	  if (argc == 1)
	    return 0;

	  if (argv[1][0])
	    {
	      strip_off_ending (argv[1], strlen (argv[1]));
	      if (argv[1][0])
		aux_base_name = argv[1];
	    }
	  
	  return 2;
	}
      else
	return 0;
      break;

    case 'o':
      if (arg[1] == 0)
	{
	  if (argc == 1)
	    return 0;

	  asm_file_name = argv[1];
	  return 2;
	}
      return 0;

    case 'G':
      {
	int g_switch_val;
	int return_val;

	if (arg[1] == 0)
	  {
	    if (argc == 1)
	      return 0;

	    g_switch_val = read_integral_parameter (argv[1], 0, -1);
	    return_val = 2;
	  }
	else
	  {
	    g_switch_val = read_integral_parameter (arg + 1, 0, -1);
	    return_val = 1;
	  }

	if (g_switch_val == -1)
	  return_val = 0;
	else
	  {
	    g_switch_set = TRUE;
	    g_switch_value = g_switch_val;
	  }

	return return_val;
      }
    }

  return 1;
}

/* Decode -m switches.  */
/* Decode the switch -mNAME.  */

static void
set_target_switch (name)
     const char *name;
{
  size_t j;
  int valid_target_option = 0;

  for (j = 0; j < ARRAY_SIZE (target_switches); j++)
    if (!strcmp (target_switches[j].name, name))
      {
	if (target_switches[j].value < 0)
	  target_flags &= ~-target_switches[j].value;
	else
	  target_flags |= target_switches[j].value;
	if (name[0] != 0)
	  {
	    if (target_switches[j].value < 0)
	      target_flags_explicit |= -target_switches[j].value;
	    else
	      target_flags_explicit |= target_switches[j].value;
	  }
	valid_target_option = 1;
      }

#ifdef TARGET_OPTIONS
  if (!valid_target_option)
    for (j = 0; j < ARRAY_SIZE (target_options); j++)
      {
	int len = strlen (target_options[j].prefix);
	if (!strncmp (target_options[j].prefix, name, len))
	  {
	    *target_options[j].variable = name + len;
	    valid_target_option = 1;
	  }
      }
#endif

  if (!valid_target_option)
    error ("invalid option `%s'", name);
}

/* Print version information to FILE.
   Each line begins with INDENT (for the case where FILE is the
   assembler output file).  */

static void
print_version (file, indent)
     FILE *file;
     const char *indent;
{
#ifndef __VERSION__
#define __VERSION__ "[?]"
#endif
  fnotice (file,
#ifdef __GNUC__
	   "%s%s%s version %s (%s)\n%s\tcompiled by GNU C version %s.\n"
#else
	   "%s%s%s version %s (%s) compiled by CC.\n"
#endif
	   , indent, *indent != 0 ? " " : "",
	   lang_hooks.name, version_string, TARGET_NAME,
	   indent, __VERSION__);
  fnotice (file, "%s%sGGC heuristics: --param ggc-min-expand=%d --param ggc-min-heapsize=%d\n",
	   indent, *indent != 0 ? " " : "",
	   PARAM_VALUE (GGC_MIN_EXPAND), PARAM_VALUE (GGC_MIN_HEAPSIZE));
}

/* Print an option value and return the adjusted position in the line.
   ??? We don't handle error returns from fprintf (disk full); presumably
   other code will catch a disk full though.  */

static int
print_single_switch (file, pos, max, indent, sep, term, type, name)
     FILE *file;
     int pos, max;
     const char *indent, *sep, *term, *type, *name;
{
  /* The ultrix fprintf returns 0 on success, so compute the result we want
     here since we need it for the following test.  */
  int len = strlen (sep) + strlen (type) + strlen (name);

  if (pos != 0
      && pos + len > max)
    {
      fprintf (file, "%s", term);
      pos = 0;
    }
  if (pos == 0)
    {
      fprintf (file, "%s", indent);
      pos = strlen (indent);
    }
  fprintf (file, "%s%s%s", sep, type, name);
  pos += len;
  return pos;
}

/* Print active target switches to FILE.
   POS is the current cursor position and MAX is the size of a "line".
   Each line begins with INDENT and ends with TERM.
   Each switch is separated from the next by SEP.  */

static void
print_switch_values (file, pos, max, indent, sep, term)
     FILE *file;
     int pos, max;
     const char *indent, *sep, *term;
{
  size_t j;
  char **p;

  /* Fill in the -frandom-seed option, if the user didn't pass it, so
     that it can be printed below.  This helps reproducibility.  Of
     course, the string may never be used, but we can't tell that at
     this point in the compile.  */
  default_flag_random_seed ();

  /* Print the options as passed.  */

  pos = print_single_switch (file, pos, max, indent, *indent ? " " : "", term,
			     _("options passed: "), "");

  for (p = &save_argv[1]; *p != NULL; p++)
    if (**p == '-')
      {
	/* Ignore these.  */
	if (strcmp (*p, "-o") == 0)
	  {
	    if (p[1] != NULL)
	      p++;
	    continue;
	  }
	if (strcmp (*p, "-quiet") == 0)
	  continue;
	if (strcmp (*p, "-version") == 0)
	  continue;
	if ((*p)[1] == 'd')
	  continue;

	pos = print_single_switch (file, pos, max, indent, sep, term, *p, "");
      }
  if (pos > 0)
    fprintf (file, "%s", term);

  /* Print the -f and -m options that have been enabled.
     We don't handle language specific options but printing argv
     should suffice.  */

  pos = print_single_switch (file, 0, max, indent, *indent ? " " : "", term,
			     _("options enabled: "), "");

  for (j = 0; j < ARRAY_SIZE (f_options); j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = print_single_switch (file, pos, max, indent, sep, term,
				 "-f", f_options[j].string);

  /* Print target specific options.  */

  for (j = 0; j < ARRAY_SIZE (target_switches); j++)
    if (target_switches[j].name[0] != '\0'
	&& target_switches[j].value > 0
	&& ((target_switches[j].value & target_flags)
	    == target_switches[j].value))
      {
	pos = print_single_switch (file, pos, max, indent, sep, term,
				   "-m", target_switches[j].name);
      }

#ifdef TARGET_OPTIONS
  for (j = 0; j < ARRAY_SIZE (target_options); j++)
    if (*target_options[j].variable != NULL)
      {
	char prefix[256];
	sprintf (prefix, "-m%s", target_options[j].prefix);
	pos = print_single_switch (file, pos, max, indent, sep, term,
				   prefix, *target_options[j].variable);
      }
#endif

  fprintf (file, "%s", term);
}

/* Open assembly code output file.  Do this even if -fsyntax-only is
   on, because then the driver will have provided the name of a
   temporary file or bit bucket for us.  NAME is the file specified on
   the command line, possibly NULL.  */
static void
init_asm_output (name)
     const char *name;
{
  if (name == NULL && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      if (asm_file_name == 0)
	{
	  int len = strlen (dump_base_name);
	  char *dumpname = (char *) xmalloc (len + 6);
	  memcpy (dumpname, dump_base_name, len + 1);
	  strip_off_ending (dumpname, len);
	  strcat (dumpname, ".s");
	  asm_file_name = dumpname;
	}
      if (!strcmp (asm_file_name, "-"))
	asm_out_file = stdout;
      else
	asm_out_file = fopen (asm_file_name, "w");
      if (asm_out_file == 0)
	fatal_io_error ("can't open %s for writing", asm_file_name);
    }

#ifdef IO_BUFFER_SIZE
  setvbuf (asm_out_file, (char *) xmalloc (IO_BUFFER_SIZE),
	   _IOFBF, IO_BUFFER_SIZE);
#endif

  if (!flag_syntax_only)
    {
#ifdef ASM_FILE_START
      ASM_FILE_START (asm_out_file);
#endif

#ifdef ASM_COMMENT_START
      if (flag_verbose_asm)
	{
	  /* Print the list of options in effect.  */
	  print_version (asm_out_file, ASM_COMMENT_START);
	  print_switch_values (asm_out_file, 0, MAX_LINE,
			       ASM_COMMENT_START, " ", "\n");
	  /* Add a blank line here so it appears in assembler output but not
	     screen output.  */
	  fprintf (asm_out_file, "\n");
	}
#endif
    }
}

/* Initialization of the front end environment, before command line
   options are parsed.  Signal handlers, internationalization etc.
   ARGV0 is main's argv[0].  */
static void
general_init (argv0)
     char *argv0;
{
  char *p;

  p = argv0 + strlen (argv0);
  while (p != argv0 && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  hex_init ();

  gcc_init_libintl ();

  /* Trap fatal signals, e.g. SIGSEGV, and convert them to ICE messages.  */
#ifdef SIGSEGV
  signal (SIGSEGV, crash_signal);
#endif
#ifdef SIGILL
  signal (SIGILL, crash_signal);
#endif
#ifdef SIGBUS
  signal (SIGBUS, crash_signal);
#endif
#ifdef SIGABRT
  signal (SIGABRT, crash_signal);
#endif
#if defined SIGIOT && (!defined SIGABRT || SIGABRT != SIGIOT)
  signal (SIGIOT, crash_signal);
#endif
#ifdef SIGFPE
  signal (SIGFPE, crash_signal);
#endif

  /* Initialize the diagnostics reporting machinery, so option parsing
     can give warnings and errors.  */
  diagnostic_initialize (global_dc);

  /* Initialize the garbage-collector, string pools and tree type hash
     table.  */
  init_ggc ();
  init_stringpool ();
  init_ttree ();
}

/* Parse command line options and set default flag values, called
   after language-independent option-independent initialization.  Do
   minimal options processing.  Outputting diagnostics is OK, but GC
   and identifier hashtables etc. are not initialized yet.

   Return nonzero to suppress compiler back end initialization.  */
static void
parse_options_and_default_flags (argc, argv)
     int argc;
     char **argv;
{
  int i;

  /* Save in case md file wants to emit args as a comment.  */
  save_argc = argc;
  save_argv = argv;

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Register the language-independent parameters.  */
  add_params (lang_independent_params, LAST_PARAM);

  /* This must be done after add_params but before argument processing.  */
  init_ggc_heuristics();

  /* Perform language-specific options initialization.  */
  (*lang_hooks.init_options) ();

  /* Scan to see what optimization level has been specified.  That will
     determine the default value of many flags.  */
  for (i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i], "-O"))
	{
	  optimize = 1;
	  optimize_size = 0;
	}
      else if (argv[i][0] == '-' && argv[i][1] == 'O')
	{
	  /* Handle -Os, -O2, -O3, -O69, ...  */
	  char *p = &argv[i][2];

	  if ((p[0] == 's') && (p[1] == 0))
	    {
	      optimize_size = 1;

	      /* Optimizing for size forces optimize to be 2.  */
	      optimize = 2;
	    }
	  else
	    {
	      const int optimize_val = read_integral_parameter (p, p - 2, -1);
	      if (optimize_val != -1)
		{
		  optimize = optimize_val;
		  optimize_size = 0;
		}
	    }
	}
    }

  if (!optimize)
    {
      flag_merge_constants = 0;
    }

  if (optimize >= 1)
    {
      flag_defer_pop = 1;
      flag_thread_jumps = 1;
#ifdef DELAY_SLOTS
      flag_delayed_branch = 1;
#endif
#ifdef CAN_DEBUG_WITHOUT_FP
      flag_omit_frame_pointer = 1;
#endif
      flag_guess_branch_prob = 1;
      flag_cprop_registers = 1;
      flag_loop_optimize = 1;
      flag_crossjumping = 1;
      flag_if_conversion = 1;
      flag_if_conversion2 = 1;
    }

  if (optimize >= 2)
    {
      flag_optimize_sibling_calls = 1;
      flag_cse_follow_jumps = 1;
      flag_cse_skip_blocks = 1;
      flag_gcse = 1;
      flag_expensive_optimizations = 1;
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
      flag_rerun_loop_opt = 1;
      flag_caller_saves = 1;
      flag_force_mem = 1;
      flag_peephole2 = 1;
#ifdef INSN_SCHEDULING
      flag_schedule_insns = 1;
      flag_schedule_insns_after_reload = 1;
#endif
      flag_regmove = 1;
      flag_strict_aliasing = 1;
      flag_delete_null_pointer_checks = 1;
      flag_reorder_blocks = 1;
      flag_reorder_functions = 1;
    }

  if (optimize >= 3)
    {
      flag_inline_functions = 1;
      flag_rename_registers = 1;
    }

  if (optimize < 2 || optimize_size)
    {
      align_loops = 1;
      align_jumps = 1;
      align_labels = 1;
      align_functions = 1;

      /* Don't reorder blocks when optimizing for size because extra
	 jump insns may be created; also barrier may create extra padding.

	 More correctly we should have a block reordering mode that tried
	 to minimize the combined size of all the jumps.  This would more
	 or less automatically remove extra jumps, but would also try to
	 use more short jumps instead of long jumps.  */
      flag_reorder_blocks = 0;
    }

  /* Initialize whether `char' is signed.  */
  flag_signed_char = DEFAULT_SIGNED_CHAR;
#ifdef DEFAULT_SHORT_ENUMS
  /* Initialize how much space enums occupy, by default.  */
  flag_short_enums = DEFAULT_SHORT_ENUMS;
#endif

  /* Initialize target_flags before OPTIMIZATION_OPTIONS so the latter can
     modify it.  */
  target_flags = 0;
  set_target_switch ("");

  /* Unwind tables are always present in an ABI-conformant IA-64
     object file, so the default should be ON.  */
#ifdef IA64_UNWIND_INFO
  flag_unwind_tables = IA64_UNWIND_INFO;
#endif

#ifdef OPTIMIZATION_OPTIONS
  /* Allow default optimizations to be specified on a per-machine basis.  */
  OPTIMIZATION_OPTIONS (optimize, optimize_size);
#endif

  /* Perform normal command line switch decoding.  */
  for (i = 1; i < argc;)
    {
      int lang_processed;
      int indep_processed;

      /* Give the language a chance to decode the option for itself.  */
      lang_processed = (*lang_hooks.decode_option) (argc - i, argv + i);

      if (lang_processed >= 0)
	/* Now see if the option also has a language independent meaning.
	   Some options are both language specific and language independent,
	   eg --help.  */
	indep_processed = independent_decode_option (argc - i, argv + i);
      else
	{
	  lang_processed = -lang_processed;
	  indep_processed = 0;
	}

      if (lang_processed || indep_processed)
	i += MAX (lang_processed, indep_processed);
      else
	{
	  const char *option = NULL;
	  const char *lang = NULL;
	  unsigned int j;

	  /* It is possible that the command line switch is not valid for the
	     current language, but it is valid for another language.  In order
	     to be compatible with previous versions of the compiler (which
	     did not issue an error message in this case) we check for this
	     possibility here.  If we do find a match, then if extra_warnings
	     is set we generate a warning message, otherwise we will just
	     ignore the option.  */
	  for (j = 0; j < ARRAY_SIZE (documented_lang_options); j++)
	    {
	      option = documented_lang_options[j].option;

	      if (option == NULL)
		lang = documented_lang_options[j].description;
	      else if (! strncmp (argv[i], option, strlen (option)))
		break;
	    }

	  if (j != ARRAY_SIZE (documented_lang_options))
	    {
	      if (extra_warnings)
		{
		  warning ("ignoring command line option '%s'", argv[i]);
		  if (lang)
		    warning
		      ("(it is valid for %s but not the selected language)",
		       lang);
		}
	    }
	  else if (argv[i][0] == '-' && argv[i][1] == 'g')
	    warning ("`%s': unknown or unsupported -g option", &argv[i][2]);
	  else
	    error ("unrecognized option `%s'", argv[i]);

	  i++;
	}
    }

  if (flag_no_inline == 2)
    flag_no_inline = 0;
  else
    flag_really_no_inline = flag_no_inline;

  /* Set flag_no_inline before the post_options () hook.  The C front
     ends use it to determine tree inlining defaults.  FIXME: such
     code should be lang-independent when all front ends use tree
     inlining, in which case it, and this condition, should be moved
     to the top of process_options() instead.  */
  if (optimize == 0)
    {
      /* Inlining does not work if not optimizing,
	 so force it not to be done.  */
      flag_no_inline = 1;
      warn_inline = 0;

      /* The c_decode_option function and decode_option hook set
	 this to `2' if -Wall is used, so we can avoid giving out
	 lots of errors for people who don't realize what -Wall does.  */
      if (warn_uninitialized == 1)
	warning ("-Wuninitialized is not supported without -O");
    }

  if (flag_really_no_inline == 2)
    flag_really_no_inline = flag_no_inline;
}

/* Process the options that have been parsed.  */
static void
process_options ()
{
#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

  /* Set up the align_*_log variables, defaulting them to 1 if they
     were still unset.  */
  if (align_loops <= 0) align_loops = 1;
  if (align_loops_max_skip > align_loops || !align_loops)
    align_loops_max_skip = align_loops - 1;
  align_loops_log = floor_log2 (align_loops * 2 - 1);
  if (align_jumps <= 0) align_jumps = 1;
  if (align_jumps_max_skip > align_jumps || !align_jumps)
    align_jumps_max_skip = align_jumps - 1;
  align_jumps_log = floor_log2 (align_jumps * 2 - 1);
  if (align_labels <= 0) align_labels = 1;
  align_labels_log = floor_log2 (align_labels * 2 - 1);
  if (align_labels_max_skip > align_labels || !align_labels)
    align_labels_max_skip = align_labels - 1;
  if (align_functions <= 0) align_functions = 1;
  align_functions_log = floor_log2 (align_functions * 2 - 1);

  /* Unrolling all loops implies that standard loop unrolling must also
     be done.  */
  if (flag_unroll_all_loops)
    flag_unroll_loops = 1;
  /* Loop unrolling requires that strength_reduction be on also.  Silently
     turn on strength reduction here if it isn't already on.  Also, the loop
     unrolling code assumes that cse will be run after loop, so that must
     be turned on also.  */
  if (flag_unroll_loops)
    {
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
    }

  if (flag_non_call_exceptions)
    flag_asynchronous_unwind_tables = 1;
  if (flag_asynchronous_unwind_tables)
    flag_unwind_tables = 1;

  /* Warn about options that are not supported on this machine.  */
#ifndef INSN_SCHEDULING
  if (flag_schedule_insns || flag_schedule_insns_after_reload)
    warning ("instruction scheduling not supported on this target machine");
#endif
#ifndef DELAY_SLOTS
  if (flag_delayed_branch)
    warning ("this target machine does not have delayed branches");
#endif

  user_label_prefix = USER_LABEL_PREFIX;
  if (flag_leading_underscore != -1)
    {
      /* If the default prefix is more complicated than "" or "_",
	 issue a warning and ignore this option.  */
      if (user_label_prefix[0] == 0 ||
	  (user_label_prefix[0] == '_' && user_label_prefix[1] == 0))
	{
	  user_label_prefix = flag_leading_underscore ? "_" : "";
	}
      else
	warning ("-f%sleading-underscore not supported on this target machine",
		 flag_leading_underscore ? "" : "no-");
    }

  /* If we are in verbose mode, write out the version and maybe all the
     option flags in use.  */
  if (version_flag)
    {
      print_version (stderr, "");
      if (! quiet_flag)
	print_switch_values (stderr, 0, MAX_LINE, "", " ", "\n");
    }

  if (! quiet_flag || flag_detailed_statistics)
    time_report = 1;

  if (flag_syntax_only)
    {
      write_symbols = NO_DEBUG;
      profile_flag = 0;
    }

  /* Now we know write_symbols, set up the debug hooks based on it.
     By default we do nothing for debug output.  */
#if defined(DBX_DEBUGGING_INFO)
  if (write_symbols == DBX_DEBUG)
    debug_hooks = &dbx_debug_hooks;
#endif
#if defined(XCOFF_DEBUGGING_INFO)
  if (write_symbols == XCOFF_DEBUG)
    debug_hooks = &xcoff_debug_hooks;
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    debug_hooks = &sdb_debug_hooks;
#endif
#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    debug_hooks = &dwarf_debug_hooks;
#endif
#ifdef DWARF2_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG)
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  if (write_symbols == VMS_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    debug_hooks = &vmsdbg_debug_hooks;
#endif

  /* If auxiliary info generation is desired, open the output file.
     This goes in the same directory as the source file--unlike
     all the other output files.  */
  if (flag_gen_aux_info)
    {
      aux_info_file = fopen (aux_info_file_name, "w");
      if (aux_info_file == 0)
	fatal_io_error ("can't open %s", aux_info_file_name);
    }

  if (! targetm.have_named_sections)
    {
      if (flag_function_sections)
	{
	  warning ("-ffunction-sections not supported for this target");
	  flag_function_sections = 0;
	}
      if (flag_data_sections)
	{
	  warning ("-fdata-sections not supported for this target");
	  flag_data_sections = 0;
	}
    }

  if (flag_function_sections && profile_flag)
    {
      warning ("-ffunction-sections disabled; it makes profiling impossible");
      flag_function_sections = 0;
    }

#ifndef HAVE_prefetch
  if (flag_prefetch_loop_arrays)
    {
      warning ("-fprefetch-loop-arrays not supported for this target");
      flag_prefetch_loop_arrays = 0;
    }
#else
  if (flag_prefetch_loop_arrays && !HAVE_prefetch)
    {
      warning ("-fprefetch-loop-arrays not supported for this target (try -march switches)");
      flag_prefetch_loop_arrays = 0;
    }
#endif

  /* This combination of options isn't handled for i386 targets and doesn't
     make much sense anyway, so don't allow it.  */
  if (flag_prefetch_loop_arrays && optimize_size)
    {
      warning ("-fprefetch-loop-arrays is not supported with -Os");
      flag_prefetch_loop_arrays = 0;
    }

#ifndef OBJECT_FORMAT_ELF
  if (flag_function_sections && write_symbols != NO_DEBUG)
    warning ("-ffunction-sections may affect debugging on some targets");
#endif

    /* The presence of IEEE signaling NaNs, implies all math can trap.  */
    if (flag_signaling_nans)
      flag_trapping_math = 1;
}

/* Initialize the compiler back end.  */
static void
backend_init ()
{
  /* init_emit_once uses reg_raw_mode and therefore must be called
     after init_regs which initialized reg_raw_mode.  */
  init_regs ();
  init_emit_once (debug_info_level == DINFO_LEVEL_NORMAL
		  || debug_info_level == DINFO_LEVEL_VERBOSE
#ifdef VMS_DEBUGGING_INFO
		    /* Enable line number info for traceback */
		    || debug_info_level > DINFO_LEVEL_NONE
#endif
		    || flag_test_coverage
		    || warn_notreached);
  init_fake_stack_mems ();
  init_alias_once ();
  init_loop ();
  init_reload ();
  init_function_once ();
  init_varasm_once ();

  /* The following initialization functions need to generate rtl, so
     provide a dummy function context for them.  */
  init_dummy_function_start ();
  init_expmed ();
  if (flag_caller_saves)
    init_caller_save ();
  expand_dummy_function_end ();
}

/* Language-dependent initialization.  Returns nonzero on success.  */
static int
lang_dependent_init (name)
     const char *name;
{
  if (dump_base_name == 0)
    dump_base_name = name ? name : "gccdump";
  
  /* Front-end initialization.  This hook can assume that GC,
     identifier hashes etc. are set up, but debug initialization is
     not done yet.  This routine must return the original filename
     (e.g. foo.i -> foo.c) so can correctly initialize debug output.  */
  name = (*lang_hooks.init) (name);
  if (name == NULL)
    return 0;

  /* Is this duplication necessary?  */
  name = ggc_strdup (name);
  main_input_filename = input_filename = name;
  init_asm_output (name);

  /* These create various _DECL nodes, so need to be called after the
     front end is initialized.  */
  init_eh ();
  init_optabs ();

  /* The following initialization functions need to generate rtl, so
     provide a dummy function context for them.  */
  init_dummy_function_start ();
  init_expr_once ();
  expand_dummy_function_end ();

  /* Put an entry on the input file stack for the main input file.  */
  push_srcloc (input_filename, 0);

  /* If dbx symbol table desired, initialize writing it and output the
     predefined types.  */
  timevar_push (TV_SYMOUT);

#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_init ();
#endif

  /* Now we have the correct original filename, we can initialize
     debug output.  */
  (*debug_hooks->init) (name);

  timevar_pop (TV_SYMOUT);

  return 1;
}

/* Clean up: close opened files, etc.  */

static void
finalize ()
{
  /* Close the dump files.  */
  if (flag_gen_aux_info)
    {
      fclose (aux_info_file);
      if (errorcount)
	unlink (aux_info_file_name);
    }

  /* Close non-debugging input and output files.  Take special care to note
     whether fclose returns an error, since the pages might still be on the
     buffer chain while the file is open.  */

  if (asm_out_file)
    {
      if (ferror (asm_out_file) != 0)
	fatal_io_error ("error writing to %s", asm_file_name);
      if (fclose (asm_out_file) != 0)
	fatal_io_error ("error closing %s", asm_file_name);
    }

  /* Do whatever is necessary to finish printing the graphs.  */
  if (graph_dump_format != no_graph)
    {
      int i;

      for (i = 0; i < (int) DFI_MAX; ++i)
	if (dump_file[i].initialized && dump_file[i].graph_dump_p)
	  {
	    char seq[16];
	    char *suffix;

	    sprintf (seq, DUMPFILE_FORMAT, i);
	    suffix = concat (seq, dump_file[i].extension, NULL);
	    finish_graph_dump_file (dump_base_name, suffix);
	    free (suffix);
	  }
    }

  if (mem_report)
    {
      ggc_print_statistics ();
      stringpool_statistics ();
      dump_tree_statistics ();
    }

  /* Free up memory for the benefit of leak detectors.  */
  free_reg_info ();

  /* Language-specific end of compilation actions.  */
  (*lang_hooks.finish) ();
}

/* Initialize the compiler, and compile the input file.  */
static void
do_compile ()
{
  /* All command line options have been parsed; allow the front end to
     perform consistency checks, etc.  */
  bool no_backend = (*lang_hooks.post_options) ();

  /* The bulk of command line switch processing.  */
  process_options ();

  /* If an error has already occurred, give up.  */
  if (errorcount)
    return;

  if (aux_base_name)
    /*NOP*/;
  else if (filename)
    {
      char *name = xstrdup (lbasename (filename));
      
      aux_base_name = name;
      strip_off_ending (name, strlen (name));
    }
  else
    aux_base_name = "gccaux";

  /* We cannot start timing until after options are processed since that
     says if we run timers or not.  */
  init_timevar ();
  timevar_start (TV_TOTAL);

  /* Set up the back-end if requested.  */
  if (!no_backend)
    backend_init ();

  /* Language-dependent initialization.  Returns true on success.  */
  if (lang_dependent_init (filename))
    compile_file ();

  finalize ();

  /* Stop timing and print the times.  */
  timevar_stop (TV_TOTAL);
  timevar_print (stderr);
}

/* Entry point of cc1, cc1plus, jc1, f771, etc.
   Decode command args, then call compile_file.
   Exit code is FATAL_EXIT_CODE if can't open files or if there were
   any errors, or SUCCESS_EXIT_CODE if compilation succeeded.

   It is not safe to call this function more than once.  */

int
toplev_main (argc, argv)
     int argc;
     char **argv;
{
  /* Initialization of GCC's environment, and diagnostics.  */
  general_init (argv[0]);

  /* Parse the options and do minimal processing; basically just
     enough to default flags appropriately.  */
  parse_options_and_default_flags (argc, argv);

  /* Exit early if we can (e.g. -help).  */
  if (!exit_after_options)
    do_compile ();

  if (errorcount || sorrycount)
    return (FATAL_EXIT_CODE);

  return (SUCCESS_EXIT_CODE);
}
