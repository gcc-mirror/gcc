/* Top level of GCC compilers (cc1, cc1plus, etc.)
   Copyright (C) 1987, 1988, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "tm.h"
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
#include "params.h"
#include "reload.h"
#include "dwarf2asm.h"
#include "integrate.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "langhooks.h"
#include "cfglayout.h"
#include "cfgloop.h"
#include "hosthooks.h"
#include "cgraph.h"
#include "opts.h"
#include "coverage.h"
#include "value-prof.h"
#include "alloc-pool.h"

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

#ifndef HAVE_conditional_execution
#define HAVE_conditional_execution 0
#endif

/* Carry information from ASM_DECLARE_OBJECT_NAME
   to ASM_FINISH_DECLARE_OBJECT.  */

extern int size_directive_output;
extern tree last_assemble_variable_decl;

extern void reg_alloc (void);

static void general_init (const char *);
static void do_compile (void);
static void process_options (void);
static void backend_init (void);
static int lang_dependent_init (const char *);
static void init_asm_output (const char *);
static void finalize (void);

static void crash_signal (int) ATTRIBUTE_NORETURN;
static void setup_core_dumping (void);
static void compile_file (void);

static int print_single_switch (FILE *, int, int, const char *,
				const char *, const char *,
				const char *, const char *);
static void print_switch_values (FILE *, int, int, const char *,
				 const char *, const char *);

/* Rest of compilation helper functions.  */
static bool rest_of_handle_inlining (tree);
static void rest_of_handle_cse (tree, rtx);
static void rest_of_handle_cse2 (tree, rtx);
static void rest_of_handle_gcse (tree, rtx);
static void rest_of_handle_life (tree, rtx);
static void rest_of_handle_loop_optimize (tree, rtx);
static void rest_of_handle_loop2 (tree, rtx);
static void rest_of_handle_jump_bypass (tree, rtx);
static void rest_of_handle_sibling_calls (rtx);
static void rest_of_handle_null_pointer (tree, rtx);
static void rest_of_handle_addressof (tree, rtx);
static void rest_of_handle_cfg (tree, rtx);
static void rest_of_handle_branch_prob (tree, rtx);
static void rest_of_handle_value_profile_transformations (tree, rtx);
static void rest_of_handle_if_conversion (tree, rtx);
static void rest_of_handle_if_after_combine (tree, rtx);
static void rest_of_handle_tracer (tree, rtx);
static void rest_of_handle_combine (tree, rtx);
static void rest_of_handle_regmove (tree, rtx);
#ifdef INSN_SCHEDULING
static void rest_of_handle_sched (tree, rtx);
static void rest_of_handle_sched2 (tree, rtx);
#endif
static bool rest_of_handle_new_regalloc (tree, rtx);
static bool rest_of_handle_old_regalloc (tree, rtx);
static void rest_of_handle_regrename (tree, rtx);
static void rest_of_handle_reorder_blocks (tree, rtx);
#ifdef STACK_REGS
static void rest_of_handle_stack_regs (tree, rtx);
#endif
static void rest_of_handle_machine_reorg (tree, rtx);
#ifdef DELAY_SLOTS
static void rest_of_handle_delay_slots (tree, rtx);
#endif
static void rest_of_handle_final (tree, rtx);

/* Nonzero to dump debug info whilst parsing (-dy option).  */
static int set_yydebug;

/* True if we don't need a backend (e.g. preprocessing only).  */
static bool no_backend;

/* Length of line when printing switch values.  */
#define MAX_LINE 75

/* Name of program invoked, sans directories.  */

const char *progname;

/* Copy of argument vector to toplev_main.  */
static const char **save_argv;

/* Name of top-level original source file (what was input to cpp).
   This comes from the #-command at the beginning of the actual input.
   If there isn't any there, then this is the cc1 input file name.  */

const char *main_input_filename;

/* Current position in real source file.  */

location_t input_location;

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

const struct gcc_debug_hooks *debug_hooks;

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
  DFI_cgraph,
  DFI_rtl,
  DFI_sibling,
  DFI_eh,
  DFI_jump,
  DFI_null,
  DFI_cse,
  DFI_addressof,
  DFI_gcse,
  DFI_loop,
  DFI_bypass,
  DFI_cfg,
  DFI_bp,
  DFI_vpt,
  DFI_ce1,
  DFI_tracer,
  DFI_loop2,
  DFI_web,
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
  DFI_ce3,
  DFI_rnreg,
  DFI_bbro,
  DFI_branch_target_load,
  DFI_sched2,
  DFI_stack,
  DFI_mach,
  DFI_dbr,
  DFI_MAX
};

/* Describes all the dump files.  Should be kept in order of the
   pass and in sync with dump_file_index above.

   Remaining -d letters:

	"   e        m   q         "
	"         JK   O Q     WXY "
*/

static struct dump_file_info dump_file[DFI_MAX] =
{
  { "cgraph",	'U', 0, 0, 0 },
  { "rtl",	'r', 0, 0, 0 },
  { "sibling",  'i', 0, 0, 0 },
  { "eh",	'h', 0, 0, 0 },
  { "jump",	'j', 0, 0, 0 },
  { "null",	'u', 0, 0, 0 },
  { "cse",	's', 0, 0, 0 },
  { "addressof", 'F', 0, 0, 0 },
  { "gcse",	'G', 1, 0, 0 },
  { "loop",	'L', 1, 0, 0 },
  { "bypass",   'G', 1, 0, 0 }, /* Yes, duplicate enable switch.  */
  { "cfg",	'f', 1, 0, 0 },
  { "bp",	'b', 1, 0, 0 },
  { "vpt",	'V', 1, 0, 0 },
  { "ce1",	'C', 1, 0, 0 },
  { "tracer",	'T', 1, 0, 0 },
  { "loop2",	'L', 1, 0, 0 },
  { "web",      'Z', 0, 0, 0 },
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
  { "ce3",	'E', 1, 0, 0 },
  { "rnreg",	'n', 1, 0, 0 },
  { "bbro",	'B', 1, 0, 0 },
  { "btl",	'd', 1, 0, 0 }, /* Yes, duplicate enable switch.  */
  { "sched2",	'R', 1, 0, 0 },
  { "stack",	'k', 1, 0, 0 },
  { "mach",	'M', 1, 0, 0 },
  { "dbr",	'd', 0, 0, 0 },
};

static int open_dump_file (enum dump_file_index, tree);
static void close_dump_file (enum dump_file_index,
			     void (*) (FILE *, rtx), rtx);

/* Other flags saying which kinds of debugging dump have been requested.  */

int rtl_dump_and_exit;
int flag_print_asm_name;
enum graph_dump_types graph_dump_format;

/* Name for output file of assembly code, specified with -o.  */

const char *asm_file_name;

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

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to the FUNC_BEGIN label of the current function, or NULL_TREE
   if none.  */
tree current_function_func_begin_label;

/* Nonzero if doing dwarf2 duplicate elimination.  */

int flag_eliminate_dwarf2_dups = 0;

/* Nonzero if doing unused type elimination.  */

int flag_eliminate_unused_debug_types = 1;

/* Nonzero means emit debugging information only for symbols which are used.  */
int flag_debug_only_used_symbols = 0;

/* Nonzero if generating code to do profiling.  */

int profile_flag = 0;

/* Nonzero if generating code to profile program flow graph arcs.  */

int profile_arc_flag = 0;

/* Nonzero if value histograms should be measured.  */

int flag_profile_values = 0;

/* Nonzero if value histograms should be used to optimize code.  */
int flag_value_profile_transformations = 0;

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

/* Nonzero means to collect statistics which might be expensive
   and to print them when we are done.  */
int flag_detailed_statistics = 0;

/* A random sequence of characters, unless overridden by user.  */
const char *flag_random_seed;

/* A local time stamp derived from the time of compilation. It will be
   zero if the system cannot provide a time.  It will be -1u, if the
   user has specified a particular random seed.  */
unsigned local_tick;

/* -f flags.  */

/* Nonzero means `char' should be signed.  */

int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */

int flag_short_enums;

/* Nonzero for -fcaller-saves: allocate values in regs that need to
   be saved across function calls, if that produces overall better code.
   Optional now, so people can test it.  */

int flag_caller_saves = 0;

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

int flag_old_unroll_loops;

/* Nonzero enables loop unrolling in unroll.c.  All loops are unrolled.
   This is generally not a win.  */

int flag_old_unroll_all_loops;

/* Enables unrolling of simple loops in loop-unroll.c.  */
int flag_unroll_loops;

/* Enables unrolling of all loops in loop-unroll.c.  */
int flag_unroll_all_loops;

/* Nonzero enables loop peeling.  */
int flag_peel_loops;

/* Nonzero enables loop unswitching.  */
int flag_unswitch_loops;

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

/* Nonzero means disable transformations that assume default floating
   point rounding behavior.  */

int flag_rounding_math = 0;

/* Nonzero means disable transformations observable by signaling NaNs.
   This option implies that any operation on an IEEE signaling NaN can
   generate a (user-visible) trap.  */

int flag_signaling_nans = 0;

/* 0 means straightforward implementation of complex divide acceptable.
   1 means wide ranges of inputs must work for complex divide.
   2 means C99-like requirements for complex divide (not yet implemented).  */

int flag_complex_divide_method = 0;

/* Nonzero means just do syntax checking; don't output anything.  */

int flag_syntax_only = 0;

/* Nonzero means performs web construction pass.  */

int flag_web;

/* Nonzero means perform loop optimizer.  */

int flag_loop_optimize;

/* Nonzero means perform crossjumping.  */

int flag_crossjumping;

/* Nonzero means perform if conversion.  */

int flag_if_conversion;

/* Nonzero means perform if conversion after reload.  */

int flag_if_conversion2;

/* Nonzero means to use global dataflow analysis to eliminate
   useless null pointer tests.  */

int flag_delete_null_pointer_checks;

/* Nonzero means perform global CSE.  */

int flag_gcse = 0;

/* Nonzero means to do the enhanced load motion during gcse, which trys
   to hoist loads by not killing them when a store to the same location
   is seen.  */

int flag_gcse_lm = 1;

/* Nonzero means to perform store motion after gcse, which will try to
   move stores closer to the exit block.  Its not very effective without
   flag_gcse_lm.  */

int flag_gcse_sm = 1;

/* Nonzero if we want to perfrom redundant load after store elimination
   in gcse.  */

int flag_gcse_las = 1;

/* Perform target register optimization before prologue / epilogue
   threading.  */

int flag_branch_target_load_optimize = 0;

/* Perform target register optimization after prologue / epilogue
   threading and jump2.  */

int flag_branch_target_load_optimize2 = 0;

/* Nonzero means to rerun cse after loop optimization.  This increases
   compilation time about 20% and picks up a few more common expressions.  */

int flag_rerun_cse_after_loop;

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

const char *aux_info_file_name;

/* Nonzero means make the text shared if supported.  */

int flag_shared_data;

/* Nonzero means schedule into delayed branch slots if supported.  */

int flag_delayed_branch;

/* Nonzero if we are compiling pure (sharable) code.
   Value is 1 if we are doing "small" pic; value is 2 if we're doing
   "large" pic.  */

int flag_pic;

/* Nonzero if we are compiling position independent code for executable.
   The value is 1 if we are doing "small" pic; value is 2 if we're doing
   "large" pic.  */

int flag_pie;

/* Nonzero if we are compiling code for a shared library, zero for
   executable.  */

int flag_shlib;

/* Set to the default thread-local storage (tls) model to use.  */

enum tls_model flag_tls_default = TLS_MODEL_GLOBAL_DYNAMIC;

/* Nonzero means generate extra code for exception handling and enable
   exception handling.  */

int flag_exceptions;

/* Nonzero means generate frame unwind info table when supported.  */

int flag_unwind_tables = 0;

/* Nonzero means generate frame unwind info table exact at each insn
   boundary.  */

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

/* When flag_schedule_insns_after_reload is set, use EBB scheduler.  */
int flag_sched2_use_superblocks = 0;

/* When flag_schedule_insns_after_reload is set, construct traces and EBB
   scheduler.  */
int flag_sched2_use_traces = 0;

/* The following flags have effect only for scheduling before register
   allocation:

   flag_schedule_interblock means schedule insns across basic blocks.
   flag_schedule_speculative means allow speculative motion of non-load insns.
   flag_schedule_speculative_load means allow speculative motion of some
   load insns.
   flag_schedule_speculative_load_dangerous allows speculative motion of more
   load insns.  */

int flag_schedule_interblock = 1;
int flag_schedule_speculative = 1;
int flag_schedule_speculative_load = 0;
int flag_schedule_speculative_load_dangerous = 0;

/* The following flags have an effect during scheduling after register
   allocation:

   flag_sched_stalled_insns means that insns can be moved prematurely from the queue
   of stalled insns into the ready list.

   flag_sched_stalled_insns_dep controls how many insn groups will be examined
   for a dependency on a stalled insn that is candidate for premature removal
   from the queue of stalled insns into the ready list (has an effect only if
   the flag 'sched_stalled_insns' is set).  */

int flag_sched_stalled_insns = 0;
int flag_sched_stalled_insns_dep = 1;

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

/* Nonzero means put zero initialized data in the bss section.  */
int flag_zero_initialized_in_bss = 1;

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

/* Nonzero if we perform whole unit at a time compilation.  */

int flag_unit_at_a_time = 0;

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

typedef struct
{
  const char *const string;
  int *const variable;
  const int on_value;
}
lang_independent_options;

/* Nonzero if signed arithmetic overflow should trap.  */
int flag_trapv = 0;

/* Nonzero if signed arithmetic overflow should wrap around.  */
int flag_wrapv = 0;

/* Nonzero if subexpressions must be evaluated from left-to-right.  */
int flag_evaluation_order = 0;

/* Add or remove a leading underscore from user symbols.  */
int flag_leading_underscore = -1;

/*  The version of the C++ ABI in use.  The following values are
    allowed:

    0: The version of the ABI believed most conformant with the
       C++ ABI specification.  This ABI may change as bugs are
       discovered and fixed.  Therefore, 0 will not necessarily
       indicate the same ABI in different versions of G++.

    1: The version of the ABI first used in G++ 3.2.

    2: The version of the ABI first used in G++ 3.4.

    Additional positive integers will be assigned as new versions of
    the ABI become the default version of the ABI.  */

int flag_abi_version = 2;

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
  {"eliminate-dwarf2-dups", &flag_eliminate_dwarf2_dups, 1 },
  {"eliminate-unused-debug-symbols", &flag_debug_only_used_symbols, 1 },
  {"eliminate-unused-debug-types", &flag_eliminate_unused_debug_types, 1 },
  {"float-store", &flag_float_store, 1 },
  {"defer-pop", &flag_defer_pop, 1 },
  {"omit-frame-pointer", &flag_omit_frame_pointer, 1 },
  {"optimize-sibling-calls", &flag_optimize_sibling_calls, 1 },
  {"tracer", &flag_tracer, 1 },
  {"unit-at-a-time", &flag_unit_at_a_time, 1 },
  {"cse-follow-jumps", &flag_cse_follow_jumps, 1 },
  {"cse-skip-blocks", &flag_cse_skip_blocks, 1 },
  {"expensive-optimizations", &flag_expensive_optimizations, 1 },
  {"thread-jumps", &flag_thread_jumps, 1 },
  {"strength-reduce", &flag_strength_reduce, 1 },
  {"unroll-loops", &flag_unroll_loops, 1 },
  {"unroll-all-loops", &flag_unroll_all_loops, 1 },
  {"old-unroll-loops", &flag_old_unroll_loops, 1 },
  {"old-unroll-all-loops", &flag_old_unroll_all_loops, 1 },
  {"peel-loops", &flag_peel_loops, 1 },
  {"unswitch-loops", &flag_unswitch_loops, 1 },
  {"prefetch-loop-arrays", &flag_prefetch_loop_arrays, 1 },
  {"move-all-movables", &flag_move_all_movables, 1 },
  {"reduce-all-givs", &flag_reduce_all_givs, 1 },
  {"writable-strings", &flag_writable_strings, 1 },
  {"peephole", &flag_no_peephole, 0 },
  {"force-mem", &flag_force_mem, 1 },
  {"force-addr", &flag_force_addr, 1 },
  {"function-cse", &flag_no_function_cse, 0 },
  {"inline-functions", &flag_inline_functions, 1 },
  {"keep-inline-functions", &flag_keep_inline_functions, 1 },
  {"inline", &flag_no_inline, 0 },
  {"keep-static-consts", &flag_keep_static_consts, 1 },
  {"syntax-only", &flag_syntax_only, 1 },
  {"shared-data", &flag_shared_data, 1 },
  {"caller-saves", &flag_caller_saves, 1 },
  {"pcc-struct-return", &flag_pcc_struct_return, 1 },
  {"reg-struct-return", &flag_pcc_struct_return, 0 },
  {"delayed-branch", &flag_delayed_branch, 1 },
  {"web", &flag_web, 1},
  {"gcse", &flag_gcse, 1 },
  {"gcse-lm", &flag_gcse_lm, 1 },
  {"gcse-sm", &flag_gcse_sm, 1 },
  {"gcse-las", &flag_gcse_las, 1 },
  {"branch-target-load-optimize", &flag_branch_target_load_optimize, 1 },
  {"branch-target-load-optimize2", &flag_branch_target_load_optimize2, 1 },
  {"loop-optimize", &flag_loop_optimize, 1 },
  {"crossjumping", &flag_crossjumping, 1 },
  {"if-conversion", &flag_if_conversion, 1 },
  {"if-conversion2", &flag_if_conversion2, 1 },
  {"rerun-cse-after-loop", &flag_rerun_cse_after_loop, 1 },
  {"rerun-loop-opt", &flag_rerun_loop_opt, 1 },
  {"delete-null-pointer-checks", &flag_delete_null_pointer_checks, 1 },
  {"schedule-insns", &flag_schedule_insns, 1 },
  {"schedule-insns2", &flag_schedule_insns_after_reload, 1 },
  {"sched-interblock",&flag_schedule_interblock, 1 },
  {"sched-spec",&flag_schedule_speculative, 1 },
  {"sched-spec-load",&flag_schedule_speculative_load, 1 },
  {"sched-spec-load-dangerous",&flag_schedule_speculative_load_dangerous, 1 },
  {"sched-stalled-insns", &flag_sched_stalled_insns, 0 },
  {"sched-stalled-insns-dep", &flag_sched_stalled_insns_dep, 1 },
  {"sched2-use-superblocks", &flag_sched2_use_superblocks, 1 },
  {"sched2-use-traces", &flag_sched2_use_traces, 1 },
  {"branch-count-reg",&flag_branch_on_count_reg, 1 },
  {"pic", &flag_pic, 1 },
  {"PIC", &flag_pic, 2 },
  {"pie", &flag_pie, 1 },
  {"PIE", &flag_pie, 2 },
  {"exceptions", &flag_exceptions, 1 },
  {"unwind-tables", &flag_unwind_tables, 1 },
  {"asynchronous-unwind-tables", &flag_asynchronous_unwind_tables, 1 },
  {"non-call-exceptions", &flag_non_call_exceptions, 1 },
  {"profile-arcs", &profile_arc_flag, 1 },
  {"profile-values", &flag_profile_values, 1 },
  {"vpt", &flag_value_profile_transformations, 1 },
  {"test-coverage", &flag_test_coverage, 1 },
  {"branch-probabilities", &flag_branch_probabilities, 1 },
  {"profile", &profile_flag, 1 },
  {"reorder-blocks", &flag_reorder_blocks, 1 },
  {"reorder-functions", &flag_reorder_functions, 1 },
  {"rename-registers", &flag_rename_registers, 1 },
  {"cprop-registers", &flag_cprop_registers, 1 },
  {"common", &flag_no_common, 0 },
  {"inhibit-size-directive", &flag_inhibit_size_directive, 1 },
  {"function-sections", &flag_function_sections, 1 },
  {"data-sections", &flag_data_sections, 1 },
  {"verbose-asm", &flag_verbose_asm, 1 },
  {"regmove", &flag_regmove, 1 },
  {"optimize-register-move", &flag_regmove, 1 },
  {"pack-struct", &flag_pack_struct, 1 },
  {"stack-check", &flag_stack_check, 1 },
  {"argument-alias", &flag_argument_noalias, 0 },
  {"argument-noalias", &flag_argument_noalias, 1 },
  {"argument-noalias-global", &flag_argument_noalias, 2 },
  {"strict-aliasing", &flag_strict_aliasing, 1 },
  {"align-loops", &align_loops, 0 },
  {"align-jumps", &align_jumps, 0 },
  {"align-labels", &align_labels, 0 },
  {"align-functions", &align_functions, 0 },
  {"merge-constants", &flag_merge_constants, 1 },
  {"merge-all-constants", &flag_merge_constants, 2 },
  {"dump-unnumbered", &flag_dump_unnumbered, 1 },
  {"instrument-functions", &flag_instrument_function_entry_exit, 1 },
  {"zero-initialized-in-bss", &flag_zero_initialized_in_bss, 1 },
  {"leading-underscore", &flag_leading_underscore, 1 },
  {"ident", &flag_no_ident, 0 },
  { "peephole2", &flag_peephole2, 1 },
  {"finite-math-only", &flag_finite_math_only, 1 },
  { "guess-branch-probability", &flag_guess_branch_prob, 1 },
  {"math-errno", &flag_errno_math, 1 },
  {"trapping-math", &flag_trapping_math, 1 },
  {"rounding-math", &flag_rounding_math, 1 },
  {"unsafe-math-optimizations", &flag_unsafe_math_optimizations, 1 },
  {"signaling-nans", &flag_signaling_nans, 1 },
  {"bounds-check", &flag_bounds_check, 1 },
  {"single-precision-constant", &flag_single_precision_constant, 1 },
  {"time-report", &time_report, 1 },
  {"mem-report", &mem_report, 1 },
  { "trapv", &flag_trapv, 1 },
  { "wrapv", &flag_wrapv, 1 },
  { "new-ra", &flag_new_regalloc, 1 }
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
  const char *const value;
}
target_options[] = TARGET_OPTIONS;
#endif

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

int warn_return_type;

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
FILE *rtl_dump_file = NULL;
FILE *cgraph_dump_file = NULL;

/* The current working directory of a translation.  It's generally the
   directory from which compilation was initiated, but a preprocessed
   file may specify the original directory in which it was
   created.  */

static const char *src_pwd;

/* Initialize src_pwd with the given string, and return true.  If it
   was already initialized, return false.  As a special case, it may
   be called with a NULL argument to test whether src_pwd has NOT been
   initialized yet.  */

bool
set_src_pwd (const char *pwd)
{
  if (src_pwd)
    return false;

  src_pwd = xstrdup (pwd);
  return true;
}

/* Return the directory from which the translation unit was initiated,
   in case set_src_pwd() was not called before to assign it a
   different value.  */

const char *
get_src_pwd (void)
{
  if (! src_pwd)
    src_pwd = getpwd ();

   return src_pwd;
}

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */
void
announce_function (tree decl)
{
  if (!quiet_flag)
    {
      if (rtl_dump_and_exit)
	verbatim ("%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	verbatim (" %s", (*lang_hooks.decl_printable_name) (decl, 2));
      fflush (stderr);
      pp_needs_newline (global_dc->printer) = true;
      diagnostic_set_last_function (global_dc);
    }
}

/* Set up a default flag_random_seed and local_tick, unless the user
   already specified one.  */

static void
randomize (void)
{
  if (!flag_random_seed)
    {
      unsigned HOST_WIDE_INT value;
      static char random_seed[HOST_BITS_PER_WIDE_INT / 4 + 3];

      /* Get some more or less random data.  */
#ifdef HAVE_GETTIMEOFDAY
      {
 	struct timeval tv;

 	gettimeofday (&tv, NULL);
	local_tick = tv.tv_sec * 1000 + tv.tv_usec / 1000;
      }
#else
      {
	time_t now = time (NULL);

	if (now != (time_t)-1)
	  local_tick = (unsigned) now;
      }
#endif
      value = local_tick ^ getpid ();

      sprintf (random_seed, HOST_WIDE_INT_PRINT_HEX, value);
      flag_random_seed = random_seed;
    }
  else if (!local_tick)
    local_tick = -1;
}


/* Decode the string P as an integral parameter.
   If the string is indeed an integer return its numeric value else
   issue an Invalid Option error for the option PNAME and return DEFVAL.
   If PNAME is zero just return DEFVAL, do not call error.  */

int
read_integral_parameter (const char *p, const char *pname, const int  defval)
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
	error ("invalid option argument `%s'", pname);
      return defval;
    }

  return atoi (p);
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.

   This should be used via the `exact_log2' macro.  */

int
exact_log2_wide (unsigned HOST_WIDE_INT x)
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
floor_log2_wide (unsigned HOST_WIDE_INT x)
{
  int log = -1;
  while (x != 0)
    log++,
    x >>= 1;
  return log;
}

/* Handler for fatal signals, such as SIGSEGV.  These are transformed
   into ICE messages, which is much more user friendly.  In case the
   error printer crashes, reset the signal to prevent infinite recursion.  */

static void
crash_signal (int signo)
{
  signal (signo, SIG_DFL);
  internal_error ("%s", strsignal (signo));
}

/* Arrange to dump core on error.  (The regular error message is still
   printed first, except in the case of abort().)  */

static void
setup_core_dumping (void)
{
#ifdef SIGABRT
  signal (SIGABRT, SIG_DFL);
#endif
#if defined(HAVE_SETRLIMIT)
  {
    struct rlimit rlim;
    if (getrlimit (RLIMIT_CORE, &rlim) != 0)
      fatal_error ("getting core file size maximum limit: %m");
    rlim.rlim_cur = rlim.rlim_max;
    if (setrlimit (RLIMIT_CORE, &rlim) != 0)
      fatal_error ("setting core file size limit to maximum: %m");
  }
#endif
  diagnostic_abort_on_error (global_dc);
}


/* Strip off a legitimate source ending from the input string NAME of
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by
   up to five characters.  (Java uses ".class".)  */

void
strip_off_ending (char *name, int len)
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
output_quoted_string (FILE *asm_file, const char *string)
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

/* Output a file name in the form wanted by System V.  */

void
output_file_directive (FILE *asm_file, const char *input_name)
{
  int len;
  const char *na;

  if (input_name == NULL)
    input_name = "<stdin>";

  len = strlen (input_name);
  na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (IS_DIR_SEPARATOR (na[-1]))
	break;
      na--;
    }

#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
#else
  fprintf (asm_file, "\t.file\t");
  output_quoted_string (asm_file, na);
  fputc ('\n', asm_file);
#endif
}

/* Routine to open a dump file.  Return true if the dump file is enabled.  */

static int
open_dump_file (enum dump_file_index index, tree decl)
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
    fatal_error ("can't open %s: %m", dump_name);

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
close_dump_file (enum dump_file_index index,
		 void (*func) (FILE *, rtx),
		 rtx insns)
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
wrapup_global_declarations (tree *vec, int len)
{
  tree decl;
  int i;
  int reconsider;
  int output_something = 0;

  for (i = 0; i < len; i++)
    {
      decl = vec[i];

      /* We're not deferring this any longer.  Assignment is
	 conditional to avoid needlessly dirtying PCH pages.  */
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

	      if (flag_unit_at_a_time
		  && cgraph_varpool_node (decl)->finalized)
		needed = 0;
	      else if ((flag_unit_at_a_time && !cgraph_global_info_ready)
		       && (TREE_USED (decl)
			   || TREE_USED (DECL_ASSEMBLER_NAME (decl))))
		/* needed */;
	      else if (TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
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
	      && DECL_SAVED_INSNS (decl)->saved_for_inline
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
check_global_declarations (tree *vec, int len)
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
	    pedwarn ("%J'%F' used but never defined", decl, decl);
	  else
	    warning ("%J'%F' declared `static' but never defined", decl, decl);
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
	  /* A volatile variable might be used in some non-obvious way.  */
	  && ! TREE_THIS_VOLATILE (decl)
	  /* Global register variables must be declared to reserve them.  */
	  && ! (TREE_CODE (decl) == VAR_DECL && DECL_REGISTER (decl))
	  /* Otherwise, ask the language.  */
	  && (*lang_hooks.decls.warn_unused_global) (decl))
	warning ("%J'%D' defined but not used", decl, decl);

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

/* Warn about a use of an identifier which was marked deprecated.  */
void
warn_deprecated_use (tree node)
{
  if (node == 0 || !warn_deprecated_decl)
    return;

  if (DECL_P (node))
    warning ("`%s' is deprecated (declared at %s:%d)",
	     IDENTIFIER_POINTER (DECL_NAME (node)),
	     DECL_SOURCE_FILE (node), DECL_SOURCE_LINE (node));
  else if (TYPE_P (node))
    {
      const char *what = NULL;
      tree decl = TYPE_STUB_DECL (node);

      if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	what = IDENTIFIER_POINTER (TYPE_NAME (node));
      else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
	       && DECL_NAME (TYPE_NAME (node)))
	what = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node)));

      if (what)
	{
	  if (decl)
	    warning ("`%s' is deprecated (declared at %s:%d)", what,
		     DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
	  else
	    warning ("`%s' is deprecated", what);
	}
      else if (decl)
	warning ("type is deprecated (declared at %s:%d)",
		 DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
      else
	warning ("type is deprecated");
    }
}

/* Save the current INPUT_LOCATION on the top entry in the
   INPUT_FILE_STACK.  Push a new entry for FILE and LINE, and set the
   INPUT_LOCATION accordingly.  */

void
push_srcloc (const char *file, int line)
{
  struct file_stack *fs;

  fs = xmalloc (sizeof (struct file_stack));
  fs->location = input_location;
  fs->next = input_file_stack;
  input_filename = file;
  input_line = line;
  input_file_stack = fs;
  input_file_stack_tick++;
}

/* Pop the top entry off the stack of presently open source files.
   Restore the INPUT_LOCATION from the new topmost entry on the
   stack.  */

void
pop_srcloc (void)
{
  struct file_stack *fs;

  fs = input_file_stack;
  input_location = fs->location;
  input_file_stack = fs->next;
  free (fs);
  input_file_stack_tick++;
}

/* Compile an entire translation unit.  Write a file of assembly
   output and various debugging dumps.  */

static void
compile_file (void)
{
  /* Initialize yet another pass.  */

  init_final (main_input_filename);
  coverage_init (aux_base_name);

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

  cgraph_varpool_assemble_pending_decls ();

  /* This must occur after the loop to output deferred functions.
     Else the coverage initializer would not be emitted if all the
     functions in this compilation unit were deferred.  */
  coverage_finish ();

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

  /* Flush any pending equate directives.  */
  process_pending_assemble_output_defs ();

  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      timevar_push (TV_DUMP);
      open_dump_file (DFI_bp, NULL);

      end_branch_prob ();

      close_dump_file (DFI_bp, NULL, NULL_RTX);
      timevar_pop (TV_DUMP);
    }

  targetm.asm_out.file_end ();

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
rest_of_decl_compilation (tree decl,
			  const char *asmspec,
			  int top_level,
			  int at_end)
{
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
	 is seen.  But at end of compilation, do output code for them.

	 We do output all variables when unit-at-a-time is active and rely on
	 callgraph code to defer them except for forward declarations
	 (see gcc.c-torture/compile/920624-1.c) */
      if ((at_end
	   || !DECL_DEFER_OUTPUT (decl)
	   || (flag_unit_at_a_time && DECL_INITIAL (decl)))
	  && !DECL_EXTERNAL (decl))
	{
	  if (flag_unit_at_a_time && !cgraph_global_info_ready
	      && TREE_CODE (decl) != FUNCTION_DECL && top_level)
	    cgraph_varpool_finalize_decl (decl);
	  else
	    assemble_variable (decl, top_level, at_end, 0);
	}

#ifdef ASM_FINISH_DECLARE_OBJECT
      if (decl == last_assemble_variable_decl)
	{
	  ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
				     top_level, at_end);
	}
#endif

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
rest_of_type_compilation (
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)	\
    || defined (SDB_DEBUGGING_INFO) || defined (DWARF2_DEBUGGING_INFO)
			  tree type,
			  int toplev
#else
			  tree type ATTRIBUTE_UNUSED,
			  int toplev ATTRIBUTE_UNUSED
#endif
			  )
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

/* Turn the RTL into assembly.  */
static void
rest_of_handle_final (tree decl, rtx insns)
{
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
}

#ifdef DELAY_SLOTS
/* Run delay slot optimization.  */
static void
rest_of_handle_delay_slots (tree decl, rtx insns)
{
  timevar_push (TV_DBR_SCHED);
  open_dump_file (DFI_dbr, decl);

  dbr_schedule (insns, rtl_dump_file);

  close_dump_file (DFI_dbr, print_rtl, insns);
  timevar_pop (TV_DBR_SCHED);

  ggc_collect ();
}
#endif

#ifdef STACK_REGS
/* Convert register usage from flat register file usage to a stack
   register file.  */
static void
rest_of_handle_stack_regs (tree decl, rtx insns)
{
#if defined (HAVE_ATTR_length)
  /* If flow2 creates new instructions which need splitting
     and scheduling after reload is not done, they might not be
     split until final which doesn't allow splitting
     if HAVE_ATTR_length.  */
#ifdef INSN_SCHEDULING
  if (optimize && !flag_schedule_insns_after_reload)
#else
  if (optimize)
#endif
    {
      timevar_push (TV_SHORTEN_BRANCH);
      split_all_insns (1);
      timevar_pop (TV_SHORTEN_BRANCH);
    }
#endif

  timevar_push (TV_REG_STACK);
  open_dump_file (DFI_stack, decl);

  if (reg_to_stack (insns, rtl_dump_file) && optimize)
    {
      if (cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK
		       | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0))
	  && flag_reorder_blocks)
	{
	  reorder_basic_blocks (0);
	  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_POST_REGSTACK);
	}
    }

  close_dump_file (DFI_stack, print_rtl_with_bb, insns);
  timevar_pop (TV_REG_STACK);

  ggc_collect ();
}
#endif


/* Machine independent reorg pass.  */
static void
rest_of_handle_machine_reorg (tree decl, rtx insns)
{
  timevar_push (TV_MACH_DEP);
  open_dump_file (DFI_mach, decl);

  (*targetm.machine_dependent_reorg) ();

  close_dump_file (DFI_mach, print_rtl, insns);
  timevar_pop (TV_MACH_DEP);

  ggc_collect ();
}


/* Run new register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static bool
rest_of_handle_new_regalloc (tree decl, rtx insns)
{
  int failure;

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
    return true;

  reload_completed = 1;

  return false;
}

/* Run old register allocator.  Return TRUE if we must exit
   rest_of_compilation upon return.  */
static bool
rest_of_handle_old_regalloc (tree decl, rtx insns)
{
  int failure;
  int rebuild_notes;

  /* Allocate the reg_renumber array.  */
  allocate_reg_info (max_regno, FALSE, TRUE);

  /* And the reg_equiv_memory_loc array.  */
  reg_equiv_memory_loc = xcalloc (max_regno, sizeof (rtx));

  allocate_initial_values (reg_equiv_memory_loc);

  regclass (insns, max_reg_num (), rtl_dump_file);
  rebuild_notes = local_alloc ();

  timevar_pop (TV_LOCAL_ALLOC);

  /* Local allocation may have turned an indirect jump into a direct
     jump.  If so, we must rebuild the JUMP_LABEL fields of jumping
     instructions.  */
  if (rebuild_notes)
    {
      timevar_push (TV_JUMP);

      rebuild_jump_labels (insns);
      purge_all_dead_edges (0);

      timevar_pop (TV_JUMP);
    }

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

  return failure;
}

/* Run the regrename and cprop passes.  */
static void
rest_of_handle_regrename (tree decl, rtx insns)
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

/* Reorder basic blocks.  */
static void
rest_of_handle_reorder_blocks (tree decl, rtx insns)
{
  bool changed;
  unsigned int liveness_flags;

  open_dump_file (DFI_bbro, decl);

  /* Last attempt to optimize CFG, as scheduling, peepholing and insn
     splitting possibly introduced more crossjumping opportunities.  */
  liveness_flags = (!HAVE_conditional_execution ? CLEANUP_UPDATE_LIFE : 0);
  changed = cleanup_cfg (CLEANUP_EXPENSIVE | liveness_flags);

  if (flag_sched2_use_traces && flag_schedule_insns_after_reload)
    tracer (liveness_flags);
  if (flag_reorder_blocks)
    reorder_basic_blocks (liveness_flags);
  if (flag_reorder_blocks
      || (flag_sched2_use_traces && flag_schedule_insns_after_reload))
    changed |= cleanup_cfg (CLEANUP_EXPENSIVE | liveness_flags);

  /* On conditional execution targets we can not update the life cheaply, so
     we deffer the updating to after both cleanups.  This may lose some cases
     but should not be terribly bad.  */
  if (changed && HAVE_conditional_execution)
    update_life_info (NULL, UPDATE_LIFE_GLOBAL_RM_NOTES,
		      PROP_DEATH_NOTES | PROP_REG_INFO);
  close_dump_file (DFI_bbro, print_rtl_with_bb, insns);
}

#ifdef INSN_SCHEDULING
/* Run instruction scheduler.  */
static void
rest_of_handle_sched (tree decl, rtx insns)
{
  timevar_push (TV_SCHED);

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */
  if (optimize > 0 && flag_schedule_insns)
    {
      open_dump_file (DFI_sched, decl);

      /* Do control and data sched analysis,
	 and write some of the results to dump file.  */

      schedule_insns (rtl_dump_file);

      close_dump_file (DFI_sched, print_rtl_with_bb, insns);
    }
  timevar_pop (TV_SCHED);

  ggc_collect ();
}

/* Run second scheduling pass after reload.  */
static void
rest_of_handle_sched2 (tree decl, rtx insns)
{
  timevar_push (TV_SCHED2);
  open_dump_file (DFI_sched2, decl);

  /* Do control and data sched analysis again,
     and write some more of the results to dump file.  */

  split_all_insns (1);

  if (flag_sched2_use_superblocks || flag_sched2_use_traces)
    {
      schedule_ebbs (rtl_dump_file);
      /* No liveness updating code yet, but it should be easy to do.
	 reg-stack recompute the liveness when needed for now.  */
      count_or_remove_death_notes (NULL, 1);
      cleanup_cfg (CLEANUP_EXPENSIVE);
    }
  else
    schedule_insns (rtl_dump_file);

  close_dump_file (DFI_sched2, print_rtl_with_bb, insns);
  timevar_pop (TV_SCHED2);

  ggc_collect ();
}
#endif

/* Register allocation pre-pass, to reduce number of moves necessary
   for two-address machines.  */
static void
rest_of_handle_regmove (tree decl, rtx insns)
{
  timevar_push (TV_REGMOVE);
  open_dump_file (DFI_regmove, decl);

  regmove_optimize (insns, max_reg_num (), rtl_dump_file);

  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_UPDATE_LIFE);
  close_dump_file (DFI_regmove, print_rtl_with_bb, insns);
  timevar_pop (TV_REGMOVE);

  ggc_collect ();
}

/* Run tracer.  */
static void
rest_of_handle_tracer (tree decl, rtx insns)
{
  open_dump_file (DFI_tracer, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  tracer (0);
  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (insns, max_reg_num (), 0);
  close_dump_file (DFI_tracer, print_rtl_with_bb, get_insns ());
}

/* If-conversion and CFG cleanup.  */
static void
rest_of_handle_if_conversion (tree decl, rtx insns)
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

/* Rerun if-conversion, as combine may have simplified things enough
   to now meet sequence length restrictions.  */
static void
rest_of_handle_if_after_combine (tree decl, rtx insns)
{
  timevar_push (TV_IFCVT);
  open_dump_file (DFI_ce2, decl);

  no_new_pseudos = 0;
  if_convert (1);
  no_new_pseudos = 1;

  close_dump_file (DFI_ce2, print_rtl_with_bb, insns);
  timevar_pop (TV_IFCVT);
}

static void
rest_of_handle_web (tree decl, rtx insns)
{
  open_dump_file (DFI_web, decl);
  timevar_push (TV_WEB);
  web_main ();
  delete_trivially_dead_insns (insns, max_reg_num ());
  cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_WEB);
  close_dump_file (DFI_web, print_rtl_with_bb, insns);
  reg_scan (get_insns (), max_reg_num (), 0);
}

/* Do branch profiling and static profile estimation passes.  */
static void
rest_of_handle_branch_prob (tree decl, rtx insns)
{
  struct loops loops;

  timevar_push (TV_BRANCH_PROB);
  open_dump_file (DFI_bp, decl);

  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
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
  free_dominance_info (CDI_DOMINATORS);
  close_dump_file (DFI_bp, print_rtl_with_bb, insns);
  timevar_pop (TV_BRANCH_PROB);
}

/* Do optimizations based on expression value profiles.  */
static void
rest_of_handle_value_profile_transformations (tree decl, rtx insns)
{
  open_dump_file (DFI_vpt, decl);
  timevar_push (TV_VPT);

  if (value_profile_transformations ())
    cleanup_cfg (CLEANUP_EXPENSIVE);

  timevar_pop (TV_VPT);
  close_dump_file (DFI_vpt, print_rtl_with_bb, insns);
}

/* Do control and data flow analysis; write some of the results to the
   dump file.  */
static void
rest_of_handle_cfg (tree decl, rtx insns)
{
  open_dump_file (DFI_cfg, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));

  /* It may make more sense to mark constant functions after dead code is
     eliminated by life_analysis, but we need to do it early, as -fprofile-arcs
     may insert code making function non-constant, but we still must consider
     it as constant, otherwise -fbranch-probabilities will not read data back.

     life_analysis rarely eliminates modification of external memory.
   */
  if (optimize)
    {
      /* Alias analysis depends on this information and mark_constant_function
       depends on alias analysis.  */
      reg_scan (insns, max_reg_num (), 1);
      mark_constant_function ();
    }

  close_dump_file (DFI_cfg, print_rtl_with_bb, insns);
}

/* Purge addressofs.  */
static void
rest_of_handle_addressof (tree decl, rtx insns)
{
  open_dump_file (DFI_addressof, decl);

  purge_addressof (insns);
  if (optimize && purge_all_dead_edges (0))
    delete_unreachable_blocks ();
  reg_scan (insns, max_reg_num (), 1);

  close_dump_file (DFI_addressof, print_rtl, insns);
}

/* We may have potential sibling or tail recursion sites.  Select one
   (of possibly multiple) methods of performing the call.  */
static void
rest_of_handle_sibling_calls (rtx insns)
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

  close_dump_file (DFI_sibling, print_rtl, get_insns ());
}

/* Perform jump bypassing and control flow optimizations.  */
static void
rest_of_handle_jump_bypass (tree decl, rtx insns)
{
  timevar_push (TV_BYPASS);
  open_dump_file (DFI_bypass, decl);

  cleanup_cfg (CLEANUP_EXPENSIVE);
  reg_scan (insns, max_reg_num (), 1);

  if (bypass_jumps (rtl_dump_file))
    {
      rebuild_jump_labels (insns);
      cleanup_cfg (CLEANUP_EXPENSIVE);
      delete_trivially_dead_insns (insns, max_reg_num ());
    }

  close_dump_file (DFI_bypass, print_rtl_with_bb, insns);
  timevar_pop (TV_BYPASS);

  ggc_collect ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
}

/* Handle inlining of functions in rest_of_compilation.  Return TRUE
   if we must exit rest_of_compilation upon return.  */
static bool
rest_of_handle_inlining (tree decl)
{
  rtx insns;
  int inlinable = 0;
  tree parent;
  const char *lose;

  /* If we are reconsidering an inline function at the end of
     compilation, skip the stuff for making it inline.  */
  if (cfun->rtl_inline_init)
    return 0;
  cfun->rtl_inline_init = 1;

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
	return true;
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
	  if (warn_inline && lose && DECL_INLINE (decl))
            {
              char *msg = concat ("%J", lose, NULL);
              warning (msg, decl);
              free (msg);
            }
	  DECL_ABSTRACT_ORIGIN (decl) = 0;
	  /* Don't really compile an extern inline function.
	     If we can't make it inline, pretend
	     it was only declared.  */
	  if (DECL_EXTERNAL (decl))
	    {
	      DECL_INITIAL (decl) = 0;
	      return true;
	    }
	}
      else
	inlinable = DECL_INLINE (decl) = 1;
    }

  insns = get_insns ();

  /* Dump the rtl code if we are dumping rtl.  */

  if (open_dump_file (DFI_rtl, decl))
    {
      if (DECL_SAVED_INSNS (decl) && DECL_SAVED_INSNS (decl)->saved_for_inline)
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
	  /* Egad.  This RTL deferral test conflicts with Fortran assumptions
	     for unreferenced symbols.  See g77.f-torture/execute/980520-1.f.
	     But removing this line from the check breaks all languages that
	     use the call graph to output symbols.  This hard-coded check is
	     the least invasive work-around.  Nested functions need to be
	     deferred too.  */
	  && (flag_inline_functions
	      || strcmp (lang_hooks.name, "GNU F77") == 0
	      || (cgraph_n_nodes > 0 && cgraph_node (decl)->origin))
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
      return true;
    }

  /* If specified extern inline but we aren't inlining it, we are
     done.  This goes for anything that gets here with DECL_EXTERNAL
     set, not just things with DECL_INLINE.  */
  return (bool) DECL_EXTERNAL (decl);
}

/* Try to identify useless null pointer tests and delete them.  */
static void
rest_of_handle_null_pointer (tree decl, rtx insns)
{
  open_dump_file (DFI_null, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  if (delete_null_pointer_checks (insns))
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  close_dump_file (DFI_null, print_rtl_with_bb, insns);
}

/* Try combining insns through substitution.  */
static void
rest_of_handle_combine (tree decl, rtx insns)
{
  int rebuild_jump_labels_after_combine = 0;

  timevar_push (TV_COMBINE);
  open_dump_file (DFI_combine, decl);

  rebuild_jump_labels_after_combine
    = combine_instructions (insns, max_reg_num ());

  /* Combining insns may have turned an indirect jump into a
     direct jump.  Rebuild the JUMP_LABEL fields of jumping
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

/* Perform life analysis.  */
static void
rest_of_handle_life (tree decl, rtx insns)
{
  open_dump_file (DFI_life, decl);
  regclass_init ();

#ifdef ENABLE_CHECKING
  verify_flow_info ();
#endif
  life_analysis (insns, rtl_dump_file, PROP_FINAL);
  if (optimize)
    cleanup_cfg ((optimize ? CLEANUP_EXPENSIVE : 0) | CLEANUP_UPDATE_LIFE
		 | CLEANUP_LOG_LINKS
		 | (flag_thread_jumps ? CLEANUP_THREADING : 0));
  timevar_pop (TV_FLOW);

  if (warn_uninitialized)
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
}

/* Perform common subexpression elimination.  Nonzero value from
   `cse_main' means that jumps were simplified and some code may now
   be unreachable, so do jump optimization again.  */
static void
rest_of_handle_cse (tree decl, rtx insns)
{
  int tem;

  open_dump_file (DFI_cse, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  timevar_push (TV_CSE);

  reg_scan (insns, max_reg_num (), 1);

  tem = cse_main (insns, max_reg_num (), 0, rtl_dump_file);
  if (tem)
    rebuild_jump_labels (insns);
  if (purge_all_dead_edges (0))
    delete_unreachable_blocks ();

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

/* Run second CSE pass after loop optimizations.  */
static void
rest_of_handle_cse2 (tree decl, rtx insns)
{
  int tem;

  timevar_push (TV_CSE2);
  open_dump_file (DFI_cse2, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  /* CFG is no longer maintained up-to-date.  */
  tem = cse_main (insns, max_reg_num (), 1, rtl_dump_file);

  /* Run a pass to eliminate duplicated assignments to condition code
     registers.  We have to run this after bypass_jumps, because it
     makes it harder for that pass to determine whether a jump can be
     bypassed safely.  */
  cse_condition_code_reg ();

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

/* Perform global cse.  */
static void
rest_of_handle_gcse (tree decl, rtx insns)
{
  int save_csb, save_cfj;
  int tem2 = 0, tem;

  timevar_push (TV_GCSE);
  open_dump_file (DFI_gcse, decl);

  tem = gcse_main (insns, rtl_dump_file);
  rebuild_jump_labels (insns);
  delete_trivially_dead_insns (insns, max_reg_num ());

  save_csb = flag_cse_skip_blocks;
  save_cfj = flag_cse_follow_jumps;
  flag_cse_skip_blocks = flag_cse_follow_jumps = 0;

  /* Instantiate any remaining CONSTANT_P_RTX nodes.  */
  if (current_function_calls_constant_p)
    purge_builtin_constant_p ();

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
static void
rest_of_handle_loop_optimize (tree decl, rtx insns)
{
  int do_unroll, do_prefetch;

  timevar_push (TV_LOOP);
  delete_dead_jumptables ();
  cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);
  open_dump_file (DFI_loop, decl);

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  if (flag_unroll_loops)
    do_unroll = LOOP_AUTO_UNROLL;	/* Having two unrollers is useless.  */
  else
    do_unroll = flag_old_unroll_loops ? LOOP_UNROLL : LOOP_AUTO_UNROLL;
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

/* Perform loop optimizations.  It might be better to do them a bit
   sooner, but we want the profile feedback to work more
   efficiently.  */
static void
rest_of_handle_loop2 (tree decl, rtx insns)
{
  struct loops *loops;
  timevar_push (TV_LOOP);
  open_dump_file (DFI_loop2, decl);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);

  loops = loop_optimizer_init (rtl_dump_file);

  if (loops)
    {
      /* The optimizations:  */
      if (flag_unswitch_loops)
	unswitch_loops (loops);

      if (flag_peel_loops || flag_unroll_loops)
	unroll_and_peel_loops (loops,
			       (flag_peel_loops ? UAP_PEEL : 0) |
			       (flag_unroll_loops ? UAP_UNROLL : 0) |
			       (flag_unroll_all_loops ? UAP_UNROLL_ALL : 0));

      loop_optimizer_finalize (loops, rtl_dump_file);
    }

  cleanup_cfg (CLEANUP_EXPENSIVE);
  delete_trivially_dead_insns (insns, max_reg_num ());
  reg_scan (insns, max_reg_num (), 0);
  if (rtl_dump_file)
    dump_flow_info (rtl_dump_file);
  close_dump_file (DFI_loop2, print_rtl_with_bb, get_insns ());
  timevar_pop (TV_LOOP);
  ggc_collect ();
}

/* This is called from finish_function (within langhooks.parse_file)
   after each top-level definition is parsed.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (tree decl)
{
  rtx insns;

  timevar_push (TV_REST_OF_COMPILATION);

  /* Register rtl specific functions for cfg.  */
  rtl_register_cfg_hooks ();

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

  if (rest_of_handle_inlining (decl))
    goto exit_rest_of_compilation;

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
  if (cgraph_function_possibly_inlined_p (decl))
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

  /* Turn NOTE_INSN_PREDICTIONs into branch predictions.  */
  if (flag_guess_branch_prob)
    {
      timevar_push (TV_BRANCH_PROB);
      note_prediction_to_br_prob ();
      timevar_pop (TV_BRANCH_PROB);
    }

  if (flag_optimize_sibling_calls)
    rest_of_handle_sibling_calls (insns);

  /* We have to issue these warnings now already, because CFG cleanups
     further down may destroy the required information.  However, this
     must be done after the sibcall optimization pass because the barrier
     emitted for noreturn calls that are candidate for the optimization
     is folded into the CALL_PLACEHOLDER until after this pass, so the
     CFG is inaccurate.  */
  check_function_return_warnings ();

  timevar_pop (TV_JUMP);

  insn_locators_initialize ();
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
    goto exit_rest_of_compilation;

  timevar_push (TV_JUMP);

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE | CLEANUP_PRE_LOOP);

  if (flag_delete_null_pointer_checks)
    rest_of_handle_null_pointer (decl, insns);

  /* Jump optimization, and the removal of NULL pointer checks, may
     have reduced the number of instructions substantially.  CSE, and
     future passes, allocate arrays whose dimensions involve the
     maximum instruction UID, so if we can reduce the maximum UID
     we'll save big on memory.  */
  renumber_insns (rtl_dump_file);
  timevar_pop (TV_JUMP);

  close_dump_file (DFI_jump, print_rtl_with_bb, insns);

  ggc_collect ();

  if (optimize > 0)
    rest_of_handle_cse (decl, insns);

  rest_of_handle_addressof (decl, insns);

  ggc_collect ();

  if (optimize > 0)
    {
      if (flag_gcse)
	rest_of_handle_gcse (decl, insns);

      if (flag_loop_optimize)
	rest_of_handle_loop_optimize (decl, insns);

      if (flag_gcse)
	rest_of_handle_jump_bypass (decl, insns);
    }

  timevar_push (TV_FLOW);

  rest_of_handle_cfg (decl, insns);

  if (optimize > 0
      || profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    {
      rest_of_handle_branch_prob (decl, insns);

      if (flag_branch_probabilities
	  && flag_profile_values
	  && flag_value_profile_transformations)
	rest_of_handle_value_profile_transformations (decl, insns);

      /* Remove the death notes created for vpt.  */
      if (flag_profile_values)
	count_or_remove_death_notes (NULL, 1);
    }

  if (optimize > 0)
    rest_of_handle_if_conversion (decl, insns);

  if (flag_tracer)
    rest_of_handle_tracer (decl, insns);

  if (optimize > 0
      && (flag_unswitch_loops
	  || flag_peel_loops
	  || flag_unroll_loops))
    rest_of_handle_loop2 (decl, insns);

  if (flag_web)
    rest_of_handle_web (decl, insns);

  if (flag_rerun_cse_after_loop)
    rest_of_handle_cse2 (decl, insns);

  cse_not_expected = 1;

  rest_of_handle_life (decl, insns);

  if (optimize > 0)
    rest_of_handle_combine (decl, insns);

  if (flag_if_conversion)
    rest_of_handle_if_after_combine (decl, insns);

  if (optimize > 0 && (flag_regmove || flag_expensive_optimizations))
    rest_of_handle_regmove (decl, insns);

  /* Do unconditional splitting before register allocation to allow machine
     description to add extra information not needed previously.  */
  split_all_insns (1);

#ifdef OPTIMIZE_MODE_SWITCHING
  timevar_push (TV_MODE_SWITCH);

  no_new_pseudos = 0;
  optimize_mode_switching (NULL);
  no_new_pseudos = 1;

  timevar_pop (TV_MODE_SWITCH);
#endif

  /* Any of the several passes since flow1 will have munged register
     lifetime data a bit.  We need it to be up to date for scheduling
     (see handling of reg_known_equiv in init_alias_analysis).  */
  recompute_reg_usage (insns, !optimize_size);

#ifdef INSN_SCHEDULING
  rest_of_handle_sched (decl, insns);
#endif

  /* Determine if the current function is a leaf before running reload
     since this can impact optimizations done by the prologue and
     epilogue thus changing register elimination offsets.  */
  current_function_is_leaf = leaf_function_p ();

  timevar_push (TV_LOCAL_ALLOC);
  open_dump_file (DFI_lreg, decl);

  if (flag_new_regalloc)
    {
      if (rest_of_handle_new_regalloc (decl, insns))
	goto exit_rest_of_compilation;
    }
  else
    {
      if (rest_of_handle_old_regalloc (decl, insns))
	goto exit_rest_of_compilation;
    }

  ggc_collect ();

  open_dump_file (DFI_postreload, decl);

  /* Do a very simple CSE pass over just the hard registers.  */
  if (optimize > 0)
    {
      timevar_push (TV_RELOAD_CSE_REGS);
      reload_cse_regs (insns);
      /* reload_cse_regs can eliminate potentially-trapping MEMs.
	 Remove any EH edges associated with them.  */
      if (flag_non_call_exceptions)
	purge_all_dead_edges (0);
      timevar_pop (TV_RELOAD_CSE_REGS);
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

    if (flag_branch_target_load_optimize)
      {
	open_dump_file (DFI_branch_target_load, decl);

	branch_target_load_optimize (insns, false);

	close_dump_file (DFI_branch_target_load, print_rtl_with_bb, insns);

	ggc_collect ();
      }

  if (optimize)
    cleanup_cfg (CLEANUP_EXPENSIVE);

  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */
  thread_prologue_and_epilogue_insns (insns);
  epilogue_completed = 1;

  if (optimize)
    {
      life_analysis (insns, rtl_dump_file, PROP_POSTRELOAD);
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

  open_dump_file (DFI_ce3, decl);
  if (optimize)
    /* Last attempt to optimize CFG, as scheduling, peepholing and insn
       splitting possibly introduced more crossjumping opportunities.  */
    cleanup_cfg (CLEANUP_EXPENSIVE
		 | CLEANUP_UPDATE_LIFE 
		 | (flag_crossjumping ? CLEANUP_CROSSJUMP : 0));
  if (flag_if_conversion2)
    {
      timevar_push (TV_IFCVT2);

      if_convert (1);

      timevar_pop (TV_IFCVT2);
    }
  close_dump_file (DFI_ce3, print_rtl_with_bb, insns);

  if (optimize > 0)
    {
      if (flag_rename_registers || flag_cprop_registers)
	rest_of_handle_regrename (decl, insns);

      rest_of_handle_reorder_blocks (decl, insns);
    }

  if (flag_branch_target_load_optimize2)
    {
      /* Leave this a warning for now so that it is possible to experiment
	 with running this pass twice.  In 3.6, we should either make this
	 an error, or use separate dump files.  */
      if (flag_branch_target_load_optimize)
	warning ("branch target register load optimization is not intended "
		 "to be run twice");

      open_dump_file (DFI_branch_target_load, decl);

      branch_target_load_optimize (insns, true);

      close_dump_file (DFI_branch_target_load, print_rtl_with_bb, insns);

      ggc_collect ();
    }

#ifdef INSN_SCHEDULING
  if (optimize > 0 && flag_schedule_insns_after_reload)
    rest_of_handle_sched2 (decl, insns);
#endif

#ifdef LEAF_REGISTERS
  current_function_uses_only_leaf_regs
    = optimize > 0 && only_leaf_regs_used () && leaf_function_p ();
#endif

#ifdef STACK_REGS
  rest_of_handle_stack_regs (decl, insns);
#endif

  compute_alignments ();

  /* CFG is no longer maintained up-to-date.  */
  free_bb_for_insn ();

  if (targetm.machine_dependent_reorg != 0)
    rest_of_handle_machine_reorg (decl, insns);

  purge_line_number_notes (insns);
  cleanup_barriers ();

#ifdef DELAY_SLOTS
  if (optimize > 0 && flag_delayed_branch)
    rest_of_handle_delay_slots (decl, insns);
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

  rest_of_handle_final (decl, insns);

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

  coverage_end_function ();

  /* In case the function was not output,
     don't leave any temporary anonymous types
     queued up for sdb output.  */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_types (NULL_TREE);
#endif

  reload_completed = 0;
  epilogue_completed = 0;
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

  if ((*targetm.binds_local_p) (current_function_decl))
    {
      int pref = cfun->preferred_stack_boundary;
      if (cfun->stack_alignment_needed > cfun->preferred_stack_boundary)
	pref = cfun->stack_alignment_needed;
      cgraph_rtl_info (current_function_decl)->preferred_incoming_stack_boundary
        = pref;
    }

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
      DECL_SAVED_INSNS (decl) = 0;
    }
  cfun = 0;

  ggc_collect ();

  timevar_pop (TV_REST_OF_COMPILATION);
}

/* Display help for target options.  */
void
display_target_options (void)
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
		printf (_("  -m%-23s [undocumented]\n"), option);
	    }
	  else if (*description != 0)
	    doc += printf ("  -m%-23s %s\n", option, _(description));
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
		printf (_("  -m%-23s [undocumented]\n"), option);
	    }
	  else if (*description != 0)
	    doc += printf ("  -m%-23s %s\n", option, _(description));
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

void
decode_d_option (const char *arg)
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
      case 'H':
	setup_core_dumping();
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

/* Indexed by enum debug_info_type.  */
const char *const debug_type_names[] =
{
  "none", "stabs", "coff", "dwarf-1", "dwarf-2", "xcoff", "vms"
};

/* Decode -m switches.  */
/* Decode the switch -mNAME.  */

void
set_target_switch (const char *name)
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
	if (target_options[j].value)
	  {
	    if (!strcmp (target_options[j].prefix, name))
	      {
		*target_options[j].variable = target_options[j].value;
		valid_target_option = 1;
	      }
	  }
	else
	  {
	    if (!strncmp (target_options[j].prefix, name, len))
	      {
		*target_options[j].variable = name + len;
		valid_target_option = 1;
	      }
	  }
      }
#endif

  if (!valid_target_option)
    error ("invalid option `%s'", name);
}

/* Print version information to FILE.
   Each line begins with INDENT (for the case where FILE is the
   assembler output file).  */

void
print_version (FILE *file, const char *indent)
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
print_single_switch (FILE *file, int pos, int max,
		     const char *indent, const char *sep, const char *term,
		     const char *type, const char *name)
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
print_switch_values (FILE *file, int pos, int max,
		     const char *indent, const char *sep, const char *term)
{
  size_t j;
  const char **p;

  /* Fill in the -frandom-seed option, if the user didn't pass it, so
     that it can be printed below.  This helps reproducibility.  */
  randomize ();

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
init_asm_output (const char *name)
{
  if (name == NULL && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      if (asm_file_name == 0)
	{
	  int len = strlen (dump_base_name);
	  char *dumpname = xmalloc (len + 6);
	  memcpy (dumpname, dump_base_name, len + 1);
	  strip_off_ending (dumpname, len);
	  strcat (dumpname, ".s");
	  asm_file_name = dumpname;
	}
      if (!strcmp (asm_file_name, "-"))
	asm_out_file = stdout;
      else
	asm_out_file = fopen (asm_file_name, "w+");
      if (asm_out_file == 0)
	fatal_error ("can't open %s for writing: %m", asm_file_name);
    }

#ifdef IO_BUFFER_SIZE
  setvbuf (asm_out_file, xmalloc (IO_BUFFER_SIZE),
	   _IOFBF, IO_BUFFER_SIZE);
#endif

  if (!flag_syntax_only)
    {
      targetm.asm_out.file_start ();

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

/* Default version of get_pch_validity.
   By default, every flag difference is fatal; that will be mostly right for
   most targets, but completely right for very few.  */

void *
default_get_pch_validity (size_t *len)
{
#ifdef TARGET_OPTIONS
  size_t i;
#endif
  char *result, *r;

  *len = sizeof (target_flags) + 2;
#ifdef TARGET_OPTIONS
  for (i = 0; i < ARRAY_SIZE (target_options); i++)
    {
      *len += 1;
      if (*target_options[i].variable)
	*len += strlen (*target_options[i].variable);
    }
#endif

  result = r = xmalloc (*len);
  r[0] = flag_pic;
  r[1] = flag_pie;
  r += 2;
  memcpy (r, &target_flags, sizeof (target_flags));
  r += sizeof (target_flags);

#ifdef TARGET_OPTIONS
  for (i = 0; i < ARRAY_SIZE (target_options); i++)
    {
      const char *str = *target_options[i].variable;
      size_t l;
      if (! str)
	str = "";
      l = strlen (str) + 1;
      memcpy (r, str, l);
      r += l;
    }
#endif

  return result;
}

/* Default version of pch_valid_p.  */

const char *
default_pch_valid_p (const void *data_p, size_t len)
{
  const char *data = (const char *)data_p;
  const char *flag_that_differs = NULL;
  size_t i;

  /* -fpic and -fpie also usually make a PCH invalid.  */
  if (data[0] != flag_pic)
    return _("created and used with different settings of -fpic");
  if (data[1] != flag_pie)
    return _("created and used with different settings of -fpie");
  data += 2;

  /* Check target_flags.  */
  if (memcmp (data, &target_flags, sizeof (target_flags)) != 0)
    {
      for (i = 0; i < ARRAY_SIZE (target_switches); i++)
	{
	  int bits;
	  int tf;

	  memcpy (&tf, data, sizeof (target_flags));

	  bits = target_switches[i].value;
	  if (bits < 0)
	    bits = -bits;
	  if ((target_flags & bits) != (tf & bits))
	    {
	      flag_that_differs = target_switches[i].name;
	      goto make_message;
	    }
	}
      abort ();
    }
  data += sizeof (target_flags);
  len -= sizeof (target_flags);

  /* Check string options.  */
#ifdef TARGET_OPTIONS
  for (i = 0; i < ARRAY_SIZE (target_options); i++)
    {
      const char *str = *target_options[i].variable;
      size_t l;
      if (! str)
	str = "";
      l = strlen (str) + 1;
      if (len < l || memcmp (data, str, l) != 0)
	{
	  flag_that_differs = target_options[i].prefix;
	  goto make_message;
	}
      data += l;
      len -= l;
    }
#endif

  return NULL;

 make_message:
  {
    char *r;
    asprintf (&r, _("created and used with differing settings of `-m%s'"),
		  flag_that_differs);
    if (r == NULL)
      return _("out of memory");
    return r;
  }
}

/* Default tree printer.   Handles declarations only.  */
static bool
default_tree_printer (pretty_printer * pp, text_info *text)
{
  switch (*text->format_spec)
    {
    case 'D':
    case 'F':
    case 'T':
      {
        tree t = va_arg (*text->args_ptr, tree);
        const char *n = DECL_NAME (t)
          ? (*lang_hooks.decl_printable_name) (t, 2)
          : "<anonymous>";
        pp_string (pp, n);
      }
      return true;

    default:
      return false;
    }
}

/* Initialization of the front end environment, before command line
   options are parsed.  Signal handlers, internationalization etc.
   ARGV0 is main's argv[0].  */
static void
general_init (const char *argv0)
{
  const char *p;

  p = argv0 + strlen (argv0);
  while (p != argv0 && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  progname = p;

  xmalloc_set_program_name (progname);

  hex_init ();

  gcc_init_libintl ();

  /* Initialize the diagnostics reporting machinery, so option parsing
     can give warnings and errors.  */
  diagnostic_initialize (global_dc);
  /* Set a default printer.  Language specific initializations will
     override it later.  */
  pp_format_decoder (global_dc->printer) = &default_tree_printer;

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

  /* Other host-specific signal setup.  */
  (*host_hooks.extra_signals)();

  /* Initialize the garbage-collector, string pools and tree type hash
     table.  */
  init_ggc ();
  init_stringpool ();
  init_ttree ();

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Register the language-independent parameters.  */
  add_params (lang_independent_params, LAST_PARAM);

  /* This must be done after add_params but before argument processing.  */
  init_ggc_heuristics();
}

/* Process the options that have been parsed.  */
static void
process_options (void)
{
  /* Allow the front end to perform consistency checks and do further
     initialization based on the command line options.  This hook also
     sets the original filename if appropriate (e.g. foo.i -> foo.c)
     so we can correctly initialize debug output.  */
  no_backend = (*lang_hooks.post_options) (&main_input_filename);
  input_filename = main_input_filename;

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

  /* Set aux_base_name if not already set.  */
  if (aux_base_name)
    ;
  else if (main_input_filename)
    {
      char *name = xstrdup (lbasename (main_input_filename));

      strip_off_ending (name, strlen (name));
      aux_base_name = name;
    }
  else
    aux_base_name = "gccaux";

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

  if (flag_unroll_loops)
    {
      flag_old_unroll_loops = 0;
      flag_old_unroll_all_loops = 0;
    }

  if (flag_old_unroll_all_loops)
    flag_old_unroll_loops = 1;

  /* Old loop unrolling requires that strength_reduction be on also.  Silently
     turn on strength reduction here if it isn't already on.  Also, the loop
     unrolling code assumes that cse will be run after loop, so that must
     be turned on also.  */
  if (flag_old_unroll_loops)
    {
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
    }
  if (flag_unroll_loops || flag_peel_loops)
    flag_rerun_cse_after_loop = 1;

  if (flag_non_call_exceptions)
    flag_asynchronous_unwind_tables = 1;
  if (flag_asynchronous_unwind_tables)
    flag_unwind_tables = 1;

  /* Disable unit-at-a-time mode for frontends not supporting callgraph
     interface.  */
  if (flag_unit_at_a_time && ! lang_hooks.callgraph.expand_function)
    flag_unit_at_a_time = 0;

  if (flag_value_profile_transformations)
    flag_profile_values = 1;

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

  if (flag_syntax_only)
    {
      write_symbols = NO_DEBUG;
      profile_flag = 0;
    }

  /* A lot of code assumes write_symbols == NO_DEBUG if the debugging
     level is 0.  */
  if (debug_info_level == DINFO_LEVEL_NONE)
    write_symbols = NO_DEBUG;

  /* Now we know write_symbols, set up the debug hooks based on it.
     By default we do nothing for debug output.  */
  if (write_symbols == NO_DEBUG)
    debug_hooks = &do_nothing_debug_hooks;
#if defined(DBX_DEBUGGING_INFO)
  else if (write_symbols == DBX_DEBUG)
    debug_hooks = &dbx_debug_hooks;
#endif
#if defined(XCOFF_DEBUGGING_INFO)
  else if (write_symbols == XCOFF_DEBUG)
    debug_hooks = &xcoff_debug_hooks;
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (write_symbols == SDB_DEBUG)
    debug_hooks = &sdb_debug_hooks;
#endif
#ifdef DWARF2_DEBUGGING_INFO
  else if (write_symbols == DWARF2_DEBUG)
    debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  else if (write_symbols == VMS_DEBUG || write_symbols == VMS_AND_DWARF2_DEBUG)
    debug_hooks = &vmsdbg_debug_hooks;
#endif
  else
    error ("target system does not support the \"%s\" debug format",
	   debug_type_names[write_symbols]);

  /* If auxiliary info generation is desired, open the output file.
     This goes in the same directory as the source file--unlike
     all the other output files.  */
  if (flag_gen_aux_info)
    {
      aux_info_file = fopen (aux_info_file_name, "w");
      if (aux_info_file == 0)
	fatal_error ("can't open %s: %m", aux_info_file_name);
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
backend_init (void)
{
  init_emit_once (debug_info_level == DINFO_LEVEL_NORMAL
		  || debug_info_level == DINFO_LEVEL_VERBOSE
#ifdef VMS_DEBUGGING_INFO
		    /* Enable line number info for traceback.  */
		    || debug_info_level > DINFO_LEVEL_NONE
#endif
		    || flag_test_coverage
		    || warn_notreached);

  init_regs ();
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
lang_dependent_init (const char *name)
{
  if (dump_base_name == 0)
    dump_base_name = name ? name : "gccdump";

  /* Other front-end initialization.  */
  if ((*lang_hooks.init) () == 0)
    return 0;

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
finalize (void)
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
	fatal_error ("error writing to %s: %m", asm_file_name);
      if (fclose (asm_out_file) != 0)
	fatal_error ("error closing %s: %m", asm_file_name);
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
      dump_rtx_statistics ();
      dump_varray_statistics ();
      dump_alloc_pool_statistics ();
    }

  /* Free up memory for the benefit of leak detectors.  */
  free_reg_info ();

  /* Language-specific end of compilation actions.  */
  (*lang_hooks.finish) ();
}

/* Initialize the compiler, and compile the input file.  */
static void
do_compile (void)
{
  /* Initialize timing first.  The C front ends read the main file in
     the post_options hook, and C++ does file timings.  */
  if (time_report || !quiet_flag  || flag_detailed_statistics)
    timevar_init ();
  timevar_start (TV_TOTAL);

  process_options ();

  /* Don't do any more if an error has already occurred.  */
  if (!errorcount)
    {
      /* This must be run always, because it is needed to compute the FP
	 predefined macros, such as __LDBL_MAX__, for targets using non
	 default FP formats.  */
      init_adjust_machine_modes ();

      /* Set up the back-end if requested.  */
      if (!no_backend)
	backend_init ();

      /* Language-dependent initialization.  Returns true on success.  */
      if (lang_dependent_init (main_input_filename))
	{
	  if (flag_unit_at_a_time)
	    {
	      open_dump_file (DFI_cgraph, NULL);
	      cgraph_dump_file = rtl_dump_file;
	      rtl_dump_file = NULL;
	    }

	  compile_file ();

	  if (flag_unit_at_a_time)
	    {
	      rtl_dump_file = cgraph_dump_file;
	      cgraph_dump_file = NULL;
              close_dump_file (DFI_cgraph, NULL, NULL_RTX);
	    }
	}

      finalize ();
    }

  /* Stop timing and print the times.  */
  timevar_stop (TV_TOTAL);
  timevar_print (stderr);
}

/* Entry point of cc1, cc1plus, jc1, f771, etc.
   Exit code is FATAL_EXIT_CODE if can't open files or if there were
   any errors, or SUCCESS_EXIT_CODE if compilation succeeded.

   It is not safe to call this function more than once.  */

int
toplev_main (unsigned int argc, const char **argv)
{
  save_argv = argv;

  /* Initialization of GCC's environment, and diagnostics.  */
  general_init (argv[0]);

  /* Parse the options and do minimal processing; basically just
     enough to default flags appropriately.  */
  decode_options (argc, argv);

  randomize ();

  /* Exit early if we can (e.g. -help).  */
  if (!exit_after_options)
    do_compile ();

  if (errorcount || sorrycount)
    return (FATAL_EXIT_CODE);

  return (SUCCESS_EXIT_CODE);
}
