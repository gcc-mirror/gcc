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

#include "line-map.h"
#include "input.h"
#include "tree.h"
#include "version.h"
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
#include "tree-alias-common.h" 
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

/* Used to enable -fvar-tracking, -fweb and -frename-registers according
   to optimize and default_debug_hooks in process_options ().  */
#define AUTODETECT_FLAG_VAR_TRACKING 2

/* Current position in real source file.  */

location_t input_location;

struct line_maps line_table;

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

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm.h file
   and set by `-m...' switches.  Must be defined in rtlanal.c.  */

extern int target_flags;

/* A mask of target_flags that includes bit X if X was set or cleared
   on the command line.  */

int target_flags_explicit;

/* Debug hooks - dependent upon command line options.  */

const struct gcc_debug_hooks *debug_hooks;

/* Debug hooks - target default.  */

static const struct gcc_debug_hooks *default_debug_hooks;

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

/* A DECL for the current file-scope context.  When using IMA, this heads a
   chain of FILE_DECLs; currently only C uses it.  */

tree current_file_decl;

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

/* Nonzero if blocks should be partitioned into hot and cold sections in
   addition to being reordered.  */

int flag_reorder_blocks_and_partition = 0;

/* Nonzero if functions should be reordered.  */

int flag_reorder_functions = 0;

/* Nonzero if registers should be renamed.  When
   flag_rename_registers == AUTODETECT_FLAG_VAR_TRACKING it will be set
   according to optimize and default_debug_hooks in process_options (),
   but we do not do this yet because it triggers aborts in flow.c.  */
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

/* Nonzero means give an enum type only as many bytes as it needs.  A value
   of 2 means it has not yet been initialized.  */

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

/* Nonzero means performs web construction pass.  When flag_web ==
   AUTODETECT_FLAG_VAR_TRACKING it will be set according to optimize
   and default_debug_hooks in process_options ().  */

int flag_web = AUTODETECT_FLAG_VAR_TRACKING;

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

/* Nonzero if we want to perform redundant load after store elimination
   in gcse.  */

int flag_gcse_las = 1;

/* Nonzero means perform global cse after register allocation.  */
int flag_gcse_after_reload = 0;

/* Perform target register optimization before prologue / epilogue
   threading.  */

int flag_branch_target_load_optimize = 0;

/* Perform target register optimization after prologue / epilogue
   threading and jump2.  */

int flag_branch_target_load_optimize2 = 0;

/* For the bt-load pass, nonzero means don't re-use branch target registers
   in any basic block.  */

int flag_btr_bb_exclusive;

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

/* The following flag controls the module scheduling activation.  */
int flag_modulo_sched = 0;

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

/* Mudflap bounds-checking transform.  */
int flag_mudflap = 0;
int flag_mudflap_threads = 0;
int flag_mudflap_ignore_reads = 0;

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

/* If nonzero, use tree-based instead of rtl-based profiling.  */
int flag_tree_based_profiling = 0;

/* Enable SSA-GVN on trees.  */
int flag_tree_gvn = 0;

/* Enable the SSA-PRE tree optimization.  */
int flag_tree_pre = 0;

/* Enable points-to analysis on trees.  */
enum pta_type flag_tree_points_to = PTA_NONE;

/* Enable SSA-CCP on trees.  */
int flag_tree_ccp = 0;

/* Enable SSA-DCE on trees.  */
int flag_tree_dce = 0;

/* Enable loop header copying on tree-ssa.  */
int flag_tree_ch = 0;

/* Enable scalar replacement of aggregates.  */
int flag_tree_sra = 0;

/* Enable SSA->normal pass memory location coalescing.  */
int flag_tree_combine_temps = 0;

/* Enable SSA->normal pass expression replacement.  */
int flag_tree_ter = 0;

/* Enable SSA->normal live range splitting.  */
int flag_tree_live_range_split = 0;

/* Enable dominator optimizations.  */
int flag_tree_dom = 0;

/* Enable copy rename optimization.  */
int flag_tree_copyrename = 0;

/* Enable dead store elimination.  */
int flag_tree_dse = 0;

/* Nonzero if we perform superblock formation.  */
int flag_tracer = 0;

/* Nonzero if we perform whole unit at a time compilation.  */

int flag_unit_at_a_time = 0;

/* Nonzero if we should track variables.  When
   flag_var_tracking == AUTODETECT_FLAG_VAR_TRACKING it will be set according
   to optimize, debug_info_level and debug_hooks in process_options ().  */
int flag_var_tracking = AUTODETECT_FLAG_VAR_TRACKING;

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
  {"gcse-after-reload", &flag_gcse_after_reload, 1},
  {"branch-target-load-optimize", &flag_branch_target_load_optimize, 1 },
  {"branch-target-load-optimize2", &flag_branch_target_load_optimize2, 1 },
  {"btr-bb-exclusive", &flag_btr_bb_exclusive, 1 },
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
  {"modulo-sched", &flag_modulo_sched, 1 },
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
  {"tree-based-profiling", &flag_tree_based_profiling, 1 },
  {"reorder-blocks", &flag_reorder_blocks, 1 },
  {"reorder-blocks-and-partition", &flag_reorder_blocks_and_partition, 1},
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
  { "new-ra", &flag_new_regalloc, 1 },
  { "var-tracking", &flag_var_tracking, 1},
  { "tree-gvn", &flag_tree_gvn, 1 },
  { "tree-pre", &flag_tree_pre, 1 },
  { "tree-ccp", &flag_tree_ccp, 1 },
  { "tree-dce", &flag_tree_dce, 1 },
  { "tree-dominator-opts", &flag_tree_dom, 1 },
  { "tree-copyrename", &flag_tree_copyrename, 1 },
  { "tree-dse", &flag_tree_dse, 1 },
  { "tree-combine-temps", &flag_tree_combine_temps, 1 },
  { "tree-ter", &flag_tree_ter, 1 },
  { "tree-lrs", &flag_tree_live_range_split, 1 },
  { "tree-ch", &flag_tree_ch, 1 }
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
FILE *dump_file = NULL;
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
    {
      if (strcmp (src_pwd, pwd) == 0)
	return true;
      else
	return false;
    }

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
	verbatim (" %s", lang_hooks.decl_printable_name (decl, 2));
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
	lang_hooks.finish_incomplete_decl (decl);
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
	  && lang_hooks.decls.warn_unused_global (decl))
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
  lang_hooks.parse_file (set_yydebug);

  /* In case there were missing block closers,
     get us back to the global binding level.  */
  lang_hooks.clear_binding_stack ();

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */
  timevar_pop (TV_PARSE);

  if (flag_syntax_only)
    return;

  lang_hooks.decls.final_write_globals ();

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

  /* Attach a special .ident directive to the end of the file to identify
     the version of GCC which compiled this code.  The format of the .ident
     string is patterned after the ones produced by native SVR4 compilers.  */
#ifdef IDENT_ASM_OP
  if (!flag_no_ident)
    fprintf (asm_out_file, "%s\"GCC: (GNU) %s\"\n",
	     IDENT_ASM_OP, version_string);
#endif

  /* This must be at the end.  Some target ports emit end of file directives
     into the assembly file here, and hence we can not output anything to the
     assembly file after this point.  */
  targetm.asm_out.file_end ();
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
  int c;

  while (*arg)
    switch (c = *arg++)
      {
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

      case 'a':
      default:
	if (!enable_rtl_dump_file (c))
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
          ? lang_hooks.decl_printable_name (t, 2)
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
  linemap_init (&line_table);
  init_ttree ();

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  /* Register the language-independent parameters.  */
  add_params (lang_independent_params, LAST_PARAM);

  /* This must be done after add_params but before argument processing.  */
  init_ggc_heuristics();
  init_tree_optimization_passes ();
}

/* Process the options that have been parsed.  */
static void
process_options (void)
{
  /* Allow the front end to perform consistency checks and do further
     initialization based on the command line options.  This hook also
     sets the original filename if appropriate (e.g. foo.i -> foo.c)
     so we can correctly initialize debug output.  */
  no_backend = lang_hooks.post_options (&main_input_filename);
  input_filename = main_input_filename;

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

  if (flag_short_enums == 2)
    flag_short_enums = targetm.default_short_enums ();

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

  if (flag_tree_based_profiling && flag_test_coverage)
    sorry ("test-coverage not yet implemented in trees.");
  if (flag_tree_based_profiling && flag_profile_values)
    sorry ("value-based profiling not yet implemented in trees.");

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
  if (PREFERRED_DEBUGGING_TYPE == NO_DEBUG)
    default_debug_hooks = &do_nothing_debug_hooks;
#if defined(DBX_DEBUGGING_INFO)
  else if (PREFERRED_DEBUGGING_TYPE == DBX_DEBUG)
    default_debug_hooks = &dbx_debug_hooks;
#endif
#if defined(XCOFF_DEBUGGING_INFO)
  else if (PREFERRED_DEBUGGING_TYPE == XCOFF_DEBUG)
    default_debug_hooks = &xcoff_debug_hooks;
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == SDB_DEBUG)
    default_debug_hooks = &sdb_debug_hooks;
#endif
#ifdef DWARF2_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == DWARF2_DEBUG)
    default_debug_hooks = &dwarf2_debug_hooks;
#endif
#ifdef VMS_DEBUGGING_INFO
  else if (PREFERRED_DEBUGGING_TYPE == VMS_DEBUG
	   || PREFERRED_DEBUGGING_TYPE == VMS_AND_DWARF2_DEBUG)
    default_debug_hooks = &vmsdbg_debug_hooks;
#endif

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

  /* Now we know which debug output will be used so we can set
     flag_var_tracking, flag_rename_registers and flag_web if the user has
     not specified them.  */
  if (debug_info_level < DINFO_LEVEL_NORMAL
      || debug_hooks->var_location == do_nothing_debug_hooks.var_location)
    {
      if (flag_var_tracking == 1)
        {
	  if (debug_info_level < DINFO_LEVEL_NORMAL)
	    warning ("variable tracking requested, but useless unless "
		     "producing debug info");
	  else
	    warning ("variable tracking requested, but not supported "
		     "by this debug format");
	}
      flag_var_tracking = 0;
    }

  if (flag_rename_registers == AUTODETECT_FLAG_VAR_TRACKING)
    flag_rename_registers = default_debug_hooks->var_location
	    		    != do_nothing_debug_hooks.var_location;

  if (flag_web == AUTODETECT_FLAG_VAR_TRACKING)
    flag_web = optimize >= 2 && (default_debug_hooks->var_location
	    		         != do_nothing_debug_hooks.var_location);

  if (flag_var_tracking == AUTODETECT_FLAG_VAR_TRACKING)
    flag_var_tracking = optimize >= 1;

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
  init_adjust_machine_modes ();

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
  if (lang_hooks.init () == 0)
    return 0;

  init_asm_output (name);

  /* These create various _DECL nodes, so need to be called after the
     front end is initialized.  */
  init_eh ();
  init_optabs ();
  init_optimization_passes ();

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

  finish_optimization_passes ();

  if (mem_report)
    {
      ggc_print_statistics ();
      stringpool_statistics ();
      dump_tree_statistics ();
      dump_rtx_statistics ();
      dump_varray_statistics ();
      dump_alloc_pool_statistics ();
      dump_ggc_loc_statistics ();
    }

  /* Free up memory for the benefit of leak detectors.  */
  free_reg_info ();

  /* Language-specific end of compilation actions.  */
  lang_hooks.finish ();
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
      /* Set up the back-end if requested.  */
      if (!no_backend)
	backend_init ();

      /* Language-dependent initialization.  Returns true on success.  */
      if (lang_dependent_init (main_input_filename))
	compile_file ();

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
