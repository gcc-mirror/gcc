/* Top level of GNU C compiler
   Copyright (C) 1987, 1988, 1989, 1992 Free Software Foundation, Inc.

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


/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include <sys/types.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include <sys/stat.h>

#ifdef USG
#undef FLOAT
#include <sys/param.h>
/* This is for hpux.  It is a real screw.  They should change hpux.  */
#undef FLOAT
#include <sys/times.h>
#include <time.h>   /* Correct for hpux at least.  Is it good on other USG?  */
#undef FFS  /* Some systems define this in param.h.  */
#else
#ifndef VMS
#include <sys/time.h>
#include <sys/resource.h>
#endif
#endif

#include "input.h"
#include "tree.h"
/* #include "c-tree.h" */
#include "rtl.h"
#include "flags.h"
#include "insn-attr.h"
#include "defaults.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

#ifdef VMS
/* The extra parameters substantially improve the I/O performance.  */
static FILE *
VMS_fopen (fname, type)
     char * fname;
     char * type;
{
  if (strcmp (type, "w") == 0)
    return fopen (fname, type, "mbc=16", "deq=64", "fop=tef", "shr=nil");
  return fopen (fname, type, "mbc=16");
}
#define fopen VMS_fopen
#endif

#ifndef DEFAULT_GDB_EXTENSIONS
#define DEFAULT_GDB_EXTENSIONS 1
#endif

extern int rtx_equal_function_value_matters;

#if ! (defined (VMS) || defined (OS2))
extern char **environ;
#endif
extern char *version_string, *language_string;

extern void init_lex ();
extern void init_decl_processing ();
extern void init_obstacks ();
extern void init_tree_codes ();
extern void init_rtl ();
extern void init_optabs ();
extern void init_stmt ();
extern void init_reg_sets ();
extern void dump_flow_info ();
extern void dump_sched_info ();
extern void dump_local_alloc ();

void rest_of_decl_compilation ();
void error ();
void error_with_file_and_line ();
void fancy_abort ();
#ifndef abort
void abort ();
#endif
void set_target_switch ();
static void print_switch_values ();
static char *decl_name ();

/* Name of program invoked, sans directories.  */

char *progname;

/* Copy of arguments to main.  */
int save_argc;
char **save_argv;

/* Name of current original source file (what was input to cpp).
   This comes from each #-command in the actual input.  */

char *input_filename;

/* Name of top-level original source file (what was input to cpp).
   This comes from the #-command at the beginning of the actual input.
   If there isn't any there, then this is the cc1 input file name.  */

char *main_input_filename;

/* Stream for reading from the input file.  */

FILE *finput;

/* Current line number in real source file.  */

int lineno;

/* Stack of currently pending input files.  */

struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
int input_file_stack_tick;

/* FUNCTION_DECL for function now being parsed or compiled.  */

extern tree current_function_decl;

/* Name to use as base of names for dump output files.  */

char *dump_base_name;

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm.h file
   and set by `-m...' switches.  Must be defined in rtlanal.c.  */

extern int target_flags;

/* Flags saying which kinds of debugging dump have been requested.  */

int rtl_dump = 0;
int rtl_dump_and_exit = 0;
int jump_opt_dump = 0;
int cse_dump = 0;
int loop_dump = 0;
int cse2_dump = 0;
int flow_dump = 0;
int combine_dump = 0;
int sched_dump = 0;
int local_reg_dump = 0;
int global_reg_dump = 0;
int sched2_dump = 0;
int jump2_opt_dump = 0;
int dbr_sched_dump = 0;
int flag_print_asm_name = 0;
int stack_reg_dump = 0;

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

/* Number of error messages and warning messages so far.  */

int errorcount = 0;
int warningcount = 0;
int sorrycount = 0;

/* Pointer to function to compute the name to use to print a declaration.  */

char *(*decl_printable_name) ();

/* Pointer to function to compute rtl for a language-specific tree code.  */

struct rtx_def *(*lang_expand_expr) ();

/* Pointer to function to finish handling an incomplete decl at the
   end of compilation.  */

void (*incomplete_decl_finalize_hook) () = 0;

/* Nonzero if generating code to do profiling.  */

int profile_flag = 0;

/* Nonzero if generating code to do profiling on a line-by-line basis.  */

int profile_block_flag;

/* Nonzero for -pedantic switch: warn about anything
   that standard spec forbids.  */

int pedantic = 0;

/* Temporarily suppress certain warnings.
   This is set while reading code from a system header file.  */

int in_system_header = 0;

/* Nonzero means do stupid register allocation.
   Currently, this is 1 if `optimize' is 0.  */

int obey_regdecls = 0;

/* Don't print functions as they are compiled and don't print
   times taken by the various passes.  -quiet.  */

int quiet_flag = 0;

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

/* Nonzero to inhibit use of define_optimization peephole opts.  */

int flag_no_peephole = 0;

/* Nonzero allows GCC to violate some IEEE or ANSI rules regarding math
   operations in the interest of optimization.  For example it allows
   GCC to assume arguments to sqrt are nonnegative numbers, allowing
   faster code for sqrt to be generated. */

int flag_fast_math = 0;

/* Nonzero means all references through pointers are volatile.  */

int flag_volatile;

/* Nonzero means treat all global and extern variables as global.  */

int flag_volatile_global;

/* Nonzero means just do syntax checking; don't output anything.  */

int flag_syntax_only = 0;

/* Nonzero means to rerun cse after loop optimization.  This increases
   compilation time about 20% and picks up a few more common expressions.  */

static int flag_rerun_cse_after_loop;

/* Nonzero for -finline-functions: ok to inline functions that look like
   good inline candidates.  */

int flag_inline_functions;

/* Nonzero for -fkeep-inline-functions: even if we make a function
   go inline everywhere, keep its definition around for debugging
   purposes.  */

int flag_keep_inline_functions;

/* Nonzero means that functions declared `inline' will be treated
   as `static'.  Prevents generation of zillions of copies of unused
   static inline functions; instead, `inlines' are written out
   only when actually used.  Used in conjunction with -g.  Also
   does the right thing with #pragma interface.  */

int flag_no_inline;

/* Nonzero means we should be saving declaration info into a .X file.  */

int flag_gen_aux_info = 0;

/* Specified name of aux-info file.  */

static char *aux_info_file_name;

/* Nonzero means make the text shared if supported.  */

int flag_shared_data;

/* Nonzero means schedule into delayed branch slots if supported.  */

int flag_delayed_branch;

/* Nonzero if we are compiling pure (sharable) code.
   Value is 1 if we are doing reasonable (i.e. simple
   offset into offset table) pic.  Value is 2 if we can
   only perform register offsets.  */

int flag_pic;

/* Nonzero means place uninitialized global data in the bss section. */

int flag_no_common;

/* Nonzero means pretend it is OK to examine bits of target floats,
   even if that isn't true.  The resulting code will have incorrect constants,
   but the same series of instructions that the native compiler would make.  */

int flag_pretend_float;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */

int flag_pedantic_errors = 0;

/* flag_schedule_insns means schedule insns within basic blocks (before
   local_alloc).
   flag_schedule_insns_after_reload means schedule insns after
   global_alloc.  */

int flag_schedule_insns = 0;
int flag_schedule_insns_after_reload = 0;

/* -finhibit-size-directive inhibits output of .size for ELF.
   This is used only for compiling crtstuff.c, 
   and it may be extended to other effects
   needed for crtstuff.c on other systems.  */
int flag_inhibit_size_directive = 0;

/* -fverbose-asm causes extra commentary information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).  */

int flag_verbose_asm = 0;

/* -fgnu-linker specifies use of the GNU linker for initializations.
   (Or, more generally, a linker that handles initializations.)
   -fno-gnu-linker says that collect2 will be used.  */
#ifdef USE_COLLECT2
int flag_gnu_linker = 0;
#else
int flag_gnu_linker = 1;
#endif

/* Table of language-independent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

struct { char *string; int *variable; int on_value;} f_options[] =
{
  {"float-store", &flag_float_store, 1},
  {"volatile", &flag_volatile, 1},
  {"volatile-global", &flag_volatile_global, 1},
  {"defer-pop", &flag_defer_pop, 1},
  {"omit-frame-pointer", &flag_omit_frame_pointer, 1},
  {"cse-follow-jumps", &flag_cse_follow_jumps, 1},
  {"cse-skip-blocks", &flag_cse_skip_blocks, 1},
  {"expensive-optimizations", &flag_expensive_optimizations, 1},
  {"thread-jumps", &flag_thread_jumps, 1},
  {"strength-reduce", &flag_strength_reduce, 1},
  {"unroll-loops", &flag_unroll_loops, 1},
  {"unroll-all-loops", &flag_unroll_all_loops, 1},
  {"writable-strings", &flag_writable_strings, 1},
  {"peephole", &flag_no_peephole, 0},
  {"force-mem", &flag_force_mem, 1},
  {"force-addr", &flag_force_addr, 1},
  {"function-cse", &flag_no_function_cse, 0},
  {"inline-functions", &flag_inline_functions, 1},
  {"keep-inline-functions", &flag_keep_inline_functions, 1},
  {"inline", &flag_no_inline, 0},
  {"syntax-only", &flag_syntax_only, 1},
  {"shared-data", &flag_shared_data, 1},
  {"caller-saves", &flag_caller_saves, 1},
  {"pcc-struct-return", &flag_pcc_struct_return, 1},
  {"reg-struct-return", &flag_pcc_struct_return, 0},
  {"delayed-branch", &flag_delayed_branch, 1},
  {"rerun-cse-after-loop", &flag_rerun_cse_after_loop, 1},
  {"pretend-float", &flag_pretend_float, 1},
  {"schedule-insns", &flag_schedule_insns, 1},
  {"schedule-insns2", &flag_schedule_insns_after_reload, 1},
  {"pic", &flag_pic, 1},
  {"PIC", &flag_pic, 2},
  {"fast-math", &flag_fast_math, 1},
  {"common", &flag_no_common, 0},
  {"inhibit-size-directive", &flag_inhibit_size_directive, 1},
  {"verbose-asm", &flag_verbose_asm, 1},
  {"gnu-linker", &flag_gnu_linker, 1}
};

/* Table of language-specific options.  */

char *lang_options[] =
{
  "-ftraditional",
  "-traditional",
  "-fnotraditional",
  "-fno-traditional",
  "-fsigned-char",
  "-funsigned-char",
  "-fno-signed-char",
  "-fno-unsigned-char",
  "-fsigned-bitfields",
  "-funsigned-bitfields",
  "-fno-signed-bitfields",
  "-fno-unsigned-bitfields",
  "-fshort-enums",
  "-fno-short-enums",
  "-fcond-mismatch",
  "-fno-cond-mismatch",
  "-fshort-double",
  "-fno-short-double",
  "-fasm",
  "-fno-asm",
  "-fbuiltin",
  "-fno-builtin",
  "-fno-ident",
  "-fident",
  "-ansi",
  "-Wimplicit",
  "-Wno-implicit",
  "-Wwrite-strings",
  "-Wno-write-strings",
  "-Wcast-qual",
  "-Wno-cast-qual",
  "-Wpointer-arith",
  "-Wno-pointer-arith",
  "-Wstrict-prototypes",
  "-Wno-strict-prototypes",
  "-Wmissing-prototypes",
  "-Wno-missing-prototypes",
  "-Wredundant-decls",
  "-Wno-redundant-decls",
  "-Wnested-externs",
  "-Wno-nested-externs",
  "-Wtraditional",
  "-Wno-traditional",
  "-Wformat",
  "-Wno-format",
  "-Wchar-subscripts",
  "-Wno-char-subscripts",
  "-Wconversion",
  "-Wno-conversion",
  "-Wparentheses",
  "-Wno-parentheses",
  "-Wcomment",
  "-Wno-comment",
  "-Wcomments",
  "-Wno-comments",
  "-Wtrigraphs",
  "-Wno-trigraphs",
  "-Wimport",
  "-Wno-import",
  "-Wmissing-braces",
  "-Wno-missing-braces",
  "-Wall",

  /* These are for C++.  */
  "-+e0",			/* gcc.c tacks the `-' on the front.  */
  "-+e1",
  "-+e2",
  "-fsave-memoized",
  "-fno-save-memoized",
  "-fcadillac",
  "-fno-cadillac",
  "-fgc",
  "-fno-gc",
  "-flabels-ok",
  "-fno-labels-ok",
  "-fstats",
  "-fno-stats",
  "-fthis-is-variable",
  "-fno-this-is-variable",
  "-fstrict-prototype",
  "-fno-strict-prototype",
  "-fall-virtual",
  "-fno-all-virtual",
  "-fmemoize-lookups",
  "-fno-memoize-lookups",
  "-felide-constructors",
  "-fno-elide-constructors",
  "-finline-debug",
  "-fno-inline-debug",
  "-fhandle-exceptions",
  "-fno-handle-exceptions",
  "-fansi-exceptions",
  "-fno-ansi-exceptions",
  "-fspring-exceptions",
  "-fno-spring-exceptions",
  "-fdefault-inline",
  "-fno-default-inline",
  "-fenum-int-equiv",
  "-fno-enum-int-equiv",
  "-fdossier",
  "-fno-dossier",
  "-fxref",
  "-fno-xref",
  "-fnonnull-objects",
  "-fno-nonnull-objects",
  "-fimplement-inlines",
  "-fno-implement-inlines",

  "-Wreturn-type",
  "-Wno-return-type",
  "-Woverloaded-virtual",
  "-Wno-overloaded-virtual",
  "-Wenum-clash",
  "-Wno-enum-clash",
  "-Wtemplate-debugging",
  "-Wno-template-debugging",

  /* these are for obj c */
  "-lang-objc",
  "-gen-decls",
  "-fgnu-runtime",
  "-fno-gnu-runtime",
  "-fnext-runtime",
  "-fno-next-runtime",
  "-Wselector",
  "-Wno-selector",
  "-Wprotocol",
  "-Wno-protocol",
  0
};

/* Options controlling warnings */

/* Don't print warning messages.  -w.  */

int inhibit_warnings = 0;

/* Print various extra warnings.  -W.  */

int extra_warnings = 0;

/* Treat warnings as errors.  -Werror.  */

int warnings_are_errors = 0;

/* Nonzero to warn about unused local variables.  */

int warn_unused;

/* Nonzero to warn about variables used before they are initialized.  */

int warn_uninitialized;

/* Nonzero means warn about all declarations which shadow others.   */

int warn_shadow;

/* Warn if a switch on an enum fails to have a case for every enum value.  */

int warn_switch;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

int warn_return_type;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */

int warn_cast_align;

/* Nonzero means warn about any identifiers that match in the first N
   characters.  The value N is in `id_clash_len'.  */

int warn_id_clash;
int id_clash_len;

/* Nonzero means warn if inline function is too large.  */

int warn_inline;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */

int warn_aggregate_return;

/* Likewise for -W.  */

struct { char *string; int *variable; int on_value;} W_options[] =
{
  {"unused", &warn_unused, 1},
  {"error", &warnings_are_errors, 1},
  {"shadow", &warn_shadow, 1},
  {"switch", &warn_switch, 1},
  {"aggregate-return", &warn_aggregate_return, 1},
  {"cast-align", &warn_cast_align, 1},
  {"uninitialized", &warn_uninitialized, 1},
  {"inline", &warn_inline, 1}
};

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *aux_info_file;
FILE *rtl_dump_file;
FILE *jump_opt_dump_file;
FILE *cse_dump_file;
FILE *loop_dump_file;
FILE *cse2_dump_file;
FILE *flow_dump_file;
FILE *combine_dump_file;
FILE *sched_dump_file;
FILE *local_reg_dump_file;
FILE *global_reg_dump_file;
FILE *sched2_dump_file;
FILE *jump2_opt_dump_file;
FILE *dbr_sched_dump_file;
FILE *stack_reg_dump_file;

/* Time accumulators, to count the total time spent in various passes.  */

int parse_time;
int varconst_time;
int integration_time;
int jump_time;
int cse_time;
int loop_time;
int cse2_time;
int flow_time;
int combine_time;
int sched_time;
int local_alloc_time;
int global_alloc_time;
int sched2_time;
int dbr_sched_time;
int shorten_branch_time;
int stack_reg_time;
int final_time;
int symout_time;
int dump_time;

/* Return time used so far, in microseconds.  */

int
get_run_time ()
{
#ifdef USG
  struct tms tms;
#else
#ifndef VMS
  struct rusage rusage;
#else /* VMS */
  struct
    {
      int proc_user_time;
      int proc_system_time;
      int child_user_time;
      int child_system_time;
    } vms_times;
#endif
#endif

  if (quiet_flag)
    return 0;

#ifdef USG
  times (&tms);
  return (tms.tms_utime + tms.tms_stime) * (1000000 / HZ);
#else
#ifndef VMS
  getrusage (0, &rusage);
  return (rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec
	  + rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec);
#else /* VMS */
  times (&vms_times);
  return (vms_times.proc_user_time + vms_times.proc_system_time) * 10000;
#endif
#endif
}

#define TIMEVAR(VAR, BODY)    \
do { int otime = get_run_time (); BODY; VAR += get_run_time () - otime; } while (0)

void
print_time (str, total)
     char *str;
     int total;
{
  fprintf (stderr,
	   "time in %s: %d.%06d\n",
	   str, total / 1000000, total % 1000000);
}

/* Count an error or warning.  Return 1 if the message should be printed.  */

int
count_error (warningp)
     int warningp;
{
  if (warningp && inhibit_warnings)
    return 0;

  if (warningp && !warnings_are_errors)
    warningcount++;
  else
    {
      static int warning_message = 0;

      if (warningp && !warning_message)
	{
	  fprintf (stderr, "%s: warnings being treated as errors\n", progname);
	  warning_message = 1;
	}
      errorcount++;
    }

  return 1;
}

/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */

void
pfatal_with_name (name)
     char *name;
{
  fprintf (stderr, "%s: ", progname);
  perror (name);
  exit (35);
}

void
fatal_io_error (name)
     char *name;
{
  fprintf (stderr, "%s: %s: I/O error\n", progname, name);
  exit (35);
}

void
fatal (s, v)
     char *s;
     int v;
{
  error (s, v);
  exit (34);
}

/* Called to give a better error message when we don't have an insn to match
   what we are looking for or if the insn's constraints aren't satisfied,
   rather than just calling abort().  */

void
fatal_insn_not_found (insn)
     rtx insn;
{
  if (INSN_CODE (insn) < 0)
    error ("internal error--unrecognizable insn:", 0);
  else
    error ("internal error--insn does not satisfy its constraints:", 0);
  debug_rtx (insn);
  if (asm_out_file)
    fflush (asm_out_file);
  if (aux_info_file)
    fflush (aux_info_file);
  if (rtl_dump_file)
    fflush (rtl_dump_file);
  if (jump_opt_dump_file)
    fflush (jump_opt_dump_file);
  if (cse_dump_file)
    fflush (cse_dump_file);
  if (loop_dump_file)
    fflush (loop_dump_file);
  if (cse2_dump_file)
    fflush (cse2_dump_file);
  if (flow_dump_file)
    fflush (flow_dump_file);
  if (combine_dump_file)
    fflush (combine_dump_file);
  if (sched_dump_file)
    fflush (sched_dump_file);
  if (local_reg_dump_file)
    fflush (local_reg_dump_file);
  if (global_reg_dump_file)
    fflush (global_reg_dump_file);
  if (sched2_dump_file)
    fflush (sched2_dump_file);
  if (jump2_opt_dump_file)
    fflush (jump2_opt_dump_file);
  if (dbr_sched_dump_file)
    fflush (dbr_sched_dump_file);
  if (stack_reg_dump_file)
    fflush (stack_reg_dump_file);
  abort ();
}

/* This is the default decl_printable_name function.  */

static char *
decl_name (decl, kind)
     tree decl;
     char **kind;
{
  return IDENTIFIER_POINTER (DECL_NAME (decl));
}

static int need_error_newline;

/* Function of last error message;
   more generally, function such that if next error message is in it
   then we don't have to mention the function name.  */
static tree last_error_function = NULL;

/* Used to detect when input_file_stack has changed since last described.  */
static int last_error_tick;

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */

void
announce_function (decl)
     tree decl;
{
  if (! quiet_flag)
    {
      char *junk;
      if (rtl_dump_and_exit)
	fprintf (stderr, "%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	fprintf (stderr, " %s", (*decl_printable_name) (decl, &junk));
      fflush (stderr);
      need_error_newline = 1;
      last_error_function = current_function_decl;
    }
}

/* Prints out, if necessary, the name of the current function
   which caused an error.  Called from all error and warning functions.  */

void
report_error_function (file)
     char *file;
{
  struct file_stack *p;

  if (need_error_newline)
    {
      fprintf (stderr, "\n");
      need_error_newline = 0;
    }

  if (last_error_function != current_function_decl)
    {
      char *kind = "function";
      if (current_function_decl != 0
	  && TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
	kind = "method";

      if (file)
	fprintf (stderr, "%s: ", file);

      if (current_function_decl == NULL)
	fprintf (stderr, "At top level:\n");
      else
	{
	  char *name = (*decl_printable_name) (current_function_decl, &kind);
	  fprintf (stderr, "In %s `%s':\n", kind, name);
	}

      last_error_function = current_function_decl;
    }
  if (input_file_stack && input_file_stack->next != 0
      && input_file_stack_tick != last_error_tick)
    {
      fprintf (stderr, "In file included");
      for (p = input_file_stack->next; p; p = p->next)
	{
	  fprintf (stderr, " from %s:%d", p->name, p->line);
	  if (p->next)
	    fprintf (stderr, ",");
	}
      fprintf (stderr, ":\n");
      last_error_tick = input_file_stack_tick;
    }
}

/* Report an error at the current line number.
   S is a string and V and V2 are args for `printf'.  We use HOST_WIDE_INT
   as the type for these args assuming it is wide enough to hold a
   pointer.  This isn't terribly portable, but is the best we can do
   without vprintf universally available.  */

void
error (s, v, v2)
     char *s;
     HOST_WIDE_INT v;		/* Also used as pointer */
     HOST_WIDE_INT v2;		/* Also used as pointer */
{
  error_with_file_and_line (input_filename, lineno, s, v, v2);
}

/* Report an error at line LINE of file FILE.
   S and V are a string and an arg for `printf'.  */

void
error_with_file_and_line (file, line, s, v, v2)
     char *file;
     int line;
     char *s;
     HOST_WIDE_INT v;
     HOST_WIDE_INT v2;
{
  count_error (0);

  report_error_function (file);

  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}

/* Report an error at the declaration DECL.
   S and V are a string and an arg which uses %s to substitute
   the declaration name.  */

void
error_with_decl (decl, s, v)
     tree decl;
     char *s;
     HOST_WIDE_INT v;
{
  char *junk;
  count_error (0);

  report_error_function (DECL_SOURCE_FILE (decl));

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  if (DECL_NAME (decl))
    fprintf (stderr, s, (*decl_printable_name) (decl, &junk), v);
  else
    fprintf (stderr, s, "((anonymous))", v);
  fprintf (stderr, "\n");
}

/* Report an error at the line number of the insn INSN.
   S and V are a string and an arg for `printf'.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

void
error_for_asm (insn, s, v, v2)
     rtx insn;
     char *s;
     HOST_WIDE_INT v;		/* Also used as pointer */
     HOST_WIDE_INT v2;		/* Also used as pointer */
{
  char *filename;
  int line;
  rtx body = PATTERN (insn);
  rtx asmop;

  /* Find the (or one of the) ASM_OPERANDS in the insn.  */
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    asmop = SET_SRC (body);
  else if (GET_CODE (body) == ASM_OPERANDS)
    asmop = body;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    asmop = SET_SRC (XVECEXP (body, 0, 0));
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    asmop = XVECEXP (body, 0, 0);

  filename = ASM_OPERANDS_SOURCE_FILE (asmop);
  line = ASM_OPERANDS_SOURCE_LINE (asmop);

  error_with_file_and_line (filename, line, s, v, v2);
}

/* Report a warning at line LINE.
   S and V are a string and an arg for `printf'.  */

void
warning_with_file_and_line (file, line, s, v, v2, v3)
     char *file;
     int line;
     char *s;
     HOST_WIDE_INT v, v2, v3;
{
  if (count_error (1) == 0)
    return;

  report_error_function (file);

  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, "warning: ");
  fprintf (stderr, s, v, v2, v3);
  fprintf (stderr, "\n");
}

/* Report a warning at the current line number.
   S and V are a string and an arg for `printf'.  */

void
warning (s, v, v2, v3)
     char *s;
     HOST_WIDE_INT v, v2, v3;	/* Also used as pointer */
{
  warning_with_file_and_line (input_filename, lineno, s, v, v2, v3);
}

/* Report a warning at the declaration DECL.
   S is string which uses %s to substitute the declaration name.
   V is a second parameter that S can refer to.  */

void
warning_with_decl (decl, s, v)
     tree decl;
     char *s;
     HOST_WIDE_INT v;
{
  char *junk;

  if (count_error (1) == 0)
    return;

  report_error_function (DECL_SOURCE_FILE (decl));

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  fprintf (stderr, "warning: ");
  if (DECL_NAME (decl))
    fprintf (stderr, s, (*decl_printable_name) (decl, &junk), v);
  else
    fprintf (stderr, s, "((anonymous))", v);
  fprintf (stderr, "\n");
}

/* Report a warning at the line number of the insn INSN.
   S and V are a string and an arg for `printf'.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

void
warning_for_asm (insn, s, v, v2)
     rtx insn;
     char *s;
     HOST_WIDE_INT v;		/* Also used as pointer */
     HOST_WIDE_INT v2;		/* Also used as pointer */
{
  char *filename;
  int line;
  rtx body = PATTERN (insn);
  rtx asmop;

  /* Find the (or one of the) ASM_OPERANDS in the insn.  */
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    asmop = SET_SRC (body);
  else if (GET_CODE (body) == ASM_OPERANDS)
    asmop = body;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    asmop = SET_SRC (XVECEXP (body, 0, 0));
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    asmop = XVECEXP (body, 0, 0);

  filename = ASM_OPERANDS_SOURCE_FILE (asmop);
  line = ASM_OPERANDS_SOURCE_LINE (asmop);

  warning_with_file_and_line (filename, line, s, v, v2);
}

/* These functions issue either warnings or errors depending on
   -pedantic-errors.  */

void
pedwarn (s, v, v2)
     char *s;
     HOST_WIDE_INT v;		/* Also used as pointer */
     HOST_WIDE_INT v2;
{
  if (flag_pedantic_errors)
    error (s, v, v2);
  else
    warning (s, v, v2);
}

void
pedwarn_with_decl (decl, s, v)
     tree decl;
     char *s;
     HOST_WIDE_INT v;
{
  if (flag_pedantic_errors)
    error_with_decl (decl, s, v);
  else
    warning_with_decl (decl, s, v);
}

void
pedwarn_with_file_and_line (file, line, s, v, v2)
     char *file;
     int line;
     char *s;
     HOST_WIDE_INT v;
     HOST_WIDE_INT v2;
{
  if (flag_pedantic_errors)
    error_with_file_and_line (file, line, s, v, v2);
  else
    warning_with_file_and_line (file, line, s, v, v2);
}

/* Apologize for not implementing some feature.
   S, V, and V2 are a string and args for `printf'.  */

void
sorry (s, v, v2)
     char *s;
     HOST_WIDE_INT v, v2;
{
  sorrycount++;
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, "sorry, not implemented: ");
  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}

/* Apologize for not implementing some feature, then quit.
   S, V, and V2 are a string and args for `printf'.  */

void
really_sorry (s, v, v2)
     char *s;
     HOST_WIDE_INT v, v2;
{
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, "sorry, not implemented: ");
  fprintf (stderr, s, v, v2);
  fatal (" (fatal)\n");
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.

   I don't think this is actually a good idea.
   Other sorts of crashes will look a certain way.
   It is a good thing if crashes from calling abort look the same way.
     -- RMS  */

void
fancy_abort ()
{
  fatal ("internal gcc abort");
}

/* This calls abort and is used to avoid problems when abort if a macro.
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
{
  abort ();
}

/* Same as `malloc' but report error if no memory available.  */

char *
xmalloc (size)
     unsigned size;
{
  register char *value = (char *) malloc (size);
  if (value == 0)
    fatal ("virtual memory exhausted");
  return value;
}

/* Same as `realloc' but report error if no memory available.  */

char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  char *result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.

   This should be used via the `exact_log2' macro.  */

int
exact_log2_wide (x)
     register unsigned HOST_WIDE_INT x;
{
  register int log = 0;
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
     register unsigned HOST_WIDE_INT x;
{
  register int log = -1;
  while (x != 0)
    log++,
    x >>= 1;
  return log;
}

int float_handled;
jmp_buf float_handler;

/* Specify where to longjmp to when a floating arithmetic error happens.
   If HANDLER is 0, it means don't handle the errors any more.  */

void
set_float_handler (handler)
     jmp_buf handler;
{
  float_handled = (handler != 0);
  if (handler)
    bcopy (handler, float_handler, sizeof (float_handler));
}

/* Specify, in HANDLER, where to longjmp to when a floating arithmetic
   error happens, pushing the previous specification into OLD_HANDLER.
   Return an indication of whether there was a previous handler in effect.  */

int
push_float_handler (handler, old_handler)
     jmp_buf handler, old_handler;
{
  int was_handled = float_handled;

  float_handled = 1;
  if (was_handled)
    bcopy (float_handler, old_handler, sizeof (float_handler));
  bcopy (handler, float_handler, sizeof (float_handler));
  return was_handled;
}

/* Restore the previous specification of whether and where to longjmp to
   when a floating arithmetic error happens.  */

void
pop_float_handler (handled, handler)
     int handled;
     jmp_buf handler;
{
  float_handled = handled;
  if (handled)
    bcopy (handler, float_handler, sizeof (float_handler));
}

/* Signals actually come here.  */

static void
float_signal (signo)
     /* If this is missing, some compilers complain.  */
     int signo;
{
  if (float_handled == 0)
    abort ();
#if defined (USG) || defined (hpux)
  signal (SIGFPE, float_signal);  /* re-enable the signal catcher */
#endif
  float_handled = 0;
  signal (SIGFPE, float_signal);
  longjmp (float_handler, 1);
}

/* Handler for SIGPIPE.  */

static void
pipe_closed (signo)
     /* If this is missing, some compilers complain.  */
     int signo;
{
  fatal ("output pipe has been closed");
}

/* Strip off a legitimate source ending from the input string NAME of
   length LEN. */

void
strip_off_ending (name, len)
     char *name;
     int len;
{
  if (len > 2 && ! strcmp (".c", name + len - 2))
    name[len - 2] = 0;
  else if (len > 2 && ! strcmp (".m", name + len - 2))
    name[len - 2] = 0;
  else if (len > 2 && ! strcmp (".i", name + len - 2))
    name[len - 2] = 0;
  else if (len > 3 && ! strcmp (".ii", name + len - 3))
    name[len - 3] = 0;
  else if (len > 3 && ! strcmp (".co", name + len - 3))
    name[len - 3] = 0;
  else if (len > 3 && ! strcmp (".cc", name + len - 3))
    name[len - 3] = 0;
  else if (len > 2 && ! strcmp (".C", name + len - 2))
    name[len - 2] = 0;
  else if (len > 4 && ! strcmp (".cxx", name + len - 4))
    name[len - 4] = 0;
  else if (len > 2 && ! strcmp (".f", name + len - 2))
    name[len - 2] = 0;
  else if (len > 4 && ! strcmp (".ada", name + len - 4))
    name[len - 4] = 0;
  else if (len > 4 && ! strcmp (".atr", name + len - 4))
    name[len - 4] = 0;
}

/* Output a file name in the form wanted by System V.  */

void
output_file_directive (asm_file, input_name)
     FILE *asm_file;
     char *input_name;
{
  int len = strlen (input_name);
  char *na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (na[-1] == '/')
	break;
      na--;
    }

#ifdef ASM_OUTPUT_MAIN_SOURCE_FILENAME
  ASM_OUTPUT_MAIN_SOURCE_FILENAME (asm_file, na);
#else
#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
#else
  fprintf (asm_file, "\t.file\t\"%s\"\n", na);
#endif
#endif
}

/* Routine to build language identifier for object file. */
static void
output_lang_identify (asm_out_file)
     FILE *asm_out_file;
{
  int len = strlen (lang_identify ()) + sizeof ("__gnu_compiled_") + 1;
  char *s = (char *) alloca (len);
  sprintf (s, "__gnu_compiled_%s", lang_identify ());
  ASM_OUTPUT_LABEL (asm_out_file, s);
}

/* Compile an entire file of output from cpp, named NAME.
   Write a file of assembly output and various debugging dumps.  */

static void
compile_file (name)
     char *name;
{
  tree globals;
  int start_time;
  int dump_base_name_length;

  int name_specified = name != 0;

  if (dump_base_name == 0)
    dump_base_name = name ? name : "gccdump";
  dump_base_name_length = strlen (dump_base_name);

  parse_time = 0;
  varconst_time = 0;
  integration_time = 0;
  jump_time = 0;
  cse_time = 0;
  loop_time = 0;
  cse2_time = 0;
  flow_time = 0;
  combine_time = 0;
  sched_time = 0;
  local_alloc_time = 0;
  global_alloc_time = 0;
  sched2_time = 0;
  dbr_sched_time = 0;
  shorten_branch_time = 0;
  stack_reg_time = 0;
  final_time = 0;
  symout_time = 0;
  dump_time = 0;

  /* Open input file.  */

  if (name == 0 || !strcmp (name, "-"))
    {
      finput = stdin;
      name = "stdin";
    }
  else
    finput = fopen (name, "r");
  if (finput == 0)
    pfatal_with_name (name);

#ifdef IO_BUFFER_SIZE
  setvbuf (finput, (char *) xmalloc (IO_BUFFER_SIZE), _IOFBF, IO_BUFFER_SIZE);
#endif

  /* Initialize data in various passes.  */

  init_obstacks ();
  init_tree_codes ();
  init_lex ();
  init_rtl ();
  init_emit_once (debug_info_level == DINFO_LEVEL_NORMAL
		  || debug_info_level == DINFO_LEVEL_VERBOSE);
  init_decl_processing ();
  init_optabs ();
  init_stmt ();
  init_expmed ();
  init_expr_once ();
  init_loop ();
  init_reload ();

  if (flag_caller_saves)
    init_caller_save ();

  /* If auxiliary info generation is desired, open the output file.
     This goes in the same directory as the source file--unlike
     all the other output files.  */
  if (flag_gen_aux_info)
    {
      aux_info_file = fopen (aux_info_file_name, "w");
      if (aux_info_file == 0)
	pfatal_with_name (aux_info_file_name);
    }

  /* If rtl dump desired, open the output file.  */
  if (rtl_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".rtl");
      rtl_dump_file = fopen (dumpname, "w");
      if (rtl_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If jump_opt dump desired, open the output file.  */
  if (jump_opt_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".jump");
      jump_opt_dump_file = fopen (dumpname, "w");
      if (jump_opt_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If cse dump desired, open the output file.  */
  if (cse_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".cse");
      cse_dump_file = fopen (dumpname, "w");
      if (cse_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If loop dump desired, open the output file.  */
  if (loop_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".loop");
      loop_dump_file = fopen (dumpname, "w");
      if (loop_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If cse2 dump desired, open the output file.  */
  if (cse2_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".cse2");
      cse2_dump_file = fopen (dumpname, "w");
      if (cse2_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If flow dump desired, open the output file.  */
  if (flow_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".flow");
      flow_dump_file = fopen (dumpname, "w");
      if (flow_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If combine dump desired, open the output file.  */
  if (combine_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 10);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".combine");
      combine_dump_file = fopen (dumpname, "w");
      if (combine_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If scheduling dump desired, open the output file.  */
  if (sched_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".sched");
      sched_dump_file = fopen (dumpname, "w");
      if (sched_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If local_reg dump desired, open the output file.  */
  if (local_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".lreg");
      local_reg_dump_file = fopen (dumpname, "w");
      if (local_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If global_reg dump desired, open the output file.  */
  if (global_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".greg");
      global_reg_dump_file = fopen (dumpname, "w");
      if (global_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If 2nd scheduling dump desired, open the output file.  */
  if (sched2_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 8);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".sched2");
      sched2_dump_file = fopen (dumpname, "w");
      if (sched2_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If jump2_opt dump desired, open the output file.  */
  if (jump2_opt_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".jump2");
      jump2_opt_dump_file = fopen (dumpname, "w");
      if (jump2_opt_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If dbr_sched dump desired, open the output file.  */
  if (dbr_sched_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".dbr");
      dbr_sched_dump_file = fopen (dumpname, "w");
      if (dbr_sched_dump_file == 0)
	pfatal_with_name (dumpname);
    }

#ifdef STACK_REGS

  /* If stack_reg dump desired, open the output file.  */
  if (stack_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 10);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".stack");
      stack_reg_dump_file = fopen (dumpname, "w");
      if (stack_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

#endif

  /* Open assembler code output file.  */

  if (! name_specified && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      int len = strlen (dump_base_name);
      strcpy (dumpname, dump_base_name);
      strip_off_ending (dumpname, len);
      strcat (dumpname, ".s");
      if (asm_file_name == 0)
	{
	  asm_file_name = (char *) xmalloc (strlen (dumpname) + 1);
	  strcpy (asm_file_name, dumpname);
	}
      if (!strcmp (asm_file_name, "-"))
	asm_out_file = stdout;
      else
	asm_out_file = fopen (asm_file_name, "w");
      if (asm_out_file == 0)
	pfatal_with_name (asm_file_name);
    }

#ifdef IO_BUFFER_SIZE
  setvbuf (asm_out_file, (char *) xmalloc (IO_BUFFER_SIZE),
	   _IOFBF, IO_BUFFER_SIZE);
#endif

  input_filename = name;

  /* Perform language-specific initialization.
     This may set main_input_filename.  */
  lang_init ();

  /* If the input doesn't start with a #line, use the input name
     as the official input file name.  */
  if (main_input_filename == 0)
    main_input_filename = name;

  /* Put an entry on the input file stack for the main input file.  */
  input_file_stack
    = (struct file_stack *) xmalloc (sizeof (struct file_stack));
  input_file_stack->next = 0;
  input_file_stack->name = input_filename;

  ASM_FILE_START (asm_out_file);

  /* Output something to inform GDB that this compilation was by GCC.  */
#ifndef ASM_IDENTIFY_GCC
  fprintf (asm_out_file, "gcc2_compiled.:\n");
#else
  ASM_IDENTIFY_GCC (asm_out_file);
#endif

  /* Output something to identify which front-end produced this file. */
#ifdef ASM_IDENTIFY_LANGUAGE
  ASM_IDENTIFY_LANGUAGE (asm_out_file);
#endif

/* ??? Note: There used to be a conditional here
   to call assemble_zeros without fail if DBX_DEBUGGING_INFO is defined.
   This was to guarantee separation between gcc_compiled. and
   the first function, for the sake of dbx on Suns.
   However, having the extra zero here confused the Emacs
   code for unexec, and might confuse other programs too.
   Therefore, I took out that change.
   In future versions we should find another way to solve
   that dbx problem.  -- rms, 23 May 93.  */

  /* Don't let the first function fall at the same address
     as gcc_compiled., if profiling.  */
  if (profile_flag || profile_block_flag)
    assemble_zeros (UNITS_PER_WORD);

  /* If dbx symbol table desired, initialize writing it
     and output the predefined types.  */
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
    TIMEVAR (symout_time, dbxout_init (asm_out_file, main_input_filename,
				       getdecls ()));
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    TIMEVAR (symout_time, sdbout_init (asm_out_file, main_input_filename,
				       getdecls ()));
#endif
#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    TIMEVAR (symout_time, dwarfout_init (asm_out_file, main_input_filename));
#endif

  /* Initialize yet another pass.  */

  init_final (main_input_filename);

  start_time = get_run_time ();

  /* Call the parser, which parses the entire file
     (calling rest_of_compilation for each function).  */

  if (yyparse () != 0)
    if (errorcount == 0)
      fprintf (stderr, "Errors detected in input file (your bison.simple is out of date)");

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */

  parse_time += get_run_time () - start_time;

  parse_time -= integration_time;
  parse_time -= varconst_time;

  globals = getdecls ();

  /* Really define vars that have had only a tentative definition.
     Really output inline functions that must actually be callable
     and have not been output so far.  */

  {
    int len = list_length (globals);
    tree *vec = (tree *) alloca (sizeof (tree) * len);
    int i;
    tree decl;

    /* Process the decls in reverse order--earliest first.
       Put them into VEC from back to front, then take out from front.  */

    for (i = 0, decl = globals; i < len; i++, decl = TREE_CHAIN (decl))
      vec[len - i - 1] = decl;

    for (i = 0; i < len; i++)
      {
	decl = vec[i];
	if (TREE_CODE (decl) == VAR_DECL && DECL_SIZE (decl) == 0
	    && incomplete_decl_finalize_hook != 0)
	  (*incomplete_decl_finalize_hook) (decl);

	if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl)
	    && ! TREE_ASM_WRITTEN (decl))
	  {
	    /* Don't write out static consts, unless we used them.
	       (This used to write them out only if the address was
	       taken, but that was wrong; if the variable was simply
	       referred to, it still needs to exist or else it will
	       be undefined in the linker.)  */
	    if (! TREE_READONLY (decl)
		|| TREE_PUBLIC (decl)
		|| TREE_USED (decl)
		|| TREE_ADDRESSABLE (decl)
		|| TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (decl)))
	      rest_of_decl_compilation (decl, NULL_PTR, 1, 1);
	    else
	      /* Cancel the RTL for this decl so that, if debugging info
		 output for global variables is still to come,
		 this one will be omitted.  */
	      DECL_RTL (decl) = NULL;
	  }

	if (TREE_CODE (decl) == FUNCTION_DECL
	    && ! TREE_ASM_WRITTEN (decl)
	    && DECL_INITIAL (decl) != 0
	    && (TREE_ADDRESSABLE (decl)
		|| TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (decl)))
	    && ! DECL_EXTERNAL (decl))
	  output_inline_function (decl);

	/* Warn about any function
	   declared static but not defined.
	   We don't warn about variables,
	   because many programs have static variables
	   that exist only to get some text into the object file.  */
	if ((warn_unused
	     || TREE_USED (decl)
	     || (DECL_NAME (decl) && TREE_USED (DECL_NAME (decl))))
	    && TREE_CODE (decl) == FUNCTION_DECL
	    && DECL_INITIAL (decl) == 0
	    && DECL_EXTERNAL (decl)
	    && ! TREE_PUBLIC (decl))
	  {
	    pedwarn_with_decl (decl, 
			       "`%s' declared `static' but never defined");
	    /* This symbol is effectively an "extern" declaration now.  */
	    TREE_PUBLIC (decl) = 1;
	    assemble_external (decl);

	  }
	/* Warn about static fns or vars defined but not used,
	   but not about inline functions
	   since unused inline statics is normal practice.  */
	if (warn_unused
	    && (TREE_CODE (decl) == FUNCTION_DECL
		|| TREE_CODE (decl) == VAR_DECL)
	    && ! DECL_IN_SYSTEM_HEADER (decl)
	    && ! DECL_EXTERNAL (decl)
	    && ! TREE_PUBLIC (decl)
	    && ! TREE_USED (decl)
	    && ! DECL_INLINE (decl)
	    && ! DECL_REGISTER (decl)
	    /* The TREE_USED bit for file-scope decls
	       is kept in the identifier, to handle multiple
	       external decls in different scopes.  */
	    && ! TREE_USED (DECL_NAME (decl)))
	  warning_with_decl (decl, "`%s' defined but not used");

#ifdef SDB_DEBUGGING_INFO
	/* The COFF linker can move initialized global vars to the end.
	   And that can screw up the symbol ordering.
	   By putting the symbols in that order to begin with,
	   we avoid a problem.  mcsun!unido!fauern!tumuc!pes@uunet.uu.net.  */
	if (write_symbols == SDB_DEBUG && TREE_CODE (decl) == VAR_DECL
	    && TREE_PUBLIC (decl) && DECL_INITIAL (decl)
	    && DECL_RTL (decl) != 0)
	  TIMEVAR (symout_time, sdbout_symbol (decl, 0));

	/* Output COFF information for non-global
	   file-scope initialized variables. */
	if (write_symbols == SDB_DEBUG
	    && TREE_CODE (decl) == VAR_DECL
	    && DECL_INITIAL (decl)
	    && DECL_RTL (decl) != 0
	    && GET_CODE (DECL_RTL (decl)) == MEM)
	  TIMEVAR (symout_time, sdbout_toplevel_data (decl));
#endif /* SDB_DEBUGGING_INFO */
#ifdef DWARF_DEBUGGING_INFO
	/* Output DWARF information for file-scope tentative data object
	   declarations, file-scope (extern) function declarations (which
	   had no corresponding body) and file-scope tagged type declarations
	   and definitions which have not yet been forced out.  */

	if (write_symbols == DWARF_DEBUG
	    && (TREE_CODE (decl) != FUNCTION_DECL || !DECL_INITIAL (decl)))
	  TIMEVAR (symout_time, dwarfout_file_scope_decl (decl, 1));
#endif
      }
  }

  /* Do dbx symbols */
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
    TIMEVAR (symout_time,
	     {
	       dbxout_finish (asm_out_file, main_input_filename);
	     });
#endif

#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    TIMEVAR (symout_time,
	     {
	       dwarfout_finish ();
	     });
#endif

  /* Output some stuff at end of file if nec.  */

  end_final (main_input_filename);

#ifdef ASM_FILE_END
  ASM_FILE_END (asm_out_file);
#endif

 after_finish_compilation:

  /* Language-specific end of compilation actions.  */

  lang_finish ();

  /* Close the dump files.  */

  if (flag_gen_aux_info)
    {
      fclose (aux_info_file);
      if (errorcount)
	unlink (aux_info_file_name);
    }

  if (rtl_dump)
    fclose (rtl_dump_file);

  if (jump_opt_dump)
    fclose (jump_opt_dump_file);

  if (cse_dump)
    fclose (cse_dump_file);

  if (loop_dump)
    fclose (loop_dump_file);

  if (cse2_dump)
    fclose (cse2_dump_file);

  if (flow_dump)
    fclose (flow_dump_file);

  if (combine_dump)
    {
      dump_combine_total_stats (combine_dump_file);
      fclose (combine_dump_file);
    }

  if (sched_dump)
    fclose (sched_dump_file);

  if (local_reg_dump)
    fclose (local_reg_dump_file);

  if (global_reg_dump)
    fclose (global_reg_dump_file);

  if (sched2_dump)
    fclose (sched2_dump_file);

  if (jump2_opt_dump)
    fclose (jump2_opt_dump_file);

  if (dbr_sched_dump)
    fclose (dbr_sched_dump_file);

#ifdef STACK_REGS
  if (stack_reg_dump)
    fclose (stack_reg_dump_file);
#endif

  /* Close non-debugging input and output files.  Take special care to note
     whether fclose returns an error, since the pages might still be on the
     buffer chain while the file is open.  */

  fclose (finput);
  if (ferror (asm_out_file) != 0 || fclose (asm_out_file) != 0)
    fatal_io_error (asm_file_name);

  /* Print the times.  */

  if (! quiet_flag)
    {
      fprintf (stderr,"\n");
      print_time ("parse", parse_time);
      print_time ("integration", integration_time);
      print_time ("jump", jump_time);
      print_time ("cse", cse_time);
      print_time ("loop", loop_time);
      print_time ("cse2", cse2_time);
      print_time ("flow", flow_time);
      print_time ("combine", combine_time);
      print_time ("sched", sched_time);
      print_time ("local-alloc", local_alloc_time);
      print_time ("global-alloc", global_alloc_time);
      print_time ("sched2", sched2_time);
      print_time ("dbranch", dbr_sched_time);
      print_time ("shorten-branch", shorten_branch_time);
      print_time ("stack-reg", stack_reg_time);
      print_time ("final", final_time);
      print_time ("varconst", varconst_time);
      print_time ("symout", symout_time);
      print_time ("dump", dump_time);
    }
}

/* This is called from various places for FUNCTION_DECL, VAR_DECL,
   and TYPE_DECL nodes.

   This does nothing for local (non-static) variables.
   Otherwise, it sets up the RTL and outputs any assembler code
   (label definition, storage allocation and initialization).

   DECL is the declaration.  If ASMSPEC is nonzero, it specifies
   the assembler symbol name to be used.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (decl, asmspec, top_level, at_end)
     tree decl;
     char *asmspec;
     int top_level;
     int at_end;
{
  /* Declarations of variables, and of functions defined elsewhere.  */

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    TIMEVAR (varconst_time,
	     {
	       make_decl_rtl (decl, asmspec, top_level);
	       /* For a user-invisible decl that should be replaced
		  by its value when used, don't output anything.  */
	       if (! (TREE_CODE (decl) == VAR_DECL
		      && DECL_IGNORED_P (decl) && TREE_READONLY (decl)
		      && DECL_INITIAL (decl) != 0))
		 /* Don't output anything
		    when a tentative file-scope definition is seen.
		    But at end of compilation, do output code for them.  */
		 if (! (! at_end && top_level
			&& (DECL_INITIAL (decl) == 0
			    || DECL_INITIAL (decl) == error_mark_node
			    || DECL_IGNORED_P (decl))))
		   assemble_variable (decl, top_level, at_end);
	     });
  else if (DECL_REGISTER (decl) && asmspec != 0)
    {
      if (decode_reg_name (asmspec) >= 0)
	{
	  DECL_RTL (decl) = 0;
	  make_decl_rtl (decl, asmspec, top_level);
	}
      else
	error ("invalid register name `%s' for register variable", asmspec);
    }
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  else if ((write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	   && TREE_CODE (decl) == TYPE_DECL)
    TIMEVAR (symout_time, dbxout_symbol (decl, 0));
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (write_symbols == SDB_DEBUG && top_level
	   && TREE_CODE (decl) == TYPE_DECL)
    TIMEVAR (symout_time, sdbout_symbol (decl, 0));
#endif
}

/* Called after finishing a record, union or enumeral type.  */

void
rest_of_type_compilation (type, toplev)
     tree type;
     int toplev;
{
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
  if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
    TIMEVAR (symout_time, dbxout_symbol (TYPE_STUB_DECL (type), !toplev));
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    TIMEVAR (symout_time, sdbout_symbol (TYPE_STUB_DECL (type), !toplev));
#endif
}

/* This is called from finish_function (within yyparse)
   after each top-level definition is parsed.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (decl)
     tree decl;
{
  register rtx insns;
  int start_time = get_run_time ();
  int tem;
  /* Nonzero if we have saved the original DECL_INITIAL of the function,
     to be restored after we finish compiling the function
     (for use when compiling inline calls to this function).  */
  tree saved_block_tree = 0;
  /* Likewise, for DECL_ARGUMENTS.  */
  tree saved_arguments = 0;
  int failure = 0;

  /* If we are reconsidering an inline function
     at the end of compilation, skip the stuff for making it inline.  */

  if (DECL_SAVED_INSNS (decl) == 0)
    {
      int specd = DECL_INLINE (decl);
      char *lose;

      /* If requested, consider whether to make this function inline.  */
      if (specd || flag_inline_functions)
	TIMEVAR (integration_time,
		 {
		   lose = function_cannot_inline_p (decl);
		   if (lose)
		     {
		       if (warn_inline && specd)
			 warning_with_decl (decl, lose);
		       DECL_INLINE (decl) = 0;
		     }
		   else
		     DECL_INLINE (decl) = 1;
		 });

      insns = get_insns ();

      /* Dump the rtl code if we are dumping rtl.  */

      if (rtl_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (rtl_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   if (DECL_SAVED_INSNS (decl))
		     fprintf (rtl_dump_file, ";; (integrable)\n\n");
		   print_rtl (rtl_dump_file, insns);
		   fflush (rtl_dump_file);
		 });

      /* If function is inline, and we don't yet know whether to
	 compile it by itself, defer decision till end of compilation.
	 finish_compilation will call rest_of_compilation again
	 for those functions that need to be output.  */

      if (DECL_INLINE (decl)
	  && ((! TREE_PUBLIC (decl) && ! TREE_ADDRESSABLE (decl)
	       && ! flag_keep_inline_functions)
	      || DECL_EXTERNAL (decl)))
	{
#ifdef DWARF_DEBUGGING_INFO
	  /* Generate the DWARF info for the "abstract" instance
	     of a function which we may later generate inlined and/or
	     out-of-line instances of.  */
	  if (write_symbols == DWARF_DEBUG)
	    {
	      set_decl_abstract_flags (decl, 1);
	      TIMEVAR (symout_time, dwarfout_file_scope_decl (decl, 0));
	      set_decl_abstract_flags (decl, 0);
	    }
#endif
	  TIMEVAR (integration_time, save_for_inline_nocopy (decl));
	  goto exit_rest_of_compilation;
	}

      /* If we have to compile the function now, save its rtl and subdecls
	 so that its compilation will not affect what others get.  */
      if (DECL_INLINE (decl))
	{
#ifdef DWARF_DEBUGGING_INFO
	  /* Generate the DWARF info for the "abstract" instance of
	     a function which we will generate an out-of-line instance
	     of almost immediately (and which we may also later generate
	     various inlined instances of).  */
	  if (write_symbols == DWARF_DEBUG)
	    {
	      set_decl_abstract_flags (decl, 1);
	      TIMEVAR (symout_time, dwarfout_file_scope_decl (decl, 0));
	      set_decl_abstract_flags (decl, 0);
	    }
#endif
	  saved_block_tree = DECL_INITIAL (decl);
	  saved_arguments = DECL_ARGUMENTS (decl);
	  TIMEVAR (integration_time, save_for_inline_copying (decl));
	}
    }

  TREE_ASM_WRITTEN (decl) = 1;

  /* Now that integrate will no longer see our rtl, we need not distinguish
     between the return value of this function and the return value of called
     functions.  */
  rtx_equal_function_value_matters = 0;

  /* Don't return yet if -Wreturn-type; we need to do jump_optimize.  */
  if ((rtl_dump_and_exit || flag_syntax_only) && !warn_return_type)
    {
      goto exit_rest_of_compilation;
    }

  /* From now on, allocate rtl in current_obstack, not in saveable_obstack.
     Note that that may have been done above, in save_for_inline_copying.
     The call to resume_temporary_allocation near the end of this function
     goes back to the usual state of affairs.  */

  rtl_in_current_obstack ();

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

  unshare_all_rtl (insns);

  /* Instantiate all virtual registers.  */

  instantiate_virtual_regs (current_function_decl, get_insns ());

  /* See if we have allocated stack slots that are not directly addressable.
     If so, scan all the insns and create explicit address computation
     for all references to such slots.  */
/*   fixup_stack_slots (); */

  /* Do jump optimization the first time, if -opt.
     Also do it if -W, but in that case it doesn't change the rtl code,
     it only computes whether control can drop off the end of the function.  */

  if (optimize > 0 || extra_warnings || warn_return_type
      /* If function is `volatile', we should warn if it tries to return.  */
      || TREE_THIS_VOLATILE (decl))
    {
      TIMEVAR (jump_time, reg_scan (insns, max_reg_num (), 0));
      TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 1));
    }

  /* Now is when we stop if -fsyntax-only and -Wreturn-type.  */
  if (rtl_dump_and_exit || flag_syntax_only)
    goto exit_rest_of_compilation;

  /* Dump rtl code after jump, if we are doing that.  */

  if (jump_opt_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (jump_opt_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       print_rtl (jump_opt_dump_file, insns);
	       fflush (jump_opt_dump_file);
	     });

  /* Perform common subexpression elimination.
     Nonzero value from `cse_main' means that jumps were simplified
     and some code may now be unreachable, so do
     jump optimization again.  */

  if (cse_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (cse_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  if (optimize > 0)
    {
      TIMEVAR (cse_time, reg_scan (insns, max_reg_num (), 1));

      if (flag_thread_jumps)
	/* Hacks by tiemann & kenner.  */
	TIMEVAR (jump_time, thread_jumps (insns, max_reg_num (), 0));

      TIMEVAR (cse_time, tem = cse_main (insns, max_reg_num (),
					 0, cse_dump_file));
      TIMEVAR (cse_time, delete_dead_from_cse (insns, max_reg_num ()));

      if (tem)
	TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 0));
    }

  /* Dump rtl code after cse, if we are doing that.  */

  if (cse_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (cse_dump_file, insns);
	       fflush (cse_dump_file);
	     });

  if (loop_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (loop_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  /* Move constant computations out of loops.  */

  if (optimize > 0)
    {
      TIMEVAR (loop_time,
	       {
		 loop_optimize (insns, loop_dump_file);
	       });
    }

  /* Dump rtl code after loop opt, if we are doing that.  */

  if (loop_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (loop_dump_file, insns);
	       fflush (loop_dump_file);
	     });

  if (cse2_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (cse2_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  if (optimize > 0 && flag_rerun_cse_after_loop)
    {
      TIMEVAR (cse2_time, reg_scan (insns, max_reg_num (), 0));

      TIMEVAR (cse2_time, tem = cse_main (insns, max_reg_num (),
					  1, cse2_dump_file));
      if (tem)
	TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 0));
    }

  if (optimize > 0 && flag_thread_jumps)
    /* This pass of jump threading straightens out code
       that was kinked by loop optimization.  */
    TIMEVAR (jump_time, thread_jumps (insns, max_reg_num (), 0));

  /* Dump rtl code after cse, if we are doing that.  */

  if (cse2_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (cse2_dump_file, insns);
	       fflush (cse2_dump_file);
	     });

  /* We are no longer anticipating cse in this function, at least.  */

  cse_not_expected = 1;

  /* Now we choose between stupid (pcc-like) register allocation
     (if we got the -noreg switch and not -opt)
     and smart register allocation.  */

  if (optimize > 0)			/* Stupid allocation probably won't work */
    obey_regdecls = 0;		/* if optimizations being done.  */

  regclass_init ();

  /* Print function header into flow dump now
     because doing the flow analysis makes some of the dump.  */

  if (flow_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (flow_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  if (obey_regdecls)
    {
      TIMEVAR (flow_time,
	       {
		 regclass (insns, max_reg_num ());
		 stupid_life_analysis (insns, max_reg_num (),
				       flow_dump_file);
	       });
    }
  else
    {
      /* Do control and data flow analysis,
	 and write some of the results to dump file.  */

      TIMEVAR (flow_time, flow_analysis (insns, max_reg_num (),
					 flow_dump_file));
      if (warn_uninitialized)
	{
	  uninitialized_vars_warning (DECL_INITIAL (decl));
	  setjmp_args_warning ();
	}
    }

  /* Dump rtl after flow analysis.  */

  if (flow_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (flow_dump_file, insns);
	       fflush (flow_dump_file);
	     });

  /* If -opt, try combining insns through substitution.  */

  if (optimize > 0)
    TIMEVAR (combine_time, combine_instructions (insns, max_reg_num ()));

  /* Dump rtl code after insn combination.  */

  if (combine_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (combine_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       dump_combine_stats (combine_dump_file);
	       print_rtl (combine_dump_file, insns);
	       fflush (combine_dump_file);
	     });

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */

  if (sched_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (sched_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  if (optimize > 0 && flag_schedule_insns)
    {
      /* Do control and data sched analysis,
	 and write some of the results to dump file.  */

      TIMEVAR (sched_time, schedule_insns (sched_dump_file));
    }

  /* Dump rtl after instruction scheduling.  */

  if (sched_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (sched_dump_file, insns);
	       fflush (sched_dump_file);
	     });

  /* Unless we did stupid register allocation,
     allocate pseudo-regs that are used only within 1 basic block.  */

  if (!obey_regdecls)
    TIMEVAR (local_alloc_time,
	     {
	       regclass (insns, max_reg_num ());
	       local_alloc ();
	     });

  /* Dump rtl code after allocating regs within basic blocks.  */

  if (local_reg_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (local_reg_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       dump_flow_info (local_reg_dump_file);
	       dump_local_alloc (local_reg_dump_file);
	       print_rtl (local_reg_dump_file, insns);
	       fflush (local_reg_dump_file);
	     });

  if (global_reg_dump)
    TIMEVAR (dump_time,
	     fprintf (global_reg_dump_file, "\n;; Function %s\n\n",
		      IDENTIFIER_POINTER (DECL_NAME (decl))));

  /* Unless we did stupid register allocation,
     allocate remaining pseudo-regs, then do the reload pass
     fixing up any insns that are invalid.  */

  TIMEVAR (global_alloc_time,
	   {
	     if (!obey_regdecls)
	       failure = global_alloc (global_reg_dump_file);
	     else
	       failure = reload (insns, 0, global_reg_dump_file);
	   });

  if (global_reg_dump)
    TIMEVAR (dump_time,
	     {
	       dump_global_regs (global_reg_dump_file);
	       print_rtl (global_reg_dump_file, insns);
	       fflush (global_reg_dump_file);
	     });

  if (failure)
    goto exit_rest_of_compilation;

  reload_completed = 1;

  /* On some machines, the prologue and epilogue code, or parts thereof,
     can be represented as RTL.  Doing so lets us schedule insns between
     it and the rest of the code and also allows delayed branch
     scheduling to operate in the epilogue.  */

  thread_prologue_and_epilogue_insns (insns);

  if (optimize > 0 && flag_schedule_insns_after_reload)
    {
      if (sched2_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (sched2_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		 });

      /* Do control and data sched analysis again,
	 and write some more of the results to dump file.  */

      TIMEVAR (sched2_time, schedule_insns (sched2_dump_file));

      /* Dump rtl after post-reorder instruction scheduling.  */

      if (sched2_dump)
	TIMEVAR (dump_time,
		 {
		   print_rtl (sched2_dump_file, insns);
		   fflush (sched2_dump_file);
		 });
    }

#ifdef LEAF_REGISTERS
  leaf_function = 0;
  if (optimize > 0 && only_leaf_regs_used () && leaf_function_p ())
    leaf_function = 1;
#endif

  /* One more attempt to remove jumps to .+1
     left by dead-store-elimination.
     Also do cross-jumping this time
     and delete no-op move insns.  */

  if (optimize > 0)
    {
      TIMEVAR (jump_time, jump_optimize (insns, 1, 1, 0));
    }

  /* Dump rtl code after jump, if we are doing that.  */

  if (jump2_opt_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (jump2_opt_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       print_rtl (jump2_opt_dump_file, insns);
	       fflush (jump2_opt_dump_file);
	     });

  /* If a scheduling pass for delayed branches is to be done,
     call the scheduling code. */

#ifdef DELAY_SLOTS
  if (optimize > 0 && flag_delayed_branch)
    {
      TIMEVAR (dbr_sched_time, dbr_schedule (insns, dbr_sched_dump_file));
      if (dbr_sched_dump)
	{
	  TIMEVAR (dump_time,
		 {
		   fprintf (dbr_sched_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (dbr_sched_dump_file, insns);
		   fflush (dbr_sched_dump_file);
		 });
	}
    }
#endif

  if (optimize > 0)
    /* Shorten branches.  */
    TIMEVAR (shorten_branch_time,
	     {
	       shorten_branches (get_insns ());
	     });

#ifdef STACK_REGS
  TIMEVAR (stack_reg_time, reg_to_stack (insns, stack_reg_dump_file));
  if (stack_reg_dump)
    {
      TIMEVAR (dump_time,
	       {
		 fprintf (stack_reg_dump_file, "\n;; Function %s\n\n",
			  IDENTIFIER_POINTER (DECL_NAME (decl)));
		 print_rtl (stack_reg_dump_file, insns);
		 fflush (stack_reg_dump_file);
	       });
    }
#endif

  /* Now turn the rtl into assembler code.  */

  TIMEVAR (final_time,
	   {
	     rtx x;
	     char *fnname;

	     /* Get the function's name, as described by its RTL.
		This may be different from the DECL_NAME name used
		in the source file.  */

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
	     final_end_function (insns, asm_out_file, optimize);
	     assemble_end_function (decl, fnname);
	     fflush (asm_out_file);
	   });

  /* Write DBX symbols if requested */

  /* Note that for those inline functions where we don't initially
     know for certain that we will be generating an out-of-line copy,
     the first invocation of this routine (rest_of_compilation) will
     skip over this code by doing a `goto exit_rest_of_compilation;'.
     Later on, finish_compilation will call rest_of_compilation again
     for those inline functions that need to have out-of-line copies
     generated.  During that call, we *will* be routed past here.  */

#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    TIMEVAR (symout_time, dbxout_function (decl));
#endif

#ifdef DWARF_DEBUGGING_INFO
  if (write_symbols == DWARF_DEBUG)
    TIMEVAR (symout_time, dwarfout_file_scope_decl (decl, 0));
#endif

 exit_rest_of_compilation:

  /* In case the function was not output,
     don't leave any temporary anonymous types
     queued up for sdb output.  */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_types (NULL_TREE);
#endif

  /* Put back the tree of subblocks and list of arguments
     from before we copied them.
     Code generation and the output of debugging info may have modified
     the copy, but the original is unchanged.  */

  if (saved_block_tree != 0)
    DECL_INITIAL (decl) = saved_block_tree;
  if (saved_arguments != 0)
    DECL_ARGUMENTS (decl) = saved_arguments;

  reload_completed = 0;

  /* Clear out the real_constant_chain before some of the rtx's
     it runs through become garbage.  */

  clear_const_double_mem ();

  /* Cancel the effect of rtl_in_current_obstack.  */

  resume_temporary_allocation ();

  /* The parsing time is all the time spent in yyparse
     *except* what is spent in this function.  */

  parse_time -= get_run_time () - start_time;
}

/* Entry point of cc1/c++.  Decode command args, then call compile_file.
   Exit code is 35 if can't open files, 34 if fatal error,
   33 if had nonfatal errors, else success.  */

int
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  register int i;
  char *filename = 0;
  int flag_print_mem = 0;
  int version_flag = 0;
  char *p;

  /* save in case md file wants to emit args as a comment.  */
  save_argc = argc;
  save_argv = argv;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/') --p;
  progname = p;

#ifdef RLIMIT_STACK
  /* Get rid of any avoidable limit on stack size.  */
  {
    struct rlimit rlim;

    /* Set the stack limit huge so that alloca does not fail. */
    getrlimit (RLIMIT_STACK, &rlim);
    rlim.rlim_cur = rlim.rlim_max;
    setrlimit (RLIMIT_STACK, &rlim);
  }
#endif /* RLIMIT_STACK */

  signal (SIGFPE, float_signal);

#ifdef SIGPIPE
  signal (SIGPIPE, pipe_closed);
#endif

  decl_printable_name = decl_name;
  lang_expand_expr = (struct rtx_def *(*)()) do_abort;

  /* Initialize whether `char' is signed.  */
  flag_signed_char = DEFAULT_SIGNED_CHAR;
#ifdef DEFAULT_SHORT_ENUMS
  /* Initialize how much space enums occupy, by default.  */
  flag_short_enums = DEFAULT_SHORT_ENUMS;
#endif

  /* Scan to see what optimization level has been specified.  That will
     determine the default value of many flags.  */
  for (i = 1; i < argc; i++)
    {
      if (!strcmp (argv[i], "-O"))
	{
	  optimize = 1;
	}
      else if (argv[i][0] == '-' && argv[i][1] == 'O')
	{
	  /* Handle -O2, -O3, -O69, ...  */
	  char *p = &argv[i][2];
	  int c;

	  while (c = *p++)
	    if (! (c >= '0' && c <= '9'))
	      break;
	  if (c == 0)
	    optimize = atoi (&argv[i][2]);
	}
    }

  obey_regdecls = (optimize == 0);
  if (optimize == 0)
    {
      flag_no_inline = 1;
      warn_inline = 0;
    }

  if (optimize >= 1)
    {
      flag_defer_pop = 1;
      flag_thread_jumps = 1;
#ifdef DELAY_SLOTS
      flag_delayed_branch = 1;
#endif
    }

  if (optimize >= 2)
    {
      flag_cse_follow_jumps = 1;
      flag_cse_skip_blocks = 1;
      flag_expensive_optimizations = 1;
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
      flag_caller_saves = 1;
#ifdef INSN_SCHEDULING
      flag_schedule_insns = 1;
      flag_schedule_insns_after_reload = 1;
#endif
    }

#ifdef OPTIMIZATION_OPTIONS
  /* Allow default optimizations to be specified on a per-machine basis.  */
  OPTIMIZATION_OPTIONS (optimize);
#endif

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  target_flags = 0;
  set_target_switch ("");

  for (i = 1; i < argc; i++)
    {
      int j;
      /* If this is a language-specific option,
	 decode it in a language-specific way.  */
      for (j = 0; lang_options[j] != 0; j++)
	if (!strncmp (argv[i], lang_options[j],
		      strlen (lang_options[j])))
	  break;
      if (lang_options[j] != 0)
	/* If the option is valid for *some* language,
	   treat it as valid even if this language doesn't understand it.  */
	lang_decode_option (argv[i]);
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  register char *str = argv[i] + 1;
	  if (str[0] == 'Y')
	    str++;

	  if (str[0] == 'm')
	    set_target_switch (&str[1]);
	  else if (!strcmp (str, "dumpbase"))
	    {
	      dump_base_name = argv[++i];
	    }
	  else if (str[0] == 'd')
	    {
	      register char *p = &str[1];
	      while (*p)
		switch (*p++)
		  {
 		  case 'a':
 		    combine_dump = 1;
 		    dbr_sched_dump = 1;
 		    flow_dump = 1;
 		    global_reg_dump = 1;
 		    jump_opt_dump = 1;
 		    jump2_opt_dump = 1;
 		    local_reg_dump = 1;
 		    loop_dump = 1;
 		    rtl_dump = 1;
 		    cse_dump = 1, cse2_dump = 1;
 		    sched_dump = 1;
 		    sched2_dump = 1;
		    stack_reg_dump = 1;
		    break;
		  case 'k':
		    stack_reg_dump = 1;
		    break;
		  case 'c':
		    combine_dump = 1;
		    break;
		  case 'd':
		    dbr_sched_dump = 1;
		    break;
		  case 'f':
		    flow_dump = 1;
		    break;
		  case 'g':
		    global_reg_dump = 1;
		    break;
		  case 'j':
		    jump_opt_dump = 1;
		    break;
		  case 'J':
		    jump2_opt_dump = 1;
		    break;
		  case 'l':
		    local_reg_dump = 1;
		    break;
		  case 'L':
		    loop_dump = 1;
		    break;
		  case 'm':
		    flag_print_mem = 1;
		    break;
		  case 'p':
		    flag_print_asm_name = 1;
		    break;
		  case 'r':
		    rtl_dump = 1;
		    break;
		  case 's':
		    cse_dump = 1;
		    break;
		  case 't':
		    cse2_dump = 1;
		    break;
		  case 'S':
		    sched_dump = 1;
		    break;
		  case 'R':
		    sched2_dump = 1;
		    break;
		  case 'y':
		    set_yydebug (1);
		    break;

		  case 'x':
		    rtl_dump_and_exit = 1;
		    break;
		  }
	    }
	  else if (str[0] == 'f')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -f option.
		 P's value is the option sans `-f'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (f_options) / sizeof (f_options[0]);
		   j++)
		{
		  if (!strcmp (p, f_options[j].string))
		    {
		      *f_options[j].variable = f_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, f_options[j].string))
		    {
		      *f_options[j].variable = ! f_options[j].on_value;
		      found = 1;
		    }
		}

	      if (found)
		;
	      else if (!strncmp (p, "fixed-", 6))
		fix_register (&p[6], 1, 1);
	      else if (!strncmp (p, "call-used-", 10))
		fix_register (&p[10], 0, 1);
	      else if (!strncmp (p, "call-saved-", 11))
		fix_register (&p[11], 0, 0);
	      else
		error ("Invalid option `%s'", argv[i]);
	    }
	  else if (str[0] == 'O')
	    {
	      register char *p = str+1;
	      while (*p && *p >= '0' && *p <= '9')
		p++;
	      if (*p == '\0')
		;
	      else
		error ("Invalid option `%s'", argv[i]);
	    }
	  else if (!strcmp (str, "pedantic"))
	    pedantic = 1;
	  else if (!strcmp (str, "pedantic-errors"))
	    flag_pedantic_errors = pedantic = 1;
	  else if (!strcmp (str, "quiet"))
	    quiet_flag = 1;
	  else if (!strcmp (str, "version"))
	    version_flag = 1;
	  else if (!strcmp (str, "w"))
	    inhibit_warnings = 1;
	  else if (!strcmp (str, "W"))
	    {
	      extra_warnings = 1;
	      warn_uninitialized = 1;
	    }
	  else if (str[0] == 'W')
	    {
	      register char *p = &str[1];
	      int found = 0;

	      /* Some kind of -W option.
		 P's value is the option sans `-W'.
		 Search for it in the table of options.  */

	      for (j = 0;
		   !found && j < sizeof (W_options) / sizeof (W_options[0]);
		   j++)
		{
		  if (!strcmp (p, W_options[j].string))
		    {
		      *W_options[j].variable = W_options[j].on_value;
		      /* A goto here would be cleaner,
			 but breaks the vax pcc.  */
		      found = 1;
		    }
		  if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		      && ! strcmp (p+3, W_options[j].string))
		    {
		      *W_options[j].variable = ! W_options[j].on_value;
		      found = 1;
		    }
		}

	      if (found)
		;
	      else if (!strncmp (p, "id-clash-", 9))
		{
		  char *endp = p + 9;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto id_clash_lose;
			}
		    }
		  warn_id_clash = 1;
		  id_clash_len = atoi (str + 10);
		id_clash_lose: ;
		}
	      else
		error ("Invalid option `%s'", argv[i]);
	    }
	  else if (!strcmp (str, "p"))
	    profile_flag = 1;
	  else if (!strcmp (str, "a"))
	    {
#if !defined (BLOCK_PROFILER) || !defined (FUNCTION_BLOCK_PROFILER)
	      warning ("`-a' option (basic block profile) not supported");
#else
	      profile_block_flag = 1;
#endif
	    }
	  else if (str[0] == 'g')
	    {
	      char *p = str + 1;
	      char *q;
	      unsigned len;
	      unsigned level;

	      while (*p && (*p < '0' || *p > '9'))
		p++;
	      len = p - str;
	      q = p;
	      while (*q && (*q >= '0' && *q <= '9'))
		q++;
	      if (*p)
		level = atoi (p);
	      else
		level = 2;	/* default debugging info level */
	      if (*q || level > 3)
		{
		  warning ("invalid debug level specification in option: `-%s'",
			   str);
		  warning ("no debugging information will be generated");
		  level = 0;
		}

	      /* If more than one debugging type is supported,
		 you must define PREFERRED_DEBUGGING_TYPE
		 to choose a format in a system-dependent way.  */
	      /* This is one long line cause VAXC can't handle a \-newline.  */
#if 1 < (defined (DBX_DEBUGGING_INFO) + defined (SDB_DEBUGGING_INFO) + defined (DWARF_DEBUGGING_INFO) + defined (XCOFF_DEBUGGING_INFO))
#ifdef PREFERRED_DEBUGGING_TYPE
	      if (!strncmp (str, "ggdb", len))
		write_symbols = PREFERRED_DEBUGGING_TYPE;
#else /* no PREFERRED_DEBUGGING_TYPE */
You Lose!  You must define PREFERRED_DEBUGGING_TYPE!
#endif /* no PREFERRED_DEBUGGING_TYPE */
#endif /* More than one debugger format enabled.  */
#ifdef DBX_DEBUGGING_INFO
	      if (write_symbols != NO_DEBUG)
		;
	      else if (!strncmp (str, "ggdb", len))
		write_symbols = DBX_DEBUG;
	      else if (!strncmp (str, "gstabs", len))
		write_symbols = DBX_DEBUG;
	      else if (!strncmp (str, "gstabs+", len))
		write_symbols = DBX_DEBUG;

	      /* Always enable extensions for -ggdb or -gstabs+, 
		 always disable for -gstabs.
		 For plain -g, use system-specific default.  */
	      if (write_symbols == DBX_DEBUG && !strncmp (str, "ggdb", len)
		  && len >= 2)
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == DBX_DEBUG && !strncmp (str, "gstabs+", len)
		       && len >= 7)
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == DBX_DEBUG
		       && !strncmp (str, "gstabs", len) && len >= 2)
		use_gnu_debug_info_extensions = 0;
	      else
		use_gnu_debug_info_extensions = DEFAULT_GDB_EXTENSIONS;
#endif /* DBX_DEBUGGING_INFO */
#ifdef DWARF_DEBUGGING_INFO
	      if (write_symbols != NO_DEBUG)
		;
	      else if (!strncmp (str, "g", len))
		write_symbols = DWARF_DEBUG;
	      else if (!strncmp (str, "ggdb", len))
		write_symbols = DWARF_DEBUG;
	      else if (!strncmp (str, "gdwarf", len))
		write_symbols = DWARF_DEBUG;

	      /* Always enable extensions for -ggdb or -gdwarf+, 
		 always disable for -gdwarf.
		 For plain -g, use system-specific default.  */
	      if (write_symbols == DWARF_DEBUG && !strncmp (str, "ggdb", len)
		  && len >= 2)
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == DWARF_DEBUG && !strcmp (str, "gdwarf+"))
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == DWARF_DEBUG
		       && !strncmp (str, "gdwarf", len) && len >= 2)
		use_gnu_debug_info_extensions = 0;
	      else
		use_gnu_debug_info_extensions = DEFAULT_GDB_EXTENSIONS;
#endif
#ifdef SDB_DEBUGGING_INFO
	      if (write_symbols != NO_DEBUG)
		;
	      else if (!strncmp (str, "g", len))
		write_symbols = SDB_DEBUG;
	      else if (!strncmp (str, "gdb", len))
		write_symbols = SDB_DEBUG;
	      else if (!strncmp (str, "gcoff", len))
		write_symbols = SDB_DEBUG;
#endif /* SDB_DEBUGGING_INFO */
#ifdef XCOFF_DEBUGGING_INFO
	      if (write_symbols != NO_DEBUG)
		;
	      else if (!strncmp (str, "g", len))
		write_symbols = XCOFF_DEBUG;
	      else if (!strncmp (str, "ggdb", len))
		write_symbols = XCOFF_DEBUG;
	      else if (!strncmp (str, "gxcoff", len))
		write_symbols = XCOFF_DEBUG;

	      /* Always enable extensions for -ggdb or -gxcoff+,
		 always disable for -gxcoff.
		 For plain -g, use system-specific default.  */
	      if (write_symbols == XCOFF_DEBUG && !strncmp (str, "ggdb", len)
		  && len >= 2)
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == XCOFF_DEBUG && !strcmp (str, "gxcoff+"))
		use_gnu_debug_info_extensions = 1;
	      else if (write_symbols == XCOFF_DEBUG
		       && !strncmp (str, "gxcoff", len) && len >= 2)
		use_gnu_debug_info_extensions = 0;
	      else
		use_gnu_debug_info_extensions = DEFAULT_GDB_EXTENSIONS;
#endif	      
	      if (write_symbols == NO_DEBUG)
		warning ("`-%s' option not supported on this version of GCC", str);
	      else if (level == 0)
		write_symbols = NO_DEBUG;
	      else
		debug_info_level = (enum debug_info_level) level;
	    }
	  else if (!strcmp (str, "o"))
	    {
	      asm_file_name = argv[++i];
	    }
	  else if (str[0] == 'G')
	    {
	      g_switch_set = TRUE;
	      g_switch_value = atoi ((str[1] != '\0') ? str+1 : argv[++i]);
	    }
	  else if (!strncmp (str, "aux-info", 8))
	    {
	      flag_gen_aux_info = 1;
	      aux_info_file_name = (str[8] != '\0' ? str+8 : argv[++i]);
	    }
	  else
	    error ("Invalid option `%s'", argv[i]);
	}
      else if (argv[i][0] == '+')
	error ("Invalid option `%s'", argv[i]);
      else
	filename = argv[i];
    }

  if (optimize == 0)
    {
      /* Inlining does not work if not optimizing,
	 so force it not to be done.  */
      flag_no_inline = 1;
      warn_inline = 0;

      /* The c_decode_option and lang_decode_option functions set
	 this to `2' if -Wall is used, so we can avoid giving out
	 lots of errors for people who don't realize what -Wall does.  */
      if (warn_uninitialized == 1)
	warning ("-Wuninitialized is not supported without -O");
    }

#if defined(DWARF_DEBUGGING_INFO)
  if (write_symbols == DWARF_DEBUG
      && strcmp (language_string, "GNU C++") == 0)
    {
      warning ("-g option not supported for C++ on SVR4 systems");
      write_symbols = NO_DEBUG;
    }
#endif /* defined(DWARF_DEBUGGING_INFO) */

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

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

  /* Warn about options that are not supported on this machine.  */
#ifndef INSN_SCHEDULING
  if (flag_schedule_insns || flag_schedule_insns_after_reload)
    warning ("instruction scheduling not supported on this target machine");
#endif
#ifndef DELAY_SLOTS
  if (flag_delayed_branch)
    warning ("this target machine does not have delayed branches");
#endif

  /* If we are in verbose mode, write out the version and maybe all the
     option flags in use.  */
  if (version_flag)
    {
      fprintf (stderr, "%s version %s", language_string, version_string);
#ifdef TARGET_VERSION
      TARGET_VERSION;
#endif
#ifdef __GNUC__
#ifndef __VERSION__
#define __VERSION__ "[unknown]"
#endif
      fprintf (stderr, " compiled by GNU C version %s.\n", __VERSION__);
#else
      fprintf (stderr, " compiled by CC.\n");
#endif
      if (! quiet_flag)
	print_switch_values ();
    }

  /* Now that register usage is specified, convert it to HARD_REG_SETs.  */
  init_reg_sets_1 ();

  compile_file (filename);

#ifndef OS2
#ifndef VMS
  if (flag_print_mem)
    {
      char *lim = (char *) sbrk (0);

      fprintf (stderr, "Data size %d.\n",
	       lim - (char *) &environ);
      fflush (stderr);

#ifdef USG
      system ("ps -l 1>&2");
#else /* not USG */
      system ("ps v");
#endif /* not USG */
    }
#endif /* not VMS */
#endif /* not OS2 */

  if (errorcount)
    exit (FATAL_EXIT_CODE);
  if (sorrycount)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);
  return 34;
}

/* Decode -m switches.  */

/* Here is a table, controlled by the tm.h file, listing each -m switch
   and which bits in `target_switches' it should set or clear.
   If VALUE is positive, it is bits to set.
   If VALUE is negative, -VALUE is bits to clear.
   (The sign bit is not used so there is no confusion.)  */

struct {char *name; int value;} target_switches []
  = TARGET_SWITCHES;

/* This table is similar, but allows the switch to have a value.  */

#ifdef TARGET_OPTIONS
struct {char *prefix; char ** variable;} target_options []
  = TARGET_OPTIONS;
#endif

/* Decode the switch -mNAME.  */

void
set_target_switch (name)
     char *name;
{
  register int j;
  int valid = 0;

  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (!strcmp (target_switches[j].name, name))
      {
	if (target_switches[j].value < 0)
	  target_flags &= ~-target_switches[j].value;
	else
	  target_flags |= target_switches[j].value;
	valid = 1;
      }

#ifdef TARGET_OPTIONS
  if (!valid)
    for (j = 0; j < sizeof target_options / sizeof target_options[0]; j++)
      {
	int len = strlen (target_options[j].prefix);
	if (!strncmp (target_options[j].prefix, name, len))
	  {
	    *target_options[j].variable = name + len;
	    valid = 1;
	  }
      }
#endif

  if (!valid)
    error ("Invalid option `%s'", name);
}

/* Variable used for communication between the following two routines.  */

static int line_position;

/* Print an option value and adjust the position in the line.  */

static void
print_single_switch (type, name)
     char *type, *name;
{
  fprintf (stderr, " %s%s", type, name);

  line_position += strlen (type) + strlen (name) + 1;

  if (line_position > 65)
    {
      fprintf (stderr, "\n\t");
      line_position = 8;
    }
}
     
/* Print default target switches for -version.  */

static void
print_switch_values ()
{
  register int j;

  fprintf (stderr, "enabled:");
  line_position = 8;

  for (j = 0; j < sizeof f_options / sizeof f_options[0]; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      print_single_switch ("-f", f_options[j].string);

  for (j = 0; j < sizeof W_options / sizeof W_options[0]; j++)
    if (*W_options[j].variable == W_options[j].on_value)
      print_single_switch ("-W", W_options[j].string);

  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (target_switches[j].name[0] != '\0'
	&& target_switches[j].value > 0
	&& ((target_switches[j].value & target_flags)
	    == target_switches[j].value))
      print_single_switch ("-m", target_switches[j].name);

  fprintf (stderr, "\n");
}
