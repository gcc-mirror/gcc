/* Top level of GNU C compiler
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.

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

/* This is the top level of cc1/c++.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#include <ctype.h>
#include <sys/stat.h>

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#undef FLOAT
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#undef FLOAT /* This is for hpux. They should change hpux.  */
#undef FFS  /* Some systems define this in param.h.  */

#ifdef TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
# include <sys/time.h>
# else
#  include <time.h>
#endif
#endif

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#ifdef HAVE_SYS_TIMES_H
# include <sys/times.h>
#endif

#include "input.h"
#include "tree.h"
#include "rtl.h"
#include "flags.h"
#include "insn-attr.h"
#include "insn-codes.h"
#include "insn-config.h"
#include "recog.h"
#include "defaults.h"
#include "output.h"
#include "bytecode.h"
#include "bc-emit.h"
#include "except.h"

#ifdef XCOFF_DEBUGGING_INFO
#include "xcoffout.h"
#endif

#ifdef VMS
/* The extra parameters substantially improve the I/O performance.  */
static FILE *
vms_fopen (fname, type)
     char * fname;
     char * type;
{
  /* The <stdio.h> in the gcc-vms-1.42 distribution prototypes fopen with two
     fixed arguments, which matches ANSI's specification but not VAXCRTL's
     pre-ANSI implementation.  This hack circumvents the mismatch problem.  */
  FILE *(*vmslib_fopen)() = (FILE *(*)()) fopen;

  if (*type == 'w')
    return (*vmslib_fopen) (fname, type, "mbc=32",
			    "deq=64", "fop=tef", "shr=nil");
  else
    return (*vmslib_fopen) (fname, type, "mbc=32");
}
#define fopen vms_fopen
#endif	/* VMS */

#ifndef DEFAULT_GDB_EXTENSIONS
#define DEFAULT_GDB_EXTENSIONS 1
#endif

/* If more than one debugging type is supported, you must define
   PREFERRED_DEBUGGING_TYPE to choose a format in a system-dependent way. 

   This is one long line cause VAXC can't handle a \-newline.  */
#if 1 < (defined (DBX_DEBUGGING_INFO) + defined (SDB_DEBUGGING_INFO) + defined (DWARF_DEBUGGING_INFO) + defined (DWARF2_DEBUGGING_INFO) + defined (XCOFF_DEBUGGING_INFO))
#ifndef PREFERRED_DEBUGGING_TYPE
You Lose!  You must define PREFERRED_DEBUGGING_TYPE!
#endif /* no PREFERRED_DEBUGGING_TYPE */
#else /* Only one debugging format supported.  Define PREFERRED_DEBUGGING_TYPE
	 so the following code needn't care.  */
#ifdef DBX_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif
#ifdef SDB_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG
#endif
#ifdef DWARF_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF_DEBUG
#endif
#ifdef DWARF2_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG
#endif
#ifdef XCOFF_DEBUGGING_INFO
#define PREFERRED_DEBUGGING_TYPE XCOFF_DEBUG
#endif
#endif /* More than one debugger format enabled.  */

/* If still not defined, must have been because no debugging formats
   are supported.  */
#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE NO_DEBUG
#endif

extern int rtx_equal_function_value_matters;

#if ! (defined (VMS) || defined (OS2))
extern char **environ;
#endif
extern char *version_string, *language_string;

/* Carry information from ASM_DECLARE_OBJECT_NAME
   to ASM_FINISH_DECLARE_OBJECT.  */

extern int size_directive_output;
extern tree last_assemble_variable_decl;

extern void init_lex ();
extern void init_decl_processing ();
extern void init_obstacks ();
extern void init_tree_codes ();
extern void init_rtl ();
extern void init_regs ();
extern void init_optabs ();
extern void init_stmt ();
extern void init_reg_sets ();
extern void dump_flow_info ();
extern void dump_sched_info ();
extern void dump_local_alloc ();
extern void regset_release_memory ();

void rest_of_decl_compilation ();
void error_with_file_and_line PVPROTO((char *file, int line, char *s, ...));
void error_with_decl PVPROTO((tree decl, char *s, ...));
void error_for_asm PVPROTO((rtx insn, char *s, ...));
void error PVPROTO((char *s, ...));
void fatal PVPROTO((char *s, ...));
void warning_with_file_and_line PVPROTO((char *file, int line, char *s, ...));
void warning_with_decl PVPROTO((tree decl, char *s, ...));
void warning_for_asm PVPROTO((rtx insn, char *s, ...));
void warning PVPROTO((char *s, ...));
void pedwarn PVPROTO((char *s, ...));
void pedwarn_with_decl PVPROTO((tree decl, char *s, ...));
void pedwarn_with_file_and_line PVPROTO((char *file, int line, char *s, ...));
void sorry PVPROTO((char *s, ...));
void really_sorry PVPROTO((char *s, ...));
void fancy_abort ();
#ifndef abort
void abort ();
#endif
void set_target_switch ();
static char *decl_name ();

void print_version ();
int print_single_switch ();
void print_switch_values ();
/* Length of line when printing switch values.  */
#define MAX_LINE 75

#ifdef NEED_DECLARATION_SBRK
extern char *sbrk ();
#endif

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

#if !USE_CPPLIB
/* Stream for reading from the input file.  */
FILE *finput;
#endif

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
int addressof_dump = 0;
int cse_dump = 0;
int loop_dump = 0;
int cse2_dump = 0;
int branch_prob_dump = 0;
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

/* Flag to output bytecode instead of native assembler */
int output_bytecode = 0;

/* Pointer to function to compute the name to use to print a declaration.
   DECL is the declaration in question.
   VERBOSITY determines what information will be printed:
     0: DECL_NAME, demangled as necessary.
     1: and scope information.
     2: and any other information that might be interesting, such as function
        parameter types in C++.  */

char *(*decl_printable_name) (/* tree decl, int verbosity */);

/* Pointer to function to compute rtl for a language-specific tree code.  */

struct rtx_def *(*lang_expand_expr) ();

/* Pointer to function to finish handling an incomplete decl at the
   end of compilation.  */

void (*incomplete_decl_finalize_hook) () = 0;

/* Highest label number used at the end of reload.  */

int max_label_num_after_reload;

/* Nonzero if generating code to do profiling.  */

int profile_flag = 0;

/* Nonzero if generating code to do profiling on a line-by-line basis.  */

int profile_block_flag;

/* Nonzero if generating code to profile program flow graph arcs.  */

int profile_arc_flag = 0;

/* Nonzero if generating info for gcov to calculate line test coverage.  */

int flag_test_coverage = 0;

/* Nonzero indicates that branch taken probabilities should be calculated.  */

int flag_branch_probabilities = 0;

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

/* Nonzero means place each function into its own section on those platforms
   which support arbitrary section names and unlimited numbers of sections.  */

int flag_function_sections = 0;

/* Nonzero to inhibit use of define_optimization peephole opts.  */

int flag_no_peephole = 0;

/* Nonzero allows GCC to violate some IEEE or ANSI rules regarding math
   operations in the interest of optimization.  For example it allows
   GCC to assume arguments to sqrt are nonnegative numbers, allowing
   faster code for sqrt to be generated.  */

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

/* Nonzero means that functions will not be inlined.  */

int flag_no_inline;

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
   Value is 1 if we are doing reasonable (i.e. simple
   offset into offset table) pic.  Value is 2 if we can
   only perform register offsets.  */

int flag_pic;

/* Nonzero means generate extra code for exception handling and enable
   exception handling.  */

int flag_exceptions = 2;

/* Nonzero means don't place uninitialized global data in common storage
   by default.  */

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

/* -fgnu-linker specifies use of the GNU linker for initializations.
   (Or, more generally, a linker that handles initializations.)
   -fno-gnu-linker says that collect2 will be used.  */
#ifdef USE_COLLECT2
int flag_gnu_linker = 0;
#else
int flag_gnu_linker = 1;
#endif

/* Tag all structures with __attribute__(packed) */
int flag_pack_struct = 0;

/* Emit code to check for stack overflow; also may cause large objects
   to be allocated dynamically.  */
int flag_stack_check;

/* -fcheck-memory-usage causes extra code to be generated in order to check
   memory accesses.  This is used by a detector of bad memory accesses such
   as Checker.  */
int flag_check_memory_usage = 0;

/* -fprefix-function-name causes function name to be prefixed.  This
   can be used with -fcheck-memory-usage to isolate code compiled with
   -fcheck-memory-usage.  */
int flag_prefix_function_name = 0;

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
  {"keep-static-consts", &flag_keep_static_consts, 1},
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
  {"exceptions", &flag_exceptions, 1},
  {"sjlj-exceptions", &exceptions_via_longjmp, 1},
  {"asynchronous-exceptions", &asynchronous_exceptions, 1},
  {"profile-arcs", &profile_arc_flag, 1},
  {"test-coverage", &flag_test_coverage, 1},
  {"branch-probabilities", &flag_branch_probabilities, 1},
  {"fast-math", &flag_fast_math, 1},
  {"common", &flag_no_common, 0},
  {"inhibit-size-directive", &flag_inhibit_size_directive, 1},
  {"function-sections", &flag_function_sections, 1},
  {"verbose-asm", &flag_verbose_asm, 1},
  {"gnu-linker", &flag_gnu_linker, 1},
  {"pack-struct", &flag_pack_struct, 1},
  {"stack-check", &flag_stack_check, 1},
  {"bytecode", &output_bytecode, 1},
  {"check-memory-usage", &flag_check_memory_usage, 1},
  {"prefix-function-name", &flag_prefix_function_name, 1}
};

/* Table of language-specific options.  */

char *lang_options[] =
{
  "-ansi",
  "-fallow-single-precision",

  "-fsigned-bitfields",
  "-funsigned-bitfields",
  "-fno-signed-bitfields",
  "-fno-unsigned-bitfields",
  "-fsigned-char",
  "-funsigned-char",
  "-fno-signed-char",
  "-fno-unsigned-char",

  "-ftraditional",
  "-traditional",
  "-fnotraditional",
  "-fno-traditional",

  "-fasm",
  "-fno-asm",
  "-fbuiltin",
  "-fno-builtin",
  "-fhosted",
  "-fno-hosted",
  "-ffreestanding",
  "-fno-freestanding",
  "-fcond-mismatch",
  "-fno-cond-mismatch",
  "-fdollars-in-identifiers",
  "-fno-dollars-in-identifiers",
  "-fident",
  "-fno-ident",
  "-fshort-double",
  "-fno-short-double",
  "-fshort-enums",
  "-fno-short-enums",

  "-Wall",
  "-Wbad-function-cast",
  "-Wno-bad-function-cast",
  "-Wcast-qual",
  "-Wno-cast-qual",
  "-Wchar-subscripts",
  "-Wno-char-subscripts",
  "-Wcomment",
  "-Wno-comment",
  "-Wcomments",
  "-Wno-comments",
  "-Wconversion",
  "-Wno-conversion",
  "-Wformat",
  "-Wno-format",
  "-Wimport",
  "-Wno-import",
  "-Wimplicit-function-declaration",
  "-Wno-implicit-function-declaration",
  "-Werror-implicit-function-declaration",
  "-Wimplicit-int",
  "-Wno-implicit-int",
  "-Wimplicit",
  "-Wno-implicit",
  "-Wmain",
  "-Wno-main",
  "-Wmissing-braces",
  "-Wno-missing-braces",
  "-Wmissing-declarations",
  "-Wno-missing-declarations",
  "-Wmissing-prototypes",
  "-Wno-missing-prototypes",
  "-Wnested-externs",
  "-Wno-nested-externs",
  "-Wparentheses",
  "-Wno-parentheses",
  "-Wpointer-arith",
  "-Wno-pointer-arith",
  "-Wredundant-decls",
  "-Wno-redundant-decls",
  "-Wsign-compare",
  "-Wno-sign-compare",
  "-Wstrict-prototypes",
  "-Wno-strict-prototypes",
  "-Wtraditional",
  "-Wno-traditional",
  "-Wtrigraphs",
  "-Wno-trigraphs",
  "-Wundef",
  "-Wno-undef",
  "-Wwrite-strings",
  "-Wno-write-strings",

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
  "-print-objc-runtime-info",

#include "options.h"
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
unsigned id_clash_len;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */
 
int warn_larger_than;
unsigned larger_than_size;

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
FILE *addressof_dump_file;
FILE *cse_dump_file;
FILE *loop_dump_file;
FILE *cse2_dump_file;
FILE *branch_prob_dump_file;
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
int branch_prob_time;
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
#ifndef _WIN32
#ifdef USG
  struct tms tms;
#else
#ifndef VMS
  struct rusage rusage;
#else
  struct
    {
      int proc_user_time;
      int proc_system_time;
      int child_user_time;
      int child_system_time;
    } vms_times;
#endif
#endif
#endif

  if (quiet_flag)
    return 0;
#ifdef _WIN32
  if (clock() < 0)
    return 0;
  else
    return (clock() * 1000);
#else /* not _WIN32 */
#ifdef USG
# if HAVE_SYSCONF && defined _SC_CLK_TCK
#  define TICKS_PER_SECOND sysconf (_SC_CLK_TCK) /* POSIX 1003.1-1996 */
# else
#  ifdef CLK_TCK
#   define TICKS_PER_SECOND CLK_TCK /* POSIX 1003.1-1988; obsolescent */
#  else
#   define TICKS_PER_SECOND HZ /* traditional UNIX */
#  endif
# endif
  times (&tms);
  return (tms.tms_utime + tms.tms_stime) * (1000000 / TICKS_PER_SECOND);
#else
#ifndef VMS
  getrusage (0, &rusage);
  return (rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec
	  + rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec);
#else /* VMS */
  times ((void *) &vms_times);
  return (vms_times.proc_user_time + vms_times.proc_system_time) * 10000;
#endif
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
  exit (FATAL_EXIT_CODE);
}

void
fatal_io_error (name)
     char *name;
{
  fprintf (stderr, "%s: %s: I/O error\n", progname, name);
  exit (FATAL_EXIT_CODE);
}

/* Called to give a better error message for a bad insn rather than
   just calling abort().  */

void
fatal_insn (message, insn)
     char *message;
     rtx insn;
{
  if (!output_bytecode)
    {
      error (message);
      debug_rtx (insn);
    }
  if (asm_out_file)
    fflush (asm_out_file);
  if (aux_info_file)
    fflush (aux_info_file);
  if (rtl_dump_file)
    fflush (rtl_dump_file);
  if (jump_opt_dump_file)
    fflush (jump_opt_dump_file);
  if (addressof_dump_file)
    fflush (addressof_dump_file);
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
  fflush (stdout);
  fflush (stderr);
  abort ();
}

/* Called to give a better error message when we don't have an insn to match
   what we are looking for or if the insn's constraints aren't satisfied,
   rather than just calling abort().  */

void
fatal_insn_not_found (insn)
     rtx insn;
{
  if (INSN_CODE (insn) < 0)
    fatal_insn ("internal error--unrecognizable insn:", insn);
  else
    fatal_insn ("internal error--insn does not satisfy its constraints:", insn);
}

/* This is the default decl_printable_name function.  */

static char *
decl_name (decl, verbosity)
     tree decl;
     int verbosity;
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
      if (rtl_dump_and_exit)
	fprintf (stderr, "%s ", IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	fprintf (stderr, " %s", (*decl_printable_name) (decl, 2));
      fflush (stderr);
      need_error_newline = 1;
      last_error_function = current_function_decl;
    }
}

/* The default function to print out name of current function that caused
   an error.  */

void
default_print_error_function (file)
     char *file;
{
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
	  char *name = (*decl_printable_name) (current_function_decl, 2);
	  fprintf (stderr, "In %s `%s':\n", kind, name);
	}

      last_error_function = current_function_decl;
    }
}

/* Called by report_error_function to print out function name.
 * Default may be overridden by language front-ends.  */

void (*print_error_function) PROTO((char *)) = default_print_error_function;

/* Prints out, if necessary, the name of the current function
  that caused an error.  Called from all error and warning functions.  */

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

  (*print_error_function) (file);

  if (input_file_stack && input_file_stack->next != 0
      && input_file_stack_tick != last_error_tick
      && file == input_filename)
    {
      fprintf (stderr, "In file included");
      for (p = input_file_stack->next; p; p = p->next)
	{
	  fprintf (stderr, " from %s:%d", p->name, p->line);
	  if (p->next)
	    fprintf (stderr, ",\n                ");
	}
      fprintf (stderr, ":\n");
      last_error_tick = input_file_stack_tick;
    }
}

/* Print a message.  */

static void
vmessage (prefix, s, ap)
     char *prefix;
     char *s;
     va_list ap;
{
  if (prefix)
    fprintf (stderr, "%s: ", prefix);

#ifdef HAVE_VPRINTF
  vfprintf (stderr, s, ap);
#else
  {
    HOST_WIDE_INT v1 = va_arg(ap, HOST_WIDE_INT);
    HOST_WIDE_INT v2 = va_arg(ap, HOST_WIDE_INT);
    HOST_WIDE_INT v3 = va_arg(ap, HOST_WIDE_INT);
    HOST_WIDE_INT v4 = va_arg(ap, HOST_WIDE_INT);
    fprintf (stderr, s, v1, v2, v3, v4);
  }
#endif
}

/* Print a message relevant to line LINE of file FILE.  */

static void
v_message_with_file_and_line (file, line, prefix, s, ap)
     char *file;
     int line;
     char *prefix;
     char *s;
     va_list ap;
{
  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);

  vmessage (prefix, s, ap);
  fputc ('\n', stderr);
}

/* Print a message relevant to the given DECL.  */

static void
v_message_with_decl (decl, prefix, s, ap)
     tree decl;
     char *prefix;
     char *s;
     va_list ap;
{
  char *n, *p;

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  if (prefix)
    fprintf (stderr, "%s: ", prefix);

  /* Do magic to get around lack of varargs support for insertion
     of arguments into existing list.  We know that the decl is first;
     we ass_u_me that it will be printed with "%s".  */

  for (p = s; *p; ++p)
    {
      if (*p == '%')
	{
	  if (*(p + 1) == '%')
	    ++p;
	  else
	    break;
	}
    }

  if (p > s)			/* Print the left-hand substring.  */
    {
      char fmt[sizeof "%.255s"];
      long width = p - s;
             
      if (width > 255L) width = 255L;	/* arbitrary */
      sprintf (fmt, "%%.%lds", width);
      fprintf (stderr, fmt, s);
    }

  if (*p == '%')		/* Print the name.  */
    {
      char *n = (DECL_NAME (decl)
		 ? (*decl_printable_name) (decl, 2)
		 : "((anonymous))");
      fputs (n, stderr);
      while (*p)
	{
	  ++p;
	  if (isalpha (*(p - 1) & 0xFF))
	    break;
	}
    }

  if (*p)			/* Print the rest of the message.  */
    vmessage ((char *)NULL, p, ap);

  fputc ('\n', stderr);
}

/* Figure file and line of the given INSN.  */

static void
file_and_line_for_asm (insn, pfile, pline)
     rtx insn;
     char **pfile;
     int *pline;
{
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
  else
    asmop = NULL;

  if (asmop)
    {
      *pfile = ASM_OPERANDS_SOURCE_FILE (asmop);
      *pline = ASM_OPERANDS_SOURCE_LINE (asmop);
    }
  else
    {
      *pfile = input_filename;
      *pline = lineno;
    }
}

/* Report an error at line LINE of file FILE.  */

static void
v_error_with_file_and_line (file, line, s, ap)
     char *file;
     int line;
     char *s;
     va_list ap;
{
  count_error (0);
  report_error_function (file);
  v_message_with_file_and_line (file, line, (char *)NULL, s, ap);
}

void
error_with_file_and_line VPROTO((char *file, int line, char *s, ...))
{
#ifndef __STDC__
  char *file;
  int line;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  file = va_arg (ap, char *);
  line = va_arg (ap, int);
  s = va_arg (ap, char *);
#endif

  v_error_with_file_and_line (file, line, s, ap);
  va_end (ap);
}

/* Report an error at the declaration DECL.
   S is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la printf.  */

static void
v_error_with_decl (decl, s, ap)
     tree decl;
     char *s;
     va_list ap;
{
  count_error (0);
  report_error_function (DECL_SOURCE_FILE (decl));
  v_message_with_decl (decl, (char *)NULL, s, ap);
}

void
error_with_decl VPROTO((tree decl, char *s, ...))
{
#ifndef __STDC__
  tree decl;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  decl = va_arg (ap, tree);
  s = va_arg (ap, char *);
#endif

  v_error_with_decl (decl, s, ap);
  va_end (ap);
}

/* Report an error at the line number of the insn INSN.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

static void
v_error_for_asm (insn, s, ap)
     rtx insn;
     char *s;
     va_list ap;
{
  char *file;
  int line;

  count_error (0);
  file_and_line_for_asm (insn, &file, &line);
  report_error_function (file);
  v_message_with_file_and_line (file, line, (char *)NULL, s, ap);
}

void
error_for_asm VPROTO((rtx insn, char *s, ...))
{
#ifndef __STDC__
  rtx insn;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  insn = va_arg (ap, rtx);
  s = va_arg (ap, char *);
#endif

  v_error_for_asm (insn, s, ap);
  va_end (ap);
}

/* Report an error at the current line number.  */

static void
verror (s, ap)
     char *s;
     va_list ap;
{
  v_error_with_file_and_line (input_filename, lineno, s, ap);
}

void
error VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  verror (s, ap);
  va_end (ap);
}

/* Report a fatal error at the current line number.  */

static void
vfatal (s, ap)
     char *s;
     va_list ap;
{
  verror (s, ap);
  exit (FATAL_EXIT_CODE);
}

void
fatal VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  vfatal (s, ap);
  va_end (ap);
}

/* Report a warning at line LINE of file FILE.  */

static void
v_warning_with_file_and_line (file, line, s, ap)
     char *file;
     int line;
     char *s;
     va_list ap;
{
  if (count_error (1))
    {
      report_error_function (file);
      v_message_with_file_and_line (file, line, "warning", s, ap);
    }
}

void
warning_with_file_and_line VPROTO((char *file, int line, char *s, ...))
{
#ifndef __STDC__
  char *file;
  int line;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  file = va_arg (ap, char *);
  line = va_arg (ap, int);
  s = va_arg (ap, char *);
#endif

  v_warning_with_file_and_line (file, line, s, ap);
  va_end (ap);
}

/* Report a warning at the declaration DECL.
   S is a format string which uses %s to substitute the declaration
   name; subsequent substitutions are a la printf.  */

static void
v_warning_with_decl (decl, s, ap)
     tree decl;
     char *s;
     va_list ap;
{
  if (count_error (1))
    {
      report_error_function (DECL_SOURCE_FILE (decl));
      v_message_with_decl (decl, "warning", s, ap);
    }
}

void
warning_with_decl VPROTO((tree decl, char *s, ...))
{
#ifndef __STDC__
  tree decl;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  decl = va_arg (ap, tree);
  s = va_arg (ap, char *);
#endif

  v_warning_with_decl (decl, s, ap);
  va_end (ap);
}

/* Report a warning at the line number of the insn INSN.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

static void
v_warning_for_asm (insn, s, ap)
     rtx insn;
     char *s;
     va_list ap;
{
  if (count_error (1))
    {
      char *file;
      int line;

      file_and_line_for_asm (insn, &file, &line);
      report_error_function (file);
      v_message_with_file_and_line (file, line, "warning", s, ap);
    }
}

void
warning_for_asm VPROTO((rtx insn, char *s, ...))
{
#ifndef __STDC__
  rtx insn;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  insn = va_arg (ap, rtx);
  s = va_arg (ap, char *);
#endif

  v_warning_for_asm (insn, s, ap);
  va_end (ap);
}

/* Report a warning at the current line number.  */

static void
vwarning (s, ap)
     char *s;
     va_list ap;
{
  v_warning_with_file_and_line (input_filename, lineno, s, ap);
}

void
warning VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  vwarning (s, ap);
  va_end (ap);
}

/* These functions issue either warnings or errors depending on
   -pedantic-errors.  */

static void
vpedwarn (s, ap)
     char *s;
     va_list ap;
{
  if (flag_pedantic_errors)
    verror (s, ap);
  else
    vwarning (s, ap);
}

void
pedwarn VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  vpedwarn (s, ap);
  va_end (ap);
}

static void
v_pedwarn_with_decl (decl, s, ap)
     tree decl;
     char *s;
     va_list ap;
{
  /* We don't want -pedantic-errors to cause the compilation to fail from
     "errors" in system header files.  Sometimes fixincludes can't fix what's
     broken (eg: unsigned char bitfields - fixing it may change the alignment
     which will cause programs to mysteriously fail because the C library
     or kernel uses the original layout).  There's no point in issuing a
     warning either, it's just unnecessary noise.  */

  if (! DECL_IN_SYSTEM_HEADER (decl))
    {
      if (flag_pedantic_errors)
	v_error_with_decl (decl, s, ap);
      else
	v_warning_with_decl (decl, s, ap);
    }
}

void
pedwarn_with_decl VPROTO((tree decl, char *s, ...))
{
#ifndef __STDC__
  tree decl;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  decl = va_arg (ap, tree);
  s = va_arg (ap, char *);
#endif

  v_pedwarn_with_decl (decl, s, ap);
  va_end (ap);
}

static void
v_pedwarn_with_file_and_line (file, line, s, ap)
     char *file;
     int line;
     char *s;
     va_list ap;
{
  if (flag_pedantic_errors)
    v_error_with_file_and_line (file, line, s, ap);
  else
    v_warning_with_file_and_line (file, line, s, ap);
}

void
pedwarn_with_file_and_line VPROTO((char *file, int line, char *s, ...))
{
#ifndef __STDC__
  char *file;
  int line;
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  file = va_arg (ap, char *);
  line = va_arg (ap, int);
  s = va_arg (ap, char *);
#endif

  v_pedwarn_with_file_and_line (file, line, s, ap);
  va_end (ap);
}

/* Apologize for not implementing some feature.  */

static void
vsorry (s, ap)
     char *s;
     va_list ap;
{
  sorrycount++;
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);
  vmessage ("sorry, not implemented", s, ap);
  fputc ('\n', stderr);
}

void
sorry VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  vsorry (s, ap);
  va_end (ap);
}

/* Apologize for not implementing some feature, then quit.  */

static void
v_really_sorry (s, ap)
     char *s;
     va_list ap;
{
  sorrycount++;
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);
  vmessage ("sorry, not implemented", s, ap);
  fatal (" (fatal)\n");
}

void
really_sorry VPROTO((char *s, ...))
{
#ifndef __STDC__
  char *s;
#endif
  va_list ap;

  VA_START (ap, s);

#ifndef __STDC__
  s = va_arg (ap, char *);
#endif

  v_really_sorry (s, ap);
  va_end (ap);
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
  if (value == 0 && size != 0)
    fatal ("virtual memory exhausted");
  return value;
}

/* Same as `realloc' but report error if no memory available.  
   Also handle null PTR even if the vendor realloc gets it wrong.  */

char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  char *result = (ptr
		  ? (char *) realloc (ptr, size)
		  : (char *) malloc (size));
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

/* Same as `strdup' but report error if no memory available.  */

char *
xstrdup (s)
     register char *s;
{
  register char *result = (char *) malloc (strlen (s) + 1);

  if (! result)
    fatal ("virtual memory exhausted");
  strcpy (result, s);
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

static int float_handler_set;
int float_handled;
jmp_buf float_handler;

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

/* Specify where to longjmp to when a floating arithmetic error happens.
   If HANDLER is 0, it means don't handle the errors any more.  */

void
set_float_handler (handler)
     jmp_buf handler;
{
  float_handled = (handler != 0);
  if (handler)
    bcopy ((char *) handler, (char *) float_handler, sizeof (float_handler));

  if (float_handled && ! float_handler_set)
    {
      signal (SIGFPE, float_signal);
      float_handler_set = 1;
    }
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
    bcopy ((char *) float_handler, (char *) old_handler,
	   sizeof (float_handler));

  bcopy ((char *) handler, (char *) float_handler, sizeof (float_handler));
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
    bcopy ((char *) handler, (char *) float_handler, sizeof (float_handler));
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
   length LEN.  Rather than having to know the names used by all of
   our front ends, we strip off an ending of a period followed by one,
   two, or three characters.  */

void
strip_off_ending (name, len)
     char *name;
     int len;
{
  if (len > 2 && name[len - 2] == '.')
    name[len - 2] = '\0';
  else if (len > 3 && name[len - 3] == '.')
    name[len - 3] = '\0';
  else if (len > 4 && name[len - 4] == '.')
    name[len - 4] = '\0';
}

/* Output a quoted string.  */

void
output_quoted_string (asm_file, string)
     FILE *asm_file;
     char *string;
{
#ifdef OUTPUT_QUOTED_STRING
  OUTPUT_QUOTED_STRING (asm_file, string);
#else
  char c;

  putc ('\"', asm_file);
  while ((c = *string++) != 0)
    {
      if (c == '\"' || c == '\\')
	putc ('\\', asm_file);
      putc (c, asm_file);
    }
  putc ('\"', asm_file);
#endif
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
  fprintf (asm_file, "\t.file\t");
  output_quoted_string (asm_file, na);
  fputc ('\n', asm_file);
#endif
#endif
}

/* Routine to build language identifier for object file.  */

static void
output_lang_identify (asm_out_file)
     FILE *asm_out_file;
{
  int len = strlen (lang_identify ()) + sizeof ("__gnu_compiled_") + 1;
  char *s = (char *) alloca (len);
  sprintf (s, "__gnu_compiled_%s", lang_identify ());
  ASM_OUTPUT_LABEL (asm_out_file, s);
}

/* Routine to open a dump file.  */

static FILE *
open_dump_file (base_name, suffix)
     char *base_name;
     char *suffix;
{
  FILE *f;
  char *dumpname = (char *) alloca (strlen (base_name) + strlen (suffix) + 1);

  strcpy (dumpname, base_name);
  strcat (dumpname, suffix);
  f = fopen (dumpname, "w");
  if (f == 0)
    pfatal_with_name (dumpname);
  return f;
}

/* Compile an entire file of output from cpp, named NAME.
   Write a file of assembly output and various debugging dumps.  */

static void
compile_file (name)
     char *name;
{
  tree globals;
  int start_time;

  int name_specified = name != 0;

  if (dump_base_name == 0)
    dump_base_name = name ? name : "gccdump";

  parse_time = 0;
  varconst_time = 0;
  integration_time = 0;
  jump_time = 0;
  cse_time = 0;
  loop_time = 0;
  cse2_time = 0;
  branch_prob_time = 0;
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

#if !USE_CPPLIB
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
#endif /* !USE_CPPLIB */

  /* Initialize data in various passes.  */

  init_obstacks ();
  init_tree_codes ();
#if USE_CPPLIB
  init_parse (name);
#else
  init_lex ();
#endif
  /* Some of these really don't need to be called when generating bytecode,
     but the options would have to be parsed first to know that. -bson */
  init_rtl ();
  init_emit_once (debug_info_level == DINFO_LEVEL_NORMAL
		  || debug_info_level == DINFO_LEVEL_VERBOSE
		  || flag_test_coverage);
  init_regs ();
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
    rtl_dump_file = open_dump_file (dump_base_name, ".rtl");

  /* If jump_opt dump desired, open the output file.  */
  if (jump_opt_dump)
    jump_opt_dump_file = open_dump_file (dump_base_name, ".jump");

  /* If addressof dump desired, open the output file.  */
  if (addressof_dump)
    addressof_dump_file = open_dump_file (dump_base_name, ".addressof");

  /* If cse dump desired, open the output file.  */
  if (cse_dump)
    cse_dump_file = open_dump_file (dump_base_name, ".cse");

  /* If loop dump desired, open the output file.  */
  if (loop_dump)
    loop_dump_file = open_dump_file (dump_base_name, ".loop");

  /* If cse2 dump desired, open the output file.  */
  if (cse2_dump)
    cse2_dump_file = open_dump_file (dump_base_name, ".cse2");

  /* If branch_prob dump desired, open the output file.  */
  if (branch_prob_dump)
    branch_prob_dump_file = open_dump_file (dump_base_name, ".bp");

  /* If flow dump desired, open the output file.  */
  if (flow_dump)
    flow_dump_file = open_dump_file (dump_base_name, ".flow");

  /* If combine dump desired, open the output file.  */
  if (combine_dump)
    combine_dump_file = open_dump_file (dump_base_name, ".combine");

  /* If scheduling dump desired, open the output file.  */
  if (sched_dump)
    sched_dump_file = open_dump_file (dump_base_name, ".sched");

  /* If local_reg dump desired, open the output file.  */
  if (local_reg_dump)
    local_reg_dump_file = open_dump_file (dump_base_name, ".lreg");

  /* If global_reg dump desired, open the output file.  */
  if (global_reg_dump)
    global_reg_dump_file = open_dump_file (dump_base_name, ".greg");

  /* If 2nd scheduling dump desired, open the output file.  */
  if (sched2_dump)
    sched2_dump_file = open_dump_file (dump_base_name, ".sched2");

  /* If jump2_opt dump desired, open the output file.  */
  if (jump2_opt_dump)
    jump2_opt_dump_file = open_dump_file (dump_base_name, ".jump2");

  /* If dbr_sched dump desired, open the output file.  */
  if (dbr_sched_dump)
    dbr_sched_dump_file = open_dump_file (dump_base_name, ".dbr");

#ifdef STACK_REGS

  /* If stack_reg dump desired, open the output file.  */
  if (stack_reg_dump)
    stack_reg_dump_file = open_dump_file (dump_base_name, ".stack");

#endif

  /* Open assembler code output file.  */

  if (! name_specified && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      int len = strlen (dump_base_name);
      register char *dumpname = (char *) xmalloc (len + 6);
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

  /* Put an entry on the input file stack for the main input file.  */
  input_file_stack
    = (struct file_stack *) xmalloc (sizeof (struct file_stack));
  input_file_stack->next = 0;
  input_file_stack->name = input_filename;

  /* Gross. Gross.  lang_init is (I think) the first callback into
     the language front end, and is thus the first opportunity to
     have the selected language override the default value for any
     -f option.

     So the default value for flag_exceptions is 2 (uninitialized).
     If we encounter -fno-exceptions or -fexceptions, then flag_exceptions
     will be set to zero or one respectively.

     flag_exceptions can also be set by lang_init to something other
     than the default "uninitialized" value of 2.

     After lang_init, if the value is still 2, then we default to
     -fno-exceptions (value will be reset to zero).

     When our EH mechanism is low enough overhead that we can enable
     it by default for languages other than C++, then all this braindamage
     will go away.  */
  
  /* Perform language-specific initialization.
     This may set main_input_filename.  */
  lang_init ();

  if (flag_exceptions == 2)
    flag_exceptions = 0;
     
  /* If the input doesn't start with a #line, use the input name
     as the official input file name.  */
  if (main_input_filename == 0)
    main_input_filename = name;

  if (!output_bytecode)
    {
      ASM_FILE_START (asm_out_file);

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

  /* Output something to inform GDB that this compilation was by GCC.  Also
     serves to tell GDB file consists of bytecodes.  */
  if (output_bytecode)
    fprintf (asm_out_file, "bc_gcc2_compiled.:\n");
  else
    {
#ifndef ASM_IDENTIFY_GCC
      fprintf (asm_out_file, "gcc2_compiled.:\n");
#else
      ASM_IDENTIFY_GCC (asm_out_file);
#endif
    }

  /* Output something to identify which front-end produced this file.  */
#ifdef ASM_IDENTIFY_LANGUAGE
  ASM_IDENTIFY_LANGUAGE (asm_out_file);
#endif

#ifndef ASM_OUTPUT_SECTION_NAME
  if (flag_function_sections)
    {
      warning ("-ffunction-sections not supported for this target.");
      flag_function_sections = 0;
    }
#endif

  if (flag_function_sections
      && (profile_flag || profile_block_flag))
    {
      warning ("-ffunction-sections disabled; it makes profiling impossible.");
      flag_function_sections = 0;
    }

  if (flag_function_sections && write_symbols != NO_DEBUG)
    warning ("-ffunction-sections may affect debugging on some targets.");

  if (output_bytecode)
    {
      if (profile_flag || profile_block_flag)
	error ("profiling not supported in bytecode compilation");
    }
  else
    {
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
	{
	  /* It's best if we can write a nop here since some
	     assemblers don't tolerate zeros in the text section.  */
	  if (insn_template[CODE_FOR_nop] != 0)
	    output_asm_insn (insn_template[CODE_FOR_nop], NULL_PTR);
	  else
	    assemble_zeros (UNITS_PER_WORD);
	}
    }

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
#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_init ();
#endif
#ifdef DWARF2_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG)
    TIMEVAR (symout_time, dwarf2out_init (asm_out_file, main_input_filename));
#endif

  /* Initialize yet another pass.  */

  if (!output_bytecode)
    init_final (main_input_filename);
  init_branch_prob (dump_base_name);

  start_time = get_run_time ();

  /* Call the parser, which parses the entire file
     (calling rest_of_compilation for each function).  */

  if (yyparse () != 0)
    {
      if (errorcount == 0)
	fprintf (stderr, "Errors detected in input file (your bison.simple is out of date)");

      /* In case there were missing closebraces,
	 get us back to the global binding level.  */
      while (! global_bindings_p ())
	poplevel (0, 0, 0);
    }

  output_func_start_profiler ();

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
    int reconsider = 1;

    /* Process the decls in reverse order--earliest first.
       Put them into VEC from back to front, then take out from front.  */

    for (i = 0, decl = globals; i < len; i++, decl = TREE_CHAIN (decl))
      vec[len - i - 1] = decl;

    for (i = 0; i < len; i++)
      {
	decl = vec[i];

	/* We're not deferring this any longer.  */
	DECL_DEFER_OUTPUT (decl) = 0;

	if (TREE_CODE (decl) == VAR_DECL && DECL_SIZE (decl) == 0
	    && incomplete_decl_finalize_hook != 0)
	  (*incomplete_decl_finalize_hook) (decl);
      }

    /* Now emit any global variables or functions that we have been putting
       off.  We need to loop in case one of the things emitted here
       references another one which comes earlier in the list.  */
    while (reconsider)
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

	    if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl)
		&& (! TREE_READONLY (decl)
		    || TREE_PUBLIC (decl)
		    || (!optimize && flag_keep_static_consts)
		    || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	      {
		reconsider = 1;
		rest_of_decl_compilation (decl, NULL_PTR, 1, 1);
	      }

	    if (TREE_CODE (decl) == FUNCTION_DECL
		&& DECL_INITIAL (decl) != 0
		&& DECL_SAVED_INSNS (decl) != 0
		&& (flag_keep_inline_functions
		    || TREE_PUBLIC (decl)
		    || TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl))))
	      {
		reconsider = 1;
		temporary_allocation ();
		output_inline_function (decl);
		permanent_allocation (1);
	      }
	  }
      }

    /* Now that all possible functions have been output, we can dump
       the exception table.  */

    output_exception_table ();

    for (i = 0; i < len; i++)
      {
	decl = vec[i];

	if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl)
	    && ! TREE_ASM_WRITTEN (decl))
	  /* Cancel the RTL for this decl so that, if debugging info
	     output for global variables is still to come,
	     this one will be omitted.  */
	  DECL_RTL (decl) = NULL;

	/* Warn about any function
	   declared static but not defined.
	   We don't warn about variables,
	   because many programs have static variables
	   that exist only to get some text into the object file.  */
	if (TREE_CODE (decl) == FUNCTION_DECL
	    && (warn_unused
		|| TREE_SYMBOL_REFERENCED (DECL_ASSEMBLER_NAME (decl)))
	    && DECL_INITIAL (decl) == 0
	    && DECL_EXTERNAL (decl)
	    && ! DECL_ARTIFICIAL (decl)
	    && ! TREE_PUBLIC (decl))
	  {
	    pedwarn_with_decl (decl, 
			       "`%s' declared `static' but never defined");
	    /* This symbol is effectively an "extern" declaration now.  */
	    TREE_PUBLIC (decl) = 1;
	    assemble_external (decl);
	  }

	/* Warn about static fns or vars defined but not used,
	   but not about inline functions or static consts
	   since defining those in header files is normal practice.  */
	if (warn_unused
	    && ((TREE_CODE (decl) == FUNCTION_DECL && ! DECL_INLINE (decl))
		|| (TREE_CODE (decl) == VAR_DECL && ! TREE_READONLY (decl)))
	    && ! DECL_IN_SYSTEM_HEADER (decl)
	    && ! DECL_EXTERNAL (decl)
	    && ! TREE_PUBLIC (decl)
	    && ! TREE_USED (decl)
	    && (TREE_CODE (decl) == FUNCTION_DECL || ! DECL_REGISTER (decl))
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
	    && ! DECL_EXTERNAL (decl)
	    && DECL_RTL (decl) != 0)
	  TIMEVAR (symout_time, sdbout_symbol (decl, 0));

	/* Output COFF information for non-global
	   file-scope initialized variables.  */
	if (write_symbols == SDB_DEBUG
	    && TREE_CODE (decl) == VAR_DECL
	    && DECL_INITIAL (decl)
	    && ! DECL_EXTERNAL (decl)
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
#ifdef DWARF2_DEBUGGING_INFO
	/* Output DWARF2 information for file-scope tentative data object
	   declarations, file-scope (extern) function declarations (which
	   had no corresponding body) and file-scope tagged type declarations
	   and definitions which have not yet been forced out.  */

	if (write_symbols == DWARF2_DEBUG
	    && (TREE_CODE (decl) != FUNCTION_DECL || !DECL_INITIAL (decl)))
	  TIMEVAR (symout_time, dwarf2out_decl (decl));
#endif
      }
  }

  /* Write out any pending weak symbol declarations.  */

  weak_finish ();

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

#ifdef DWARF2_UNWIND_INFO
  if (dwarf2out_do_frame ())
    dwarf2out_frame_finish ();
#endif

#ifdef DWARF2_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG)
    TIMEVAR (symout_time,
	     {
	       dwarf2out_finish ();
	     });
#endif

  /* Output some stuff at end of file if nec.  */

  if (!output_bytecode)
    {
      end_final (dump_base_name);
      end_branch_prob (branch_prob_dump_file);

#ifdef ASM_FILE_END
      ASM_FILE_END (asm_out_file);
#endif
    }

  /* Language-specific end of compilation actions.  */

  lang_finish ();

  if (output_bytecode)
    bc_write_file (asm_out_file);

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

  if (addressof_dump)
    fclose (addressof_dump_file);

  if (cse_dump)
    fclose (cse_dump_file);

  if (loop_dump)
    fclose (loop_dump_file);

  if (cse2_dump)
    fclose (cse2_dump_file);

  if (branch_prob_dump)
    fclose (branch_prob_dump_file);

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

#if USE_CPPLIB
  finish_parse ();
#else
  fclose (finput);
#endif
  if (ferror (asm_out_file) != 0 || fclose (asm_out_file) != 0)
    fatal_io_error (asm_file_name);

  /* Print the times.  */

  if (! quiet_flag)
    {
      fprintf (stderr,"\n");
      print_time ("parse", parse_time);

      if (!output_bytecode)
	{
	  print_time ("integration", integration_time);
	  print_time ("jump", jump_time);
	  print_time ("cse", cse_time);
	  print_time ("loop", loop_time);
	  print_time ("cse2", cse2_time);
	  print_time ("branch-prob", branch_prob_time);
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

/* The most obvious approach, to put an #ifndef around where
   this macro is used, doesn't work since it's inside a macro call.  */
#ifndef ASM_FINISH_DECLARE_OBJECT
#define ASM_FINISH_DECLARE_OBJECT(FILE, DECL, TOP, END)
#endif

  /* Forward declarations for nested functions are not "external",
     but we need to treat them as if they were.  */
  if (TREE_STATIC (decl) || DECL_EXTERNAL (decl)
      || TREE_CODE (decl) == FUNCTION_DECL)
    TIMEVAR (varconst_time,
	     {
	       make_decl_rtl (decl, asmspec, top_level);
	       /* Initialized extern variable exists to be replaced
		  with its value, or represents something that will be
		  output in another file.  */
	       if (! (TREE_CODE (decl) == VAR_DECL
		      && DECL_EXTERNAL (decl) && TREE_READONLY (decl)
		      && DECL_INITIAL (decl) != 0
		      && DECL_INITIAL (decl) != error_mark_node))
		 /* Don't output anything
		    when a tentative file-scope definition is seen.
		    But at end of compilation, do output code for them.  */
		 if (! (! at_end && top_level
			&& (DECL_INITIAL (decl) == 0
			    || DECL_INITIAL (decl) == error_mark_node)))
		   assemble_variable (decl, top_level, at_end, 0);
	       if (!output_bytecode
		   && decl == last_assemble_variable_decl)
		 {
		   ASM_FINISH_DECLARE_OBJECT (asm_out_file, decl,
					      top_level, at_end);
		 }
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

  if (output_bytecode)
    return;

  /* If we are reconsidering an inline function
     at the end of compilation, skip the stuff for making it inline.  */

  if (DECL_SAVED_INSNS (decl) == 0)
    {
      int inlinable = 0;
      char *lose;

      /* If requested, consider whether to make this function inline.  */
      if (DECL_INLINE (decl) || flag_inline_functions)
	TIMEVAR (integration_time,
		 {
		   lose = function_cannot_inline_p (decl);
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
		     /* ??? Note that this has the effect of making it look
			like "inline" was specified for a function if we choose
			to inline it.  This isn't quite right, but it's
			probably not worth the trouble to fix.  */
		     inlinable = DECL_INLINE (decl) = 1;
		 });

      insns = get_insns ();

      /* Dump the rtl code if we are dumping rtl.  */

      if (rtl_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (rtl_dump_file, "\n;; Function %s\n\n",
			    (*decl_printable_name) (decl, 2));
		   if (DECL_SAVED_INSNS (decl))
		     fprintf (rtl_dump_file, ";; (integrable)\n\n");
		   print_rtl (rtl_dump_file, insns);
		   if (! quiet_flag)
		     fflush (rtl_dump_file);
		 });

      /* If function is inline, and we don't yet know whether to
	 compile it by itself, defer decision till end of compilation.
	 finish_compilation will call rest_of_compilation again
	 for those functions that need to be output.  Also defer those
	 functions that we are supposed to defer.  We cannot defer
	 functions containing nested functions since the nested function
	 data is in our non-saved obstack.  */

      /* If this is a nested inline, remove ADDRESSOF now so we can
	 finish compiling ourselves.  Otherwise, wait until EOF.
	 We have to do this because the purge_addressof transformation
	 changes the DECL_RTL for many variables, which confuses integrate.  */
      if (inlinable)
	{
	  if (decl_function_context (decl))
	    purge_addressof (insns);
	  else
	    DECL_DEFER_OUTPUT (decl) = 1;
	}

      if (! current_function_contains_functions
	  && (DECL_DEFER_OUTPUT (decl)
	      || (DECL_INLINE (decl)
		  && ((! TREE_PUBLIC (decl) && ! TREE_ADDRESSABLE (decl)
		       && ! flag_keep_inline_functions)
		      || DECL_EXTERNAL (decl)))))
	{
	  DECL_DEFER_OUTPUT (decl) = 1;

	  /* If -Wreturn-type, we have to do a bit of compilation.  */
	  if (! warn_return_type)
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
#ifdef DWARF2_DEBUGGING_INFO
	      /* Generate the DWARF2 info for the "abstract" instance
		 of a function which we may later generate inlined and/or
		 out-of-line instances of.  */
	      if (write_symbols == DWARF2_DEBUG)
		{
		  set_decl_abstract_flags (decl, 1);
		  TIMEVAR (symout_time, dwarf2out_decl (decl));
		  set_decl_abstract_flags (decl, 0);
		}
#endif
	      TIMEVAR (integration_time, save_for_inline_nocopy (decl));
	      RTX_INTEGRATED_P (DECL_SAVED_INSNS (decl)) = inlinable;
	      goto exit_rest_of_compilation;
	    }
	}

      /* If we have to compile the function now, save its rtl and subdecls
	 so that its compilation will not affect what others get.  */
      if (inlinable || DECL_DEFER_OUTPUT (decl))
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
#ifdef DWARF2_DEBUGGING_INFO
	  /* Generate the DWARF2 info for the "abstract" instance of
	     a function which we will generate an out-of-line instance
	     of almost immediately (and which we may also later generate
	     various inlined instances of).  */
	  if (write_symbols == DWARF2_DEBUG)
	    {
	      set_decl_abstract_flags (decl, 1);
	      TIMEVAR (symout_time, dwarf2out_decl (decl));
	      set_decl_abstract_flags (decl, 0);
	    }
#endif
	  saved_block_tree = DECL_INITIAL (decl);
	  saved_arguments = DECL_ARGUMENTS (decl);
	  TIMEVAR (integration_time, save_for_inline_copying (decl));
	  RTX_INTEGRATED_P (DECL_SAVED_INSNS (decl)) = inlinable;
	}

      /* If specified extern inline but we aren't inlining it, we are
	 done.  */
      if (DECL_INLINE (decl) && DECL_EXTERNAL (decl))
	goto exit_rest_of_compilation;
    }

  if (! DECL_DEFER_OUTPUT (decl))
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

  /* Add an unwinder for exception handling, if needed.
     This must be done before we finalize PIC code.  */
  emit_unwinder ();

#ifdef FINALIZE_PIC
  /* If we are doing position-independent code generation, now
     is the time to output special prologues and epilogues.
     We do not want to do this earlier, because it just clutters
     up inline functions with meaningless insns.  */
  if (flag_pic)
    FINALIZE_PIC;
#endif

  /* From now on, allocate rtl in current_obstack, not in saveable_obstack.
     Note that that may have been done above, in save_for_inline_copying.
     The call to resume_temporary_allocation near the end of this function
     goes back to the usual state of affairs.  This must be done after
     we've built up any unwinders for exception handling, and done
     the FINALIZE_PIC work, if necessary.  */

  rtl_in_current_obstack ();

  insns = get_insns ();

  /* Copy any shared structure that should not be shared.  */

  unshare_all_rtl (insns);

  /* Instantiate all virtual registers.  */

  instantiate_virtual_regs (current_function_decl, get_insns ());

  /* See if we have allocated stack slots that are not directly addressable.
     If so, scan all the insns and create explicit address computation
     for all references to such slots.  */
/*   fixup_stack_slots (); */

  /* Find all the EH handlers.  */
  find_exception_handler_labels ();

  /* Always do one jump optimization pass to ensure that JUMP_LABEL fields
     are initialized and to compute whether control can drop off the end
     of the function.  */
  TIMEVAR (jump_time, reg_scan (insns, max_reg_num (), 0));
  TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 1));

  /* Now is when we stop if -fsyntax-only and -Wreturn-type.  */
  if (rtl_dump_and_exit || flag_syntax_only || DECL_DEFER_OUTPUT (decl))
    goto exit_rest_of_compilation;

  /* Dump rtl code after jump, if we are doing that.  */

  if (jump_opt_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (jump_opt_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
	       print_rtl (jump_opt_dump_file, insns);
	       if (! quiet_flag)
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
			(*decl_printable_name) (decl, 2));
	     });

  if (optimize > 0)
    {
      TIMEVAR (cse_time, reg_scan (insns, max_reg_num (), 1));

      if (flag_thread_jumps)
	/* Hacks by tiemann & kenner.  */
	TIMEVAR (jump_time, thread_jumps (insns, max_reg_num (), 1));

      TIMEVAR (cse_time, tem = cse_main (insns, max_reg_num (),
					 0, cse_dump_file));
      TIMEVAR (cse_time, delete_dead_from_cse (insns, max_reg_num ()));

      if (tem || optimize > 1)
	TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 0));
    }

  /* Dump rtl code after cse, if we are doing that.  */

  if (cse_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (cse_dump_file, insns);
	       if (! quiet_flag)
		 fflush (cse_dump_file);
	     });

  purge_addressof (insns);
  reg_scan (insns, max_reg_num (), 1);

  if (addressof_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (addressof_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
	       print_rtl (addressof_dump_file, insns);
	       if (! quiet_flag)
		 fflush (addressof_dump_file);
	     });

  if (loop_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (loop_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
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
	       if (! quiet_flag)
		 fflush (loop_dump_file);
	     });

  if (cse2_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (cse2_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
	     });

  if (optimize > 0 && flag_rerun_cse_after_loop)
    {
      /* Running another jump optimization pass before the second
	 cse pass sometimes simplifies the RTL enough to allow
	 the second CSE pass to do a better job.  Jump_optimize can change
	 max_reg_num so we must rerun reg_scan afterwards.
	 ??? Rework to not call reg_scan so often.  */
      TIMEVAR (jump_time, reg_scan (insns, max_reg_num (), 0));
      TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 1));

      TIMEVAR (cse2_time, reg_scan (insns, max_reg_num (), 0));
      TIMEVAR (cse2_time, tem = cse_main (insns, max_reg_num (),
					  1, cse2_dump_file));
      if (tem)
	TIMEVAR (jump_time, jump_optimize (insns, 0, 0, 0));
    }

  if (optimize > 0 && flag_thread_jumps)
    {
      /* This pass of jump threading straightens out code
         that was kinked by loop optimization.  */
      TIMEVAR (jump_time, reg_scan (insns, max_reg_num (), 0));
      TIMEVAR (jump_time, thread_jumps (insns, max_reg_num (), 0));
    }
  /* Dump rtl code after cse, if we are doing that.  */

  if (cse2_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (cse2_dump_file, insns);
	       if (! quiet_flag)
		 fflush (cse2_dump_file);
	     });

  if (branch_prob_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (branch_prob_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
	     });

  if (profile_arc_flag || flag_test_coverage || flag_branch_probabilities)
    TIMEVAR (branch_prob_time,
	     {
	       branch_prob (insns, branch_prob_dump_file);
	     });

  if (branch_prob_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (branch_prob_dump_file, insns);
	       if (! quiet_flag)
		 fflush (branch_prob_dump_file);
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
			(*decl_printable_name) (decl, 2));
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
	       print_rtl_with_bb (flow_dump_file, insns);
	       if (! quiet_flag)
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
			(*decl_printable_name) (decl, 2));
	       dump_combine_stats (combine_dump_file);
	       print_rtl_with_bb (combine_dump_file, insns);
	       if (! quiet_flag)
		 fflush (combine_dump_file);
	     });

  /* Print function header into sched dump now
     because doing the sched analysis makes some of the dump.  */

  if (sched_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (sched_dump_file, "\n;; Function %s\n\n",
			(*decl_printable_name) (decl, 2));
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
	       print_rtl_with_bb (sched_dump_file, insns);
	       if (! quiet_flag)
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
			(*decl_printable_name) (decl, 2));
	       dump_flow_info (local_reg_dump_file);
	       dump_local_alloc (local_reg_dump_file);
	       print_rtl_with_bb (local_reg_dump_file, insns);
	       if (! quiet_flag)
		 fflush (local_reg_dump_file);
	     });

  if (global_reg_dump)
    TIMEVAR (dump_time,
	     fprintf (global_reg_dump_file, "\n;; Function %s\n\n",
		      (*decl_printable_name) (decl, 2)));

  /* Save the last label number used so far, so reorg can tell
     when it's safe to kill spill regs.  */
  max_label_num_after_reload = max_label_num ();

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
	       print_rtl_with_bb (global_reg_dump_file, insns);
	       if (! quiet_flag)
		 fflush (global_reg_dump_file);
	     });

  if (failure)
    goto exit_rest_of_compilation;

  reload_completed = 1;

  /* Do a very simple CSE pass over just the hard registers.  */
  if (optimize > 0)
    reload_cse_regs (insns);

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
			    (*decl_printable_name) (decl, 2));
		 });

      /* Do control and data sched analysis again,
	 and write some more of the results to dump file.  */

      TIMEVAR (sched2_time, schedule_insns (sched2_dump_file));

      /* Dump rtl after post-reorder instruction scheduling.  */

      if (sched2_dump)
	TIMEVAR (dump_time,
		 {
		   print_rtl_with_bb (sched2_dump_file, insns);
		   if (! quiet_flag)
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
			(*decl_printable_name) (decl, 2));
	       print_rtl_with_bb (jump2_opt_dump_file, insns);
	       if (! quiet_flag)
		 fflush (jump2_opt_dump_file);
	     });

  /* If a machine dependent reorganization is needed, call it.  */
#ifdef MACHINE_DEPENDENT_REORG
   MACHINE_DEPENDENT_REORG (insns);
#endif

  /* If a scheduling pass for delayed branches is to be done,
     call the scheduling code.  */

#ifdef DELAY_SLOTS
  if (optimize > 0 && flag_delayed_branch)
    {
      TIMEVAR (dbr_sched_time, dbr_schedule (insns, dbr_sched_dump_file));
      if (dbr_sched_dump)
	{
	  TIMEVAR (dump_time,
		 {
		   fprintf (dbr_sched_dump_file, "\n;; Function %s\n\n",
			    (*decl_printable_name) (decl, 2));
		   print_rtl_with_bb (dbr_sched_dump_file, insns);
		   if (! quiet_flag)
		     fflush (dbr_sched_dump_file);
		 });
	}
    }
#endif

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
		          (*decl_printable_name) (decl, 2));
		 print_rtl_with_bb (stack_reg_dump_file, insns);
		 if (! quiet_flag)
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
	     if (! quiet_flag)
	       fflush (asm_out_file);

	     /* Release all memory held by regsets now */
	     regset_release_memory ();
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

#ifdef DWARF2_DEBUGGING_INFO
  if (write_symbols == DWARF2_DEBUG)
    TIMEVAR (symout_time, dwarf2out_decl (decl));
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
    {
      DECL_INITIAL (decl) = saved_block_tree;
      DECL_ARGUMENTS (decl) = saved_arguments;
      DECL_ABSTRACT_ORIGIN (decl) = NULL_TREE;
    }

  reload_completed = 0;

  /* Clear out the insn_length contents now that they are no longer valid.  */
  init_insn_lengths ();

  /* Clear out the real_constant_chain before some of the rtx's
     it runs through become garbage.  */

  clear_const_double_mem ();

  /* Cancel the effect of rtl_in_current_obstack.  */

  resume_temporary_allocation ();

  /* Show no temporary slots allocated.  */

  init_temp_slots ();

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
  while (p != argv[0] && p[-1] != '/'
#ifdef DIR_SEPARATOR
	 && p[-1] != DIR_SEPARATOR
#endif
	 )
    --p;
  progname = p;

#if defined (RLIMIT_STACK) && defined (HAVE_GETRLIMIT) && defined (HAVE_SETRLIMIT)
  /* Get rid of any avoidable limit on stack size.  */
  {
    struct rlimit rlim;

    /* Set the stack limit huge so that alloca does not fail.  */
    getrlimit (RLIMIT_STACK, &rlim);
    rlim.rlim_cur = rlim.rlim_max;
    setrlimit (RLIMIT_STACK, &rlim);
  }
#endif

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
    }

  if (optimize >= 2)
    {
      flag_cse_follow_jumps = 1;
      flag_cse_skip_blocks = 1;
      flag_expensive_optimizations = 1;
      flag_strength_reduce = 1;
      flag_rerun_cse_after_loop = 1;
      flag_caller_saves = 1;
      flag_force_mem = 1;
#ifdef INSN_SCHEDULING
      flag_schedule_insns = 1;
      flag_schedule_insns_after_reload = 1;
#endif
    }

  if (optimize >= 3)
    {
      flag_inline_functions = 1;
    }

  /* Initialize target_flags before OPTIMIZATION_OPTIONS so the latter can
     modify it.  */
  target_flags = 0;
  set_target_switch ("");

#ifdef OPTIMIZATION_OPTIONS
  /* Allow default optimizations to be specified on a per-machine basis.  */
  OPTIMIZATION_OPTIONS (optimize);
#endif

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

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
		    branch_prob_dump = 1;
 		    combine_dump = 1;
 		    dbr_sched_dump = 1;
 		    flow_dump = 1;
 		    global_reg_dump = 1;
 		    jump_opt_dump = 1;
 		    addressof_dump = 1;
 		    jump2_opt_dump = 1;
 		    local_reg_dump = 1;
 		    loop_dump = 1;
 		    rtl_dump = 1;
 		    cse_dump = 1, cse2_dump = 1;
 		    sched_dump = 1;
 		    sched2_dump = 1;
		    stack_reg_dump = 1;
		    break;
		  case 'b':
		    branch_prob_dump = 1;
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
		  case 'D':
		    addressof_dump = 1;
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
		  case 'A':
		    flag_debug_asm = 1;
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
	      /* We save the value of warn_uninitialized, since if they put
		 -Wuninitialized on the command line, we need to generate a
		 warning about not using it without also specifying -O.  */
	      if (warn_uninitialized != 1)
		warn_uninitialized = 2;
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
	      else if (!strncmp (p, "larger-than-", 12))
		{
		  char *endp = p + 12;

		  while (*endp)
		    {
		      if (*endp >= '0' && *endp <= '9')
			endp++;
		      else
			{
			  error ("Invalid option `%s'", argv[i]);
			  goto larger_than_lose;
			}
		    }
		  warn_larger_than = 1;
		  larger_than_size = atoi (str + 13);
		larger_than_lose: ;
		}
	      else
		error ("Invalid option `%s'", argv[i]);
	    }
	  else if (!strcmp (str, "p"))
	    {
	      profile_flag = 1;
	    }
	  else if (!strcmp (str, "a"))
	    {
#if !defined (BLOCK_PROFILER) || !defined (FUNCTION_BLOCK_PROFILER)
	      warning ("`-a' option (basic block profile) not supported");
#else
              profile_block_flag = (profile_block_flag < 2) ? 1 : 3;
#endif
	    }
	  else if (!strcmp (str, "ax"))
	    {
#if !defined (FUNCTION_BLOCK_PROFILER_EXIT) || !defined (BLOCK_PROFILER) || !defined (FUNCTION_BLOCK_PROFILER)
	      warning ("`-ax' option (jump profiling) not supported");
#else
	      profile_block_flag = (!profile_block_flag 
	                               || profile_block_flag == 2) ? 2 : 3;
#endif
	    }
	  else if (str[0] == 'g')
	    {
	      unsigned len;
	      unsigned level;
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
	      /* Table of supported debugging formats.  */
	      static struct {
		char *arg;
		/* Since PREFERRED_DEBUGGING_TYPE isn't necessarily a
		   constant expression, we use NO_DEBUG in its place.  */
		enum debug_info_type debug_type;
		int use_extensions_p;
	      } *da, debug_args[] = {
		{ "g", NO_DEBUG, DEFAULT_GDB_EXTENSIONS },
		{ "ggdb", NO_DEBUG, 1 },
#ifdef DBX_DEBUGGING_INFO
		{ "gstabs", DBX_DEBUG, 0 },
		{ "gstabs+", DBX_DEBUG, 1 },
#endif
#ifdef DWARF_DEBUGGING_INFO
		{ "gdwarf", DWARF_DEBUG, 0 },
		{ "gdwarf+", DWARF_DEBUG, 1 },
#endif
#ifdef DWARF2_DEBUGGING_INFO
		{ "gdwarf-2", DWARF2_DEBUG, 0 },
#endif
#ifdef XCOFF_DEBUGGING_INFO
		{ "gxcoff", XCOFF_DEBUG, 0 },
		{ "gxcoff+", XCOFF_DEBUG, 1 },
#endif
#ifdef SDB_DEBUGGING_INFO
		{ "gcoff", SDB_DEBUG, 0 },
#endif
		{ 0, 0, 0 }
	      };
	      /* Indexed by enum debug_info_type.  */
	      static char *debug_type_names[] = {
		"none", "stabs", "coff", "dwarf-1", "dwarf-2", "xcoff"
	      };

	      /* Look up STR in the table.  */
	      for (da = debug_args; da->arg; da++)
		{
		  if (! strncmp (str, da->arg, strlen (da->arg)))
		    {
		      enum debug_info_type type = da->debug_type;
		      char *p, *q;

		      p = str + strlen (da->arg);
		      if (*p && (*p < '0' || *p > '9'))
			continue;
		      len = p - str;
		      q = p;
		      while (*q && (*q >= '0' && *q <= '9'))
			q++;
		      if (*p)
			{
			  level = atoi (p);
			  if (len > 1 && !strncmp (str, "gdwarf", len))
			    {
			      error ("use -gdwarf -g%d for DWARF v1, level %d",
				       level, level);
			      if (level == 2)
				error ("use -gdwarf-2   for DWARF v2");
			    }
			}
		      else
			level = 2;	/* default debugging info level */
		      if (*q || level > 3)
			{
			  warning ("invalid debug level specification in option: `-%s'",
				   str);
			  /* ??? This error message is incorrect in the case of
			     -g4 -g.  */
			  warning ("no debugging information will be generated");
			  level = 0;
			}

		      if (type == NO_DEBUG)
			{
			  type = PREFERRED_DEBUGGING_TYPE;
			  if (len > 1 && strncmp (str, "ggdb", len) == 0)
			    {
#if defined (DWARF2_DEBUGGING_INFO) && !defined (LINKER_DOES_NOT_WORK_WITH_DWARF2)
			      type = DWARF2_DEBUG;
#else
#ifdef DBX_DEBUGGING_INFO
			      type = DBX_DEBUG;
#endif
#endif
			    }
			}

		      if (type == NO_DEBUG)
			warning ("`-%s' not supported by this configuration of GCC",
				 str);

		      /* Does it conflict with an already selected type?  */
		      if (type_explicitly_set_p
			  /* -g/-ggdb don't conflict with anything */
			  && da->debug_type != NO_DEBUG
			  && type != selected_debug_type)
			warning ("`-%s' ignored, conflicts with `-g%s'",
				 str, debug_type_names[(int) selected_debug_type]);
		      else
			{
			  /* If the format has already been set, -g/-ggdb
			     only change the debug level.  */
			  if (type_explicitly_set_p
			      && da->debug_type == NO_DEBUG)
			    ; /* don't change debugging type */
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
		warning ("`-%s' not supported by this configuration of GCC",
			 str);
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

  /* Checker uses the frame pointer.  */
  if (flag_check_memory_usage)
    flag_omit_frame_pointer = 0;

  /* Initialize for bytecode output.  A good idea to do this as soon as
     possible after the "-f" options have been parsed.  */
  if (output_bytecode)
    {
#ifndef TARGET_SUPPORTS_BYTECODE
      /* Just die with a fatal error if not supported */
      fatal ("-fbytecode not supported for this target");
#else
      bc_initialize ();
#endif
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

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

  if (exceptions_via_longjmp == 2)
    {
#ifdef DWARF2_UNWIND_INFO
      exceptions_via_longjmp = ! DWARF2_UNWIND_INFO;
#else
      exceptions_via_longjmp = 1;
#endif
    }

  if (profile_block_flag == 3)
    {
      warning ("`-ax' and `-a' are conflicting options. `-a' ignored.");
      profile_block_flag = 2;
    }

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
      print_version (stderr, "");
      if (! quiet_flag)
	print_switch_values (stderr, 0, MAX_LINE, "", " ", "\n");
    }

  compile_file (filename);

#if !defined(OS2) && !defined(VMS) && (!defined(_WIN32) || defined (__CYGWIN32__))
  if (flag_print_mem)
    {
      char *lim = (char *) sbrk (0);

      fprintf (stderr, "Data size %d.\n",
	       lim - (char *) &environ);
      fflush (stderr);

#ifndef __MSDOS__
#ifdef USG
      system ("ps -l 1>&2");
#else /* not USG */
      system ("ps v");
#endif /* not USG */
#endif
    }
#endif /* ! OS2 && ! VMS && (! _WIN32 || CYGWIN32) */

  if (errorcount)
    exit (FATAL_EXIT_CODE);
  if (sorrycount)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);
  return 0;
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

/* Print version information to FILE.
   Each line begins with INDENT (for the case where FILE is the
   assembler output file).  */

void
print_version (file, indent)
     FILE *file;
     char *indent;
{
  fprintf (file, "%s%s%s version %s", indent, *indent != 0 ? " " : "",
	   language_string, version_string);
  fprintf (file, " (%s)", TARGET_NAME);
#ifdef __GNUC__
#ifndef __VERSION__
#define __VERSION__ "[unknown]"
#endif
  fprintf (file, " compiled by GNU C version %s.\n", __VERSION__);
#else
  fprintf (file, " compiled by CC.\n");
#endif
}

/* Print an option value and return the adjusted position in the line.
   ??? We don't handle error returns from fprintf (disk full); presumably
   other code will catch a disk full though.  */

int
print_single_switch (file, pos, max, indent, sep, term, type, name)
     FILE *file;
     int pos, max;
     char *indent, *sep, *term, *type, *name;
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

void
print_switch_values (file, pos, max, indent, sep, term)
     FILE *file;
     int pos, max;
     char *indent, *sep, *term;
{
  int j, flags;
  char **p;

  /* Print the options as passed.  */

  pos = print_single_switch (file, pos, max, indent, *indent ? " " : "", term,
			     "options passed: ", "");

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
			     "options enabled: ", "");

  for (j = 0; j < sizeof f_options / sizeof f_options[0]; j++)
    if (*f_options[j].variable == f_options[j].on_value)
      pos = print_single_switch (file, pos, max, indent, sep, term,
				 "-f", f_options[j].string);

  /* Print target specific options.  */

  flags = target_flags;
  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (target_switches[j].name[0] != '\0'
	&& target_switches[j].value > 0
	&& ((target_switches[j].value & target_flags)
	    == target_switches[j].value))
      {
	pos = print_single_switch (file, pos, max, indent, sep, term,
				   "-m", target_switches[j].name);
	flags &= ~ target_switches[j].value;
      }

#ifdef TARGET_OPTIONS
  for (j = 0; j < sizeof target_options / sizeof target_options[0]; j++)
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

/* Record the beginning of a new source file, named FILENAME.  */

void
debug_start_source_file (filename)
     register char *filename;
{
#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    dbxout_start_new_source_file (filename);
#endif
#ifdef DWARF_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF_DEBUG)
    dwarfout_start_new_source_file (filename);
#endif /* DWARF_DEBUGGING_INFO */
#ifdef DWARF2_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF2_DEBUG)
    dwarf2out_start_source_file (filename);
#endif /* DWARF2_DEBUGGING_INFO */  
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_start_new_source_file (filename);
#endif
}

/* Record the resumption of a source file.  LINENO is the line number in
   the source file we are returning to.  */

void
debug_end_source_file (lineno)
     register unsigned lineno;
{
#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    dbxout_resume_previous_source_file ();
#endif
#ifdef DWARF_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF_DEBUG)
    dwarfout_resume_previous_source_file (lineno);
#endif /* DWARF_DEBUGGING_INFO */
#ifdef DWARF2_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF2_DEBUG)
    dwarf2out_end_source_file ();
#endif /* DWARF2_DEBUGGING_INFO */
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_resume_previous_source_file ();
#endif
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

void
debug_define (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
#ifdef DWARF_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF_DEBUG)
    dwarfout_define (lineno, buffer);
#endif /* DWARF_DEBUGGING_INFO */
#ifdef DWARF2_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF2_DEBUG)
    dwarf2out_define (lineno, buffer);
#endif /* DWARF2_DEBUGGING_INFO */
}

/* Called from check_newline in c-parse.y.  The `buffer' parameter contains
   the tail part of the directive line, i.e. the part which is past the
   initial whitespace, #, whitespace, directive-name, whitespace part.  */

void
debug_undef (lineno, buffer)
     register unsigned lineno;
     register char *buffer;
{
#ifdef DWARF_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF_DEBUG)
    dwarfout_undef (lineno, buffer);
#endif /* DWARF_DEBUGGING_INFO */
#ifdef DWARF2_DEBUGGING_INFO
  if (debug_info_level == DINFO_LEVEL_VERBOSE
      && write_symbols == DWARF2_DEBUG)
    dwarf2out_undef (lineno, buffer);
#endif /* DWARF2_DEBUGGING_INFO */
}
