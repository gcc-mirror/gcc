/* Compilation switch flag definitions for GCC.
   Copyright (C) 1987, 1988, 1994, 1995, 1996, 1997, 1998, 1999, 2000
   Free Software Foundation, Inc.

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

#ifndef GCC_FLAGS_H
#define GCC_FLAGS_H

/* Name of the input .c file being compiled.  */
extern const char *main_input_filename;

enum debug_info_type
{
  NO_DEBUG,	    /* Write no debug info.  */
  DBX_DEBUG,	    /* Write BSD .stabs for DBX (using dbxout.c).  */
  SDB_DEBUG,	    /* Write COFF for (old) SDB (using sdbout.c).  */
  DWARF_DEBUG,	    /* Write Dwarf debug info (using dwarfout.c).  */
  DWARF2_DEBUG,	    /* Write Dwarf v2 debug info (using dwarf2out.c).  */
  XCOFF_DEBUG,	    /* Write IBM/Xcoff debug info (using dbxout.c).  */
  VMS_DEBUG,        /* Write VMS debug info (using vmsdbgout.c).  */
  VMS_AND_DWARF2_DEBUG /* Write VMS debug info (using vmsdbgout.c).
                          and DWARF v2 debug info (using dwarf2out.c).  */
};

/* Specify which kind of debugging info to generate.  */
extern enum debug_info_type write_symbols;

enum debug_info_level
{
  DINFO_LEVEL_NONE,	/* Write no debugging info.  */
  DINFO_LEVEL_TERSE,	/* Write minimal info to support tracebacks only.  */
  DINFO_LEVEL_NORMAL,	/* Write info for all declarations (and line table).  */
  DINFO_LEVEL_VERBOSE	/* Write normal info plus #define/#undef info.  */
};

/* Specify how much debugging info to generate.  */
extern enum debug_info_level debug_info_level;

/* Nonzero means use GNU-only extensions in the generated symbolic
   debugging information.  */
extern int use_gnu_debug_info_extensions;

/* Nonzero means do optimizations.  -opt.  */

extern int optimize;

/* Nonzero means optimize for size.  -Os.  */

extern int optimize_size;

/* Don't print functions as they are compiled and don't print
   times taken by the various passes.  -quiet.  */

extern int quiet_flag;

/* Print times taken by the various passes.  -ftime-report.  */

extern int time_report;

/* Print memory still in use at end of compilation (which may have little
   to do with peak memory consumption).  -fmem-report.  */

extern int mem_report;

/* Don't print warning messages.  -w.  */

extern int inhibit_warnings;

/* Don't suppress warnings from system headers.  -Wsystem-headers.  */

extern int warn_system_headers;

/* Do print extra warnings (such as for uninitialized variables).  -W.  */

extern int extra_warnings;

/* Nonzero to warn about unused variables, functions et.al.  Use
   set_Wunused() to update the -Wunused-* flags that correspond to the
   -Wunused option.  */

extern void set_Wunused PARAMS ((int setting));

extern int warn_unused_function;
extern int warn_unused_label;
extern int warn_unused_parameter;
extern int warn_unused_variable;
extern int warn_unused_value;

/* Nonzero to warn about code which is never reached.  */

extern int warn_notreached;

/* Nonzero means warn if inline function is too large.  */

extern int warn_inline;

/* Nonzero to warn about variables used before they are initialized.  */

extern int warn_uninitialized;

/* Zero if unknown pragmas are ignored
   One if the compiler should warn about an unknown pragma not in
   a system include file.
   Greater than one if the compiler should warn for all unknown
   pragmas.  */

extern int warn_unknown_pragmas;

/* Nonzero means warn about all declarations which shadow others.  */

extern int warn_shadow;

/* Warn if a switch on an enum fails to have a case for every enum value.  */

extern int warn_switch;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

extern int warn_return_type;

/* Warn about functions which might be candidates for attribute noreturn.  */

extern int warn_missing_noreturn;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */

extern int warn_cast_align;

/* Nonzero means warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes. The value N is in `larger_than_size'.  */

extern int warn_larger_than;
extern HOST_WIDE_INT larger_than_size;

/* Warn if a function returns an aggregate,
   since there are often incompatible calling conventions for doing this.  */

extern int warn_aggregate_return;

/* Warn if packed attribute on struct is unnecessary and inefficient.  */

extern int warn_packed;

/* Warn when gcc pads a structure to an alignment boundary.  */

extern int warn_padded;

/* Warn when an optimization pass is disabled.  */

extern int warn_disabled_optimization;

/* Nonzero means warn about uses of __attribute__((deprecated)) 
   declarations.  */

extern int warn_deprecated_decl;

/* Nonzero if generating code to do profiling.  */

extern int profile_flag;

/* Nonzero if generating code to profile program flow graph arcs.  */

extern int profile_arc_flag;

/* Nonzero if generating info for gcov to calculate line test coverage.  */

extern int flag_test_coverage;

/* Nonzero indicates that branch taken probabilities should be calculated.  */

extern int flag_branch_probabilities;

/* Nonzero if basic blocks should be reordered.  */

extern int flag_reorder_blocks;

/* Nonzero if registers should be renamed.  */

extern int flag_rename_registers;

/* Nonzero for -pedantic switch: warn about anything
   that standard C forbids.  */

extern int pedantic;

/* Temporarily suppress certain warnings.
   This is set while reading code from a system header file.  */

extern int in_system_header;

/* Nonzero for -dp: annotate the assembly with a comment describing the
   pattern and alternative used.  */

extern int flag_print_asm_name;

/* Now the symbols that are set with `-f' switches.  */

/* Nonzero means `char' should be signed.  */

extern int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */

extern int flag_short_enums;

/* Nonzero for -fcaller-saves: allocate values in regs that need to
   be saved across function calls, if that produces overall better code.
   Optional now, so people can test it.  */

extern int flag_caller_saves;

/* Nonzero for -fpcc-struct-return: return values the same way PCC does.  */

extern int flag_pcc_struct_return;

/* Nonzero for -fforce-mem: load memory value into a register
   before arithmetic on it.  This makes better cse but slower compilation.  */

extern int flag_force_mem;

/* Nonzero for -fforce-addr: load memory address into a register before
   reference to memory.  This makes better cse but slower compilation.  */

extern int flag_force_addr;

/* Nonzero for -fdefer-pop: don't pop args after each function call;
   instead save them up to pop many calls' args with one insns.  */

extern int flag_defer_pop;

/* Nonzero for -ffloat-store: don't allocate floats and doubles
   in extended-precision registers.  */

extern int flag_float_store;

/* Nonzero enables strength-reduction in loop.c.  */

extern int flag_strength_reduce;

/* Nonzero enables loop unrolling in unroll.c.  Only loops for which the
   number of iterations can be calculated at compile-time (UNROLL_COMPLETELY,
   UNROLL_MODULO) or at run-time (preconditioned to be UNROLL_MODULO) are
   unrolled.  */

extern int flag_unroll_loops;

/* Nonzero enables loop unrolling in unroll.c.  All loops are unrolled.
   This is generally not a win.  */

extern int flag_unroll_all_loops;

/* Nonzero forces all invariant computations in loops to be moved
   outside the loop.  */

extern int flag_move_all_movables;

/* Nonzero enables prefetch optimizations for arrays in loops.  */

extern int flag_prefetch_loop_arrays;

/* Nonzero forces all general induction variables in loops to be
   strength reduced.  */

extern int flag_reduce_all_givs;

/* Nonzero for -fcse-follow-jumps:
   have cse follow jumps to do a more extensive job.  */

extern int flag_cse_follow_jumps;

/* Nonzero for -fcse-skip-blocks:
   have cse follow a branch around a block.  */

extern int flag_cse_skip_blocks;

/* Nonzero for -fexpensive-optimizations:
   perform miscellaneous relatively-expensive optimizations.  */
extern int flag_expensive_optimizations;

/* Nonzero for -fwritable-strings:
   store string constants in data segment and don't uniquize them.  */

extern int flag_writable_strings;

/* Nonzero means don't put addresses of constant functions in registers.
   Used for compiling the Unix kernel, where strange substitutions are
   done on the assembly output.  */

extern int flag_no_function_cse;

/* Nonzero for -fomit-frame-pointer:
   don't make a frame pointer in simple functions that don't require one.  */

extern int flag_omit_frame_pointer;

/* Nonzero to inhibit use of define_optimization peephole opts.  */

extern int flag_no_peephole;

/* Nonzero means all references through pointers are volatile.  */

extern int flag_volatile;

/* Nonzero means treat all global and extern variables as volatile.  */

extern int flag_volatile_global;

/* Nonzero means treat all static variables as volatile.  */

extern int flag_volatile_static;

/* Nonzero allows GCC to optimize sibling and tail recursive calls.  */

extern int flag_optimize_sibling_calls;

/* Nonzero means the front end generally wants `errno' maintained by math
   operations, like built-in SQRT.  */

extern int flag_errno_math;

/* Nonzero means that unsafe floating-point math optimizations are allowed
   for the sake of speed.  IEEE compliance is not guaranteed, and operations
   are allowed to assume that their arguments and results are "normal"
   (e.g., nonnegative for SQRT).  */

extern int flag_unsafe_math_optimizations;

/* Zero means that floating-point math operations cannot generate a
   (user-visible) trap.  This is the case, for example, in nonstop
   IEEE 754 arithmetic.  */

extern int flag_trapping_math;

/* 0 means straightforward implementation of complex divide acceptable.
   1 means wide ranges of inputs must work for complex divide.
   2 means C99-like requirements for complex divide (not yet implemented).  */

extern int flag_complex_divide_method;

/* Nonzero means to run loop optimizations twice.  */

extern int flag_rerun_loop_opt;

/* Nonzero means make functions that look like good inline candidates
   go inline.  */

extern int flag_inline_functions;

/* Nonzero for -fkeep-inline-functions: even if we make a function
   go inline everywhere, keep its definition around for debugging
   purposes.  */

extern int flag_keep_inline_functions;

/* Nonzero means that functions declared `inline' will be treated
   as `static'.  Prevents generation of zillions of copies of unused
   static inline functions; instead, `inlines' are written out
   only when actually used.  Used in conjunction with -g.  Also
   does the right thing with #pragma interface.  */

extern int flag_no_inline;

/* Nonzero means that we don't want inlining by virtue of -fno-inline,
   not just because the tree inliner turned us off.  */

extern int flag_really_no_inline;

/* Nonzero if we are only using compiler to check syntax errors.  */

extern int flag_syntax_only;

/* Nonzero means we should save auxiliary info into a .X file.  */

extern int flag_gen_aux_info;

/* Nonzero means make the text shared if supported.  */

extern int flag_shared_data;

/* flag_schedule_insns means schedule insns within basic blocks (before
   local_alloc).
   flag_schedule_insns_after_reload means schedule insns after
   global_alloc.  */

extern int flag_schedule_insns;
extern int flag_schedule_insns_after_reload;

/* The following flags have effect only for scheduling before register
   allocation:

   flag_schedule_interblock means schedule insns accross basic blocks.
   flag_schedule_speculative means allow speculative motion of non-load insns.
   flag_schedule_speculative_load means allow speculative motion of some
   load insns.
   flag_schedule_speculative_load_dangerous allows speculative motion of more
   load insns.  */

extern int flag_schedule_interblock;
extern int flag_schedule_speculative;
extern int flag_schedule_speculative_load;
extern int flag_schedule_speculative_load_dangerous;

/* flag_branch_on_count_reg means try to replace add-1,compare,branch tupple
   by a cheaper branch, on a count register.  */
extern int flag_branch_on_count_reg;

/* This option is set to 1 on -fsingle-precision-constant option which is
   used to convert the floating point constants to single precision 
   constants.  */

extern int flag_single_precision_constant;

/* Nonzero means put things in delayed-branch slots if supported.  */

extern int flag_delayed_branch;

/* Nonzero means suppress output of instruction numbers and line number
   notes in debugging dumps.  */

extern int flag_dump_unnumbered;

/* Nonzero means pretend it is OK to examine bits of target floats,
   even if that isn't true.  The resulting code will have incorrect constants,
   but the same series of instructions that the native compiler would make.  */

extern int flag_pretend_float;

/* Nonzero means change certain warnings into errors.
   Usually these are warnings about failure to conform to some standard.  */

extern int flag_pedantic_errors;

/* Nonzero means generate position-independent code.
   This is not fully implemented yet.  */

extern int flag_pic;

/* Nonzero means generate extra code for exception handling and enable
   exception handling.  */

extern int flag_exceptions;

/* Nonzero means generate frame unwind info table when supported */

extern int flag_unwind_tables;

/* Nonzero means generate frame unwind info table exact at each insn boundary */

extern int flag_asynchronous_unwind_tables;

/* Nonzero means don't place uninitialized global data in common storage
   by default.  */

extern int flag_no_common;

/* -finhibit-size-directive inhibits output of .size for ELF.
   This is used only for compiling crtstuff.c,
   and it may be extended to other effects
   needed for crtstuff.c on other systems.  */
extern int flag_inhibit_size_directive;

/* Nonzero means place each function into its own section on those platforms
   which support arbitrary section names and unlimited numbers of sections.  */

extern int flag_function_sections;

/* ... and similar for data.  */
 
extern int flag_data_sections;

/* -fverbose-asm causes extra commentary information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).
   -fno-verbose-asm, the default, causes the extra information
   to not be added and is useful when comparing two assembler files.  */

extern int flag_verbose_asm;

/* -dA causes debug information to be produced in
   the generated assembly code (to make it more readable).  This option
   is generally only of use to those who actually need to read the
   generated assembly code (perhaps while debugging the compiler itself).
   Currently, this switch is only used by dwarfout.c; however, it is intended
   to be a catchall for printing debug information in the assembler file.  */

extern int flag_debug_asm;

extern int flag_dump_rtl_in_asm;

/* -fgnu-linker specifies use of the GNU linker for initializations.
   -fno-gnu-linker says that collect will be used.  */
extern int flag_gnu_linker;

/* Tag all structures with __attribute__(packed) */
extern int flag_pack_struct;

/* This flag is only tested if alias checking is enabled.
   0 if pointer arguments may alias each other.  True in C.
   1 if pointer arguments may not alias each other but may alias
   global variables.
   2 if pointer arguments may not alias each other and may not
   alias global variables.  True in Fortran.
   The value is ignored if flag_alias_check is 0.  */
extern int flag_argument_noalias;

/* Nonzero if we should do (language-dependent) alias analysis.
   Typically, this analysis will assume that expressions of certain
   types do not alias expressions of certain other types.  Only used
   if alias analysis (in general) is enabled.  */
extern int flag_strict_aliasing;

/* Emit code to probe the stack, to help detect stack overflow; also
   may cause large objects to be allocated dynamically.  */
extern int flag_stack_check;

/* Do the full regmove optimization pass.  */
extern int flag_regmove;

/* Instrument functions with calls at entry and exit, for profiling.  */
extern int flag_instrument_function_entry_exit;

/* Perform a peephole pass before sched2.  */
extern int flag_peephole2;

/* Try to guess branch probablities.  */
extern int flag_guess_branch_prob;

/* -fbounded-pointers causes gcc to compile pointers as composite
   objects occupying three words: the pointer value, the base address
   of the referent object, and the address immediately beyond the end
   of the referent object.  The base and extent allow us to perform
   runtime bounds checking.  -fbounded-pointers implies -fcheck-bounds.  */
extern int flag_bounded_pointers;

/* -fcheck-bounds causes gcc to generate array bounds checks.
   For C, C++: defaults to value of flag_bounded_pointers.
   For ObjC: defaults to off.
   For Java: defaults to on.
   For Fortran: defaults to off.
   For CHILL: defaults to off.  */
extern int flag_bounds_check;

/* This will attempt to merge constant section constants, if 1 only
   string constants and constants from constant pool, if 2 also constant
   variables.  */
extern int flag_merge_constants;

/* If one, renumber instruction UIDs to reduce the number of
   unused UIDs if there are a lot of instructions.  If greater than
   one, unconditionally renumber instruction UIDs.  */
extern int flag_renumber_insns;

/* Other basic status info about current function.  */

/* Nonzero means current function must be given a frame pointer.
   Set in stmt.c if anything is allocated on the stack there.
   Set in reload1.c if anything is allocated on the stack there.  */

extern int frame_pointer_needed;

/* Nonzero if the generated code should trap on signed overflow
   for PLUS / SUB / MULT.  */
extern int flag_trapv;

/* Value of the -G xx switch, and whether it was passed or not.  */
extern int g_switch_value;
extern int g_switch_set;

/* Values of the -falign-* flags: how much to align labels in code. 
   0 means `use default', 1 means `don't align'.  
   For each variable, there is an _log variant which is the power
   of two not less than the variable, for .align output.  */

extern int align_loops;
extern int align_loops_log;
extern int align_loops_max_skip;
extern int align_jumps;
extern int align_jumps_log;
extern int align_jumps_max_skip;
extern int align_labels;
extern int align_labels_log;
extern int align_labels_max_skip;
extern int align_functions;
extern int align_functions_log;

/* Like align_functions_log above, but used by front-ends to force the
   minimum function alignment.  Zero means no alignment is forced.  */
extern int force_align_functions_log;

/* Nonzero if we dump in VCG format, not plain text.  */
extern int dump_for_graph;

/* Selection of the graph form.  */
enum graph_dump_types
{
  no_graph = 0,
  vcg
};
extern enum graph_dump_types graph_dump_format;

/* Nonzero means ignore `#ident' directives.  0 means handle them.
   On SVR4 targets, it also controls whether or not to emit a
   string identifying the compiler.  */

extern int flag_no_ident;

/* Nonzero if we want to perform enhanced load motion during gcse.  */

extern int flag_gcse_lm;

/* Nonzero if we want to perform store motion after gcse.  */

extern int flag_gcse_sm;


/* Nonzero means we should do dwarf2 duplicate elimination.  */

extern int flag_eliminate_dwarf2_dups;

/* Non-zero means to collect statistics which might be expensive
   and to print them when we are done.  */
extern int flag_detailed_statistics;

/* Nonzero means enable synchronous exceptions for non-call instructions.  */
extern int flag_non_call_exceptions;

#endif /* ! GCC_FLAGS_H */
