/* Command line option handling.
   Copyright (C) 2002, 2003 Free Software Foundation, Inc.
   Contributed by Neil Booth.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "ggc.h"
#include "output.h"
#include "langhooks.h"
#include "opts.h"
#include "options.h"
#include "flags.h"
#include "toplev.h"
#include "params.h"
#include "diagnostic.h"

/* Value of the -G xx switch, and whether it was passed or not.  */
unsigned HOST_WIDE_INT g_switch_value;
bool g_switch_set;

/* True if we should exit after parsing options.  */
bool exit_after_options;

/* If -version.  */
bool version_flag;

/* Print various extra warnings.  -W/-Wextra.  */
bool extra_warnings;

/* Don't print warning messages.  -w.  */
bool inhibit_warnings;

/* Treat warnings as errors.  -Werror.  */
bool warnings_are_errors;

/* Warn if a function returns an aggregate, since there are often
   incompatible calling conventions for doing this.  */
bool warn_aggregate_return;

/* Nonzero means warn about pointer casts that increase the required
   alignment of the target type (and might therefore lead to a crash
   due to a misaligned access).  */
bool warn_cast_align;

/* Nonzero means warn about uses of __attribute__((deprecated))
   declarations.  */
bool warn_deprecated_decl = true;

/* Warn when an optimization pass is disabled.  */
bool warn_disabled_optimization;

/* Nonzero means warn if inline function is too large.  */
bool warn_inline;

/* True to warn about any objects definitions whose size is larger
   than N bytes.  Also want about function definitions whose returned
   values are larger than N bytes, where N is `larger_than_size'.  */
bool warn_larger_than;
HOST_WIDE_INT larger_than_size;

/* Warn about functions which might be candidates for attribute noreturn.  */
bool warn_missing_noreturn;

/* True to warn about code which is never reached.  */
bool warn_notreached;

/* Warn if packed attribute on struct is unnecessary and inefficient.  */
bool warn_packed;

/* Warn when gcc pads a structure to an alignment boundary.  */
bool warn_padded;

/* True means warn about all declarations which shadow others.  */
bool warn_shadow;

/* Nonzero means warn about constructs which might not be
   strict-aliasing safe.  */
bool warn_strict_aliasing;

/* True to warn if a switch on an enum, that does not have a default
   case, fails to have a case for every enum value.  */
bool warn_switch;

/* Warn if a switch does not have a default case.  */
bool warn_switch_default;

/* Warn if a switch on an enum fails to have a case for every enum
   value (regardless of the presence or otherwise of a default case).  */
bool warn_switch_enum;

/* Don't suppress warnings from system headers.  -Wsystem-headers.  */
bool warn_system_headers;

/* True to warn about variables used before they are initialized.  */
int warn_uninitialized;

/* True to warn about unused variables, functions et.al.  */
bool warn_unused_function;
bool warn_unused_label;
bool warn_unused_parameter;
bool warn_unused_variable;
bool warn_unused_value;

/* Hack for cooperation between set_Wunused and set_Wextra.  */
static bool maybe_warn_unused_parameter;

static size_t find_opt (const char *, int);
static int common_handle_option (size_t scode, const char *arg, int value);
static void handle_param (const char *);
static void set_Wextra (int);

/* Perform a binary search to find which option the command-line INPUT
   matches.  Returns its index in the option array, and N_OPTS
   (cl_options_count) on failure.

   This routine is quite subtle.  A normal binary search is not good
   enough because some options can be suffixed with an argument, and
   multiple sub-matches can occur, e.g. input of "-pedantic" matching
   the initial substring of "-pedantic-errors".

   A more complicated example is -gstabs.  It should match "-g" with
   an argument of "stabs".  Suppose, however, that the number and list
   of switches are such that the binary search tests "-gen-decls"
   before having tested "-g".  This doesn't match, and as "-gen-decls"
   is less than "-gstabs", it will become the lower bound of the
   binary search range, and "-g" will never be seen.  To resolve this
   issue, opts.sh makes "-gen-decls" point, via the back_chain member,
   to "-g" so that failed searches that end between "-gen-decls" and
   the lexicographically subsequent switch know to go back and see if
   "-g" causes a match (which it does in this example).

   This search is done in such a way that the longest match for the
   front end in question wins.  If there is no match for the current
   front end, the longest match for a different front end is returned
   (or N_OPTS if none) and the caller emits an error message.  */
static size_t
find_opt (const char *input, int lang_mask)
{
  size_t mn, mx, md, opt_len;
  size_t match_wrong_lang;
  int comp;

  mn = 0;
  mx = cl_options_count;

  /* Find mn such this lexicographical inequality holds:
     cl_options[mn] <= input < cl_options[mn + 1].  */
  while (mx - mn > 1)
    {
      md = (mn + mx) / 2;
      opt_len = cl_options[md].opt_len;
      comp = strncmp (input, cl_options[md].opt_text, opt_len);

      if (comp < 0)
	mx = md;
      else
	mn = md;
    }

  /* This is the switch that is the best match but for a different
     front end, or cl_options_count if there is no match at all.  */
  match_wrong_lang = cl_options_count;

  /* Backtrace the chain of possible matches, returning the longest
     one, if any, that fits best.  With current GCC switches, this
     loop executes at most twice.  */
  do
    {
      const struct cl_option *opt = &cl_options[mn];

      /* Is this switch a prefix of the input?  */
      if (!strncmp (input, opt->opt_text, opt->opt_len))
	{
	  /* If language is OK, and the match is exact or the switch
	     takes a joined argument, return it.  */
	  if ((opt->flags & lang_mask)
	      && (input[opt->opt_len] == '\0' || (opt->flags & CL_JOINED)))
	    return mn;

	  /* If we haven't remembered a prior match, remember this
	     one.  Any prior match is necessarily better.  */
	  if (match_wrong_lang != cl_options_count)
	    match_wrong_lang = mn;
	}

      /* Try the next possibility.  This is cl_options_count if there
	 are no more.  */
      mn = opt->back_chain;
    }
  while (mn != cl_options_count);

  /* Return the best wrong match, or cl_options_count if none.  */
  return match_wrong_lang;
}

/* If ARG is a non-negative integer made up solely of digits, return its
   value, otherwise return -1.  */
static int
integral_argument (const char *arg)
{
  const char *p = arg;

  while (*p && ISDIGIT (*p))
    p++;

  if (*p == '\0')
    return atoi (arg);

  return -1;
}

/* Handle the switch beginning at ARGV, with ARGC remaining.  */
int
handle_option (int argc ATTRIBUTE_UNUSED, char **argv, int lang_mask)
{
  size_t opt_index;
  const char *opt, *arg = 0;
  char *dup = 0;
  int value = 1;
  int result = 0;
  const struct cl_option *option;

  opt = argv[0];

  /* Interpret "-" or a non-switch as a file name.  */
  if (opt[0] != '-' || opt[1] == '\0')
    {
      opt_index = cl_options_count;
      arg = opt;
      main_input_filename = opt;
      result = (*lang_hooks.handle_option) (opt_index, arg, value);
    }
  else
    {
      /* Drop the "no-" from negative switches.  */
      if ((opt[1] == 'W' || opt[1] == 'f')
	  && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-')
	{
	  size_t len = strlen (opt) - 3;

	  dup = xmalloc (len + 1);
	  dup[0] = '-';
	  dup[1] = opt[1];
	  memcpy (dup + 2, opt + 5, len - 2 + 1);
	  opt = dup;
	  value = 0;
	}

      opt_index = find_opt (opt + 1, lang_mask | CL_COMMON);
      if (opt_index == cl_options_count)
	goto done;

      option = &cl_options[opt_index];

      /* Reject negative form of switches that don't take negatives.  */
      if (!value && (option->flags & CL_REJECT_NEGATIVE))
	goto done;

      /* We've recognized this switch.  */
      result = 1;

      /* Sort out any argument the switch takes.  */
      if (option->flags & CL_JOINED)
	{
	  /* Have arg point to the original switch.  This is because
	     some code, such as disable_builtin_function, expects its
	     argument to be persistent until the program exits.  */
	  arg = argv[0] + cl_options[opt_index].opt_len + 1;
	  if (!value)
	    arg += strlen ("no-");

	  if (*arg == '\0' && !(option->flags & CL_MISSING_OK))
	    {
	      if (option->flags & CL_SEPARATE)
		{
		  arg = argv[1];
		  result = 2;
		}
	      else
		/* Missing argument.  */
		arg = NULL;
	    }
	}
      else if (option->flags & CL_SEPARATE)
	{
	  arg = argv[1];
	  result = 2;
	}

      /* If the switch takes an integer, convert it.  */
      if (arg && (option->flags & CL_UINTEGER))
	{
	  value = integral_argument (arg);
	  if (value == -1)
	    {
	      error ("argument to \"-%s\" should be a non-negative integer",
		     option->opt_text);
	      goto done;
	    }
	}

      if (option->flags & lang_mask)
	if ((*lang_hooks.handle_option) (opt_index, arg, value) == 0)
	  result = 0;

      if (result && (option->flags & CL_COMMON))
	if (common_handle_option (opt_index, arg, value) == 0)
	  result = 0;
    }

 done:
  if (dup)
    free (dup);
  return result;
}

/* Handle target- and language-independent options.  Return zero to
   generate an "unknown option" message.  */
static int
common_handle_option (size_t scode, const char *arg,
		      int value ATTRIBUTE_UNUSED)
{
  const struct cl_option *option = &cl_options[scode];
  enum opt_code code = (enum opt_code) scode;

  if (arg == NULL && (option->flags & (CL_JOINED | CL_SEPARATE)))
    {
      error ("missing argument to \"-%s\"", option->opt_text);
      return 1;
    }

  switch (code)
    {
    default:
      abort ();

    case OPT__help:
      display_help ();
      exit_after_options = true;
      break;

    case OPT__param:
      handle_param (arg);
      break;

    case OPT__target_help:
      display_target_options ();
      exit_after_options = true;
      break;

    case OPT__version:
      print_version (stderr, "");
      exit_after_options = true;
      break;

    case OPT_G:
      g_switch_value = value;
      g_switch_set = true;
      break;

    case OPT_O:
    case OPT_Os:
      /* Currently handled in a prescan.  */
      break;

    case OPT_W:
      /* For backward compatibility, -W is the same as -Wextra.  */
      set_Wextra (value);
      break;

    case OPT_Waggregate_return:
      warn_aggregate_return = value;
      break;

    case OPT_Wcast_align:
      warn_cast_align = value;
      break;

    case OPT_Wdeprecated_declarations:
      warn_deprecated_decl = value;
      break;

    case OPT_Wdisabled_optimization:
      warn_disabled_optimization = value;
      break;

    case OPT_Werror:
      warnings_are_errors = value;
      break;

    case OPT_Wextra:
      set_Wextra (value);
      break;

    case OPT_Winline:
      warn_inline = value;
      break;

    case OPT_Wlarger_than_:
      larger_than_size = value;
      warn_larger_than = value != -1;
      break;

    case OPT_Wmissing_noreturn:
      warn_missing_noreturn = value;
      break;

    case OPT_Wpacked:
      warn_packed = value;
      break;

    case OPT_Wpadded:
      warn_padded = value;
      break;

    case OPT_Wshadow:
      warn_shadow = value;
      break;

    case OPT_Wstrict_aliasing:
      warn_strict_aliasing = value;
      break;

    case OPT_Wswitch:
      warn_switch = value;
      break;

    case OPT_Wswitch_default:
      warn_switch_default = value;
      break;

    case OPT_Wswitch_enum:
      warn_switch_enum = value;
      break;

    case OPT_Wsystem_headers:
      warn_system_headers = value;
      break;

    case OPT_Wuninitialized:
      warn_uninitialized = value;
      break;

    case OPT_Wunreachable_code:
      warn_notreached = value;
      break;

    case OPT_Wunused:
      set_Wunused (value);
      break;

    case OPT_Wunused_function:
      warn_unused_function = value;
      break;

    case OPT_Wunused_label:
      warn_unused_label = value;
      break;

    case OPT_Wunused_parameter:
      warn_unused_parameter = value;
      break;

    case OPT_Wunused_value:
      warn_unused_value = value;
      break;

    case OPT_Wunused_variable:
      warn_unused_variable = value;
      break;

    case OPT_aux_info:
    case OPT_aux_info_:
      aux_info_file_name = arg;
      flag_gen_aux_info = 1;
      break;

    case OPT_auxbase:
      aux_base_name = arg;
      break;

    case OPT_auxbase_strip:
      {
	char *tmp = xstrdup (arg);
	strip_off_ending (tmp, strlen (tmp));
	if (tmp[0])
	  aux_base_name = tmp;
      }
      break;

    case OPT_d:
      decode_d_option (arg);
      break;

    case OPT_dumpbase:
      dump_base_name = arg;
      break;

    case OPT_fPIC:
      flag_pic = value + value;
      break;

    case OPT_fPIE:
      flag_pie = value + value;
      break;

    case OPT_falign_functions:
    case OPT_falign_functions_:
      align_functions = value;
      break;

    case OPT_falign_jumps:
    case OPT_falign_jumps_:
      align_jumps = value;
      break;

    case OPT_falign_labels:
    case OPT_falign_labels_:
      align_labels = value;
      break;

    case OPT_falign_loops:
    case OPT_falign_loops_:
      align_loops = value;
      break;

    case OPT_fargument_alias:
      flag_argument_noalias = !value;
      break;

    case OPT_fargument_noalias:
      flag_argument_noalias = value;
      break;

    case OPT_fargument_noalias_global:
      flag_argument_noalias = value + value;
      break;

    case OPT_fasynchronous_unwind_tables:
      flag_asynchronous_unwind_tables = value;
      break;

    case OPT_fbounds_check:
      flag_bounds_check = value;
      break;

    case OPT_fbranch_count_reg:
      flag_branch_on_count_reg = value;
      break;

    case OPT_fbranch_probabilities:
      flag_branch_probabilities = value;
      break;

    case OPT_fbranch_target_load_optimize:
      flag_branch_target_load_optimize = value;
      break;

    case OPT_fbranch_target_load_optimize2:
      flag_branch_target_load_optimize2 = value;
      break;

    case OPT_fcall_used_:
      fix_register (arg, 0, 1);
      break;

    case OPT_fcall_saved_:
      fix_register (arg, 0, 0);
      break;

    case OPT_fcaller_saves:
      flag_caller_saves = value;
      break;

    case OPT_fcommon:
      flag_no_common = !value;
      break;

    case OPT_fcprop_registers:
      flag_cprop_registers = value;
      break;

    case OPT_fcrossjumping:
      flag_crossjumping = value;
      break;

    case OPT_fcse_follow_jumps:
      flag_cse_follow_jumps = value;
      break;

    case OPT_fcse_skip_blocks:
      flag_cse_skip_blocks = value;
      break;

    case OPT_fdata_sections:
      flag_data_sections = value;
      break;

    case OPT_fdefer_pop:
      flag_defer_pop = value;
      break;

    case OPT_fdelayed_branch:
      flag_delayed_branch = value;
      break;

    case OPT_fdelete_null_pointer_checks:
      flag_delete_null_pointer_checks = value;
      break;

    case OPT_fdiagnostics_show_location_:
      if (!strcmp (arg, "once"))
	diagnostic_prefixing_rule (global_dc) = DIAGNOSTICS_SHOW_PREFIX_ONCE;
      else if (!strcmp (arg, "every-line"))
	diagnostic_prefixing_rule (global_dc)
	  = DIAGNOSTICS_SHOW_PREFIX_EVERY_LINE;
      else
	return 0;
      break;

    case OPT_fdump_unnumbered:
      flag_dump_unnumbered = value;
      break;

    case OPT_feliminate_dwarf2_dups:
      flag_eliminate_dwarf2_dups = value;
      break;

    case OPT_feliminate_unused_debug_types:
      flag_eliminate_unused_debug_types = value;
      break;

    case OPT_fexceptions:
      flag_exceptions = value;
      break;

    case OPT_fexpensive_optimizations:
      flag_expensive_optimizations = value;
      break;

    case OPT_ffast_math:
      set_fast_math_flags (value);
      break;

    case OPT_ffinite_math_only:
      flag_finite_math_only = value;
      break;

    case OPT_ffixed_:
      fix_register (arg, 1, 1);
      break;

    case OPT_ffunction_cse:
      flag_no_function_cse = !value;
      break;

    case OPT_ffloat_store:
      flag_float_store = value;
      break;

    case OPT_fforce_addr:
      flag_force_addr = value;
      break;

    case OPT_fforce_mem:
      flag_force_mem = value;
      break;

    case OPT_ffunction_sections:
      flag_function_sections = value;
      break;

    case OPT_fgcse:
      flag_gcse = value;
      break;

    case OPT_fgcse_lm:
      flag_gcse_lm = value;
      break;

    case OPT_fgcse_sm:
      flag_gcse_sm = value;
      break;

    case OPT_fgnu_linker:
      flag_gnu_linker = value;
      break;

    case OPT_fguess_branch_probability:
      flag_guess_branch_prob = value;
      break;

    case OPT_fident:
      flag_no_ident = !value;
      break;

    case OPT_fif_conversion:
      flag_if_conversion = value;
      break;

    case OPT_fif_conversion2:
      flag_if_conversion2 = value;
      break;

    case OPT_finhibit_size_directive:
      flag_inhibit_size_directive = value;
      break;

    case OPT_finline:
      flag_no_inline = !value;
      break;

    case OPT_finline_functions:
      flag_inline_functions = value;
      break;

    case OPT_finline_limit_:
    case OPT_finline_limit_eq:
      set_param_value ("max-inline-insns", value);
      set_param_value ("max-inline-insns-single", value / 2);
      set_param_value ("max-inline-insns-auto", value / 2);
      set_param_value ("max-inline-insns-rtl", value);
      if (value / 4 < MIN_INLINE_INSNS)
	{
	  if (value / 4 > 10)
	    set_param_value ("min-inline-insns", value / 4);
	  else
	    set_param_value ("min-inline-insns", 10);
	}
      break;

    case OPT_finstrument_functions:
      flag_instrument_function_entry_exit = value;
      break;

    case OPT_fkeep_inline_functions:
      flag_keep_inline_functions =value;
      break;

    case OPT_fkeep_static_consts:
      flag_keep_static_consts = value;
      break;

    case OPT_fleading_underscore:
      flag_leading_underscore = value;
      break;

    case OPT_floop_optimize:
      flag_loop_optimize = value;
      break;

    case OPT_fmath_errno:
      flag_errno_math = value;
      break;

    case OPT_fmem_report:
      mem_report = value;
      break;

    case OPT_fmerge_all_constants:
      flag_merge_constants = value + value;
      break;

    case OPT_fmerge_constants:
      flag_merge_constants = value;
      break;

    case OPT_fmessage_length_:
      output_set_maximum_length (&global_dc->buffer, value);
      break;

    case OPT_fmove_all_movables:
      flag_move_all_movables = value;
      break;

    case OPT_fnew_ra:
      flag_new_regalloc = value;
      break;

    case OPT_fnon_call_exceptions:
      flag_non_call_exceptions = value;
      break;

    case OPT_fold_unroll_all_loops:
      flag_old_unroll_all_loops = value;
      break;

    case OPT_fold_unroll_loops:
      flag_old_unroll_loops = value;
      break;

    case OPT_fomit_frame_pointer:
      flag_omit_frame_pointer = value;
      break;

    case OPT_foptimize_register_move:
      flag_regmove = value;
      break;

    case OPT_foptimize_sibling_calls:
      flag_optimize_sibling_calls = value;
      break;

    case OPT_fpack_struct:
      flag_pack_struct = value;
      break;

    case OPT_fpeel_loops:
      flag_peel_loops = value;
      break;

    case OPT_fpcc_struct_return:
      flag_pcc_struct_return = value;
      break;

    case OPT_fpeephole:
      flag_no_peephole = !value;
      break;

    case OPT_fpeephole2:
      flag_peephole2 = value;
      break;

    case OPT_fpic:
      flag_pic = value;
      break;

    case OPT_fpie:
      flag_pie = value;
      break;

    case OPT_fprefetch_loop_arrays:
      flag_prefetch_loop_arrays = value;
      break;

    case OPT_fprofile:
      profile_flag = value;
      break;

    case OPT_fprofile_arcs:
      profile_arc_flag = value;
      break;

    case OPT_frandom_seed:
      /* The real switch is -fno-random-seed.  */
      if (value)
	return 0;
      flag_random_seed = NULL;
      break;

    case OPT_frandom_seed_:
      flag_random_seed = arg;
      break;

    case OPT_freduce_all_givs:
      flag_reduce_all_givs = value;
      break;

    case OPT_freg_struct_return:
      flag_pcc_struct_return = !value;
      break;

    case OPT_fregmove:
      flag_regmove = value;
      break;

    case OPT_frename_registers:
      flag_rename_registers = value;
      break;

    case OPT_freorder_blocks:
      flag_reorder_blocks = value;
      break;

    case OPT_freorder_functions:
      flag_reorder_functions = value;
      break;

    case OPT_frerun_cse_after_loop:
      flag_rerun_cse_after_loop = value;
      break;

    case OPT_frerun_loop_opt:
      flag_rerun_loop_opt = value;
      break;

    case OPT_fsched_interblock:
      flag_schedule_interblock= value;
      break;

    case OPT_fsched_spec:
      flag_schedule_speculative = value;
      break;

    case OPT_fsched_spec_load:
      flag_schedule_speculative_load = value;
      break;

    case OPT_fsched_spec_load_dangerous:
      flag_schedule_speculative_load_dangerous = value;
      break;

    case OPT_fsched_verbose_:
#ifdef INSN_SCHEDULING
      fix_sched_param ("verbose", arg);
      break;
#else
      return 0;
#endif

    case OPT_fsched2_use_superblocks:
      flag_sched2_use_superblocks = value;
      break;

    case OPT_fsched2_use_traces:
      flag_sched2_use_traces = value;
      break;

    case OPT_fschedule_insns:
      flag_schedule_insns = value;
      break;

    case OPT_fschedule_insns2:
      flag_schedule_insns_after_reload = value;
      break;

    case OPT_fshared_data:
      flag_shared_data = value;
      break;

    case OPT_fsignaling_nans:
      flag_signaling_nans = value;
      break;

    case OPT_fsingle_precision_constant:
      flag_single_precision_constant = value;
      break;

    case OPT_fssa:
      flag_ssa = value;
      break;

    case OPT_fssa_ccp:
      flag_ssa_ccp = value;
      break;

    case OPT_fssa_dce:
      flag_ssa_dce = value;
      break;

    case OPT_fstack_check:
      flag_stack_check = value;
      break;

    case OPT_fstack_limit:
      /* The real switch is -fno-stack-limit.  */
      if (value)
	return 0;
      stack_limit_rtx = NULL_RTX;
      break;

    case OPT_fstack_limit_register_:
      {
	int reg = decode_reg_name (arg);
	if (reg < 0)
	  error ("unrecognized register name \"%s\"", arg);
	else
	  stack_limit_rtx = gen_rtx_REG (Pmode, reg);
      }
      break;

    case OPT_fstack_limit_symbol_:
      stack_limit_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (arg));
      break;

    case OPT_fstrength_reduce:
      flag_strength_reduce = value;
      break;

    case OPT_fstrict_aliasing:
      flag_strict_aliasing = value;
      break;

    case OPT_fsyntax_only:
      flag_syntax_only = value;
      break;

    case OPT_ftest_coverage:
      flag_test_coverage = value;
      break;

    case OPT_fthread_jumps:
      flag_thread_jumps = value;
      break;

    case OPT_ftime_report:
      time_report = value;
      break;

    case OPT_ftls_model_:
      if (!strcmp (arg, "global-dynamic"))
	flag_tls_default = TLS_MODEL_GLOBAL_DYNAMIC;
      else if (!strcmp (arg, "local-dynamic"))
	flag_tls_default = TLS_MODEL_LOCAL_DYNAMIC;
      else if (!strcmp (arg, "initial-exec"))
	flag_tls_default = TLS_MODEL_INITIAL_EXEC;
      else if (!strcmp (arg, "local-exec"))
	flag_tls_default = TLS_MODEL_LOCAL_EXEC;
      else
	warning ("unknown tls-model \"%s\"", arg);
      break;

    case OPT_ftracer:
      flag_tracer = value;
      break;

    case OPT_ftrapping_math:
      flag_trapping_math = value;
      break;

    case OPT_ftrapv:
      flag_trapv = value;
      break;

    case OPT_funit_at_a_time:
      flag_unit_at_a_time = value;
      break;

    case OPT_funroll_all_loops:
      flag_unroll_all_loops = value;
      break;

    case OPT_funroll_loops:
      flag_unroll_loops = value;
      break;

    case OPT_funsafe_math_optimizations:
      flag_unsafe_math_optimizations = value;
      break;

    case OPT_funswitch_loops:
      flag_unswitch_loops = value;
      break;

    case OPT_funwind_tables:
      flag_unwind_tables = value;
      break;

    case OPT_fverbose_asm:
      flag_verbose_asm = value;
      break;
      
    case OPT_fwrapv:
      flag_wrapv = value;
      break;

    case OPT_fwritable_strings:
      flag_writable_strings = value;
      break;

    case OPT_fzero_initialized_in_bss:
      flag_zero_initialized_in_bss = value;
      break;

    case OPT_g:
      decode_g_option (arg);
      break;

    case OPT_m:
      set_target_switch (arg);
      break;

    case OPT_o:
      asm_file_name = arg;
      break;

    case OPT_p:
      profile_flag = 1;
      break;

    case OPT_pedantic:
      pedantic = 1;
      break;

    case OPT_pedantic_errors:
      flag_pedantic_errors = pedantic = 1;
      break;

    case OPT_quiet:
      quiet_flag = 1;
      break;

    case OPT_version:
      version_flag = 1;
      break;

    case OPT_w:
      inhibit_warnings = true;
      break;      
    }

  return 1;
}

/* Handle --param NAME=VALUE.  */
static void
handle_param (const char *carg)
{
  char *equal, *arg;
  int value;

  arg = xstrdup (carg);
  equal = strchr (arg, '=');
  if (!equal)
    error ("%s: --param arguments should be of the form NAME=VALUE", arg);
  else
    {
      value = integral_argument (equal + 1);
      if (value == -1)
	error ("invalid --param value `%s'", equal + 1);
      else
	{
	  *equal = '\0';
	  set_param_value (arg, value);
	}
    }

  free (arg);
}

/* Handle -W and -Wextra.  */
static void
set_Wextra (int setting)
{
  extra_warnings = setting;
  warn_unused_value = setting;
  warn_unused_parameter = (setting && maybe_warn_unused_parameter);

  /* We save the value of warn_uninitialized, since if they put
     -Wuninitialized on the command line, we need to generate a
     warning about not using it without also specifying -O.  */
  if (setting == 0)
    warn_uninitialized = 0;
  else if (warn_uninitialized != 1)
    warn_uninitialized = 2;
}

/* Initialize unused warning flags.  */
void
set_Wunused (int setting)
{
  warn_unused_function = setting;
  warn_unused_label = setting;
  /* Unused function parameter warnings are reported when either
     ``-Wextra -Wunused'' or ``-Wunused-parameter'' is specified.
     Thus, if -Wextra has already been seen, set warn_unused_parameter;
     otherwise set maybe_warn_extra_parameter, which will be picked up
     by set_Wextra.  */
  maybe_warn_unused_parameter = setting;
  warn_unused_parameter = (setting && extra_warnings);
  warn_unused_variable = setting;
  warn_unused_value = setting;
}

/* The following routines are useful in setting all the flags that
   -ffast-math and -fno-fast-math imply.  */
void
set_fast_math_flags (int set)
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
fast_math_flags_set_p (void)
{
  return (!flag_trapping_math
	  && flag_unsafe_math_optimizations
	  && flag_finite_math_only
	  && !flag_errno_math);
}
