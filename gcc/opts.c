/* Command line option handling.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.
   Contributed by Neil Booth.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "opts.h"
#include "options.h"
#include "tm.h" /* For STACK_CHECK_BUILTIN,
		   STACK_CHECK_STATIC_BUILTIN, DEFAULT_GDB_EXTENSIONS,
		   DWARF2_DEBUGGING_INFO and DBX_DEBUGGING_INFO.  */
#include "flags.h"
#include "params.h"
#include "diagnostic.h"
#include "diagnostic-color.h"
#include "opts-diagnostic.h"
#include "insn-attr-common.h"
#include "common/common-target.h"

static void set_Wstrict_aliasing (struct gcc_options *opts, int onoff);

/* Indexed by enum debug_info_type.  */
const char *const debug_type_names[] =
{
  "none", "stabs", "coff", "dwarf-2", "xcoff", "vms"
};

/* Parse the -femit-struct-debug-detailed option value
   and set the flag variables. */

#define MATCH( prefix, string ) \
  ((strncmp (prefix, string, sizeof prefix - 1) == 0) \
   ? ((string += sizeof prefix - 1), 1) : 0)

void
set_struct_debug_option (struct gcc_options *opts, location_t loc,
			 const char *spec)
{
  /* various labels for comparison */
  static const char dfn_lbl[] = "dfn:", dir_lbl[] = "dir:", ind_lbl[] = "ind:";
  static const char ord_lbl[] = "ord:", gen_lbl[] = "gen:";
  static const char none_lbl[] = "none", any_lbl[] = "any";
  static const char base_lbl[] = "base", sys_lbl[] = "sys";

  enum debug_struct_file files = DINFO_STRUCT_FILE_ANY;
  /* Default is to apply to as much as possible. */
  enum debug_info_usage usage = DINFO_USAGE_NUM_ENUMS;
  int ord = 1, gen = 1;

  /* What usage? */
  if (MATCH (dfn_lbl, spec))
    usage = DINFO_USAGE_DFN;
  else if (MATCH (dir_lbl, spec))
    usage = DINFO_USAGE_DIR_USE;
  else if (MATCH (ind_lbl, spec))
    usage = DINFO_USAGE_IND_USE;

  /* Generics or not? */
  if (MATCH (ord_lbl, spec))
    gen = 0;
  else if (MATCH (gen_lbl, spec))
    ord = 0;

  /* What allowable environment? */
  if (MATCH (none_lbl, spec))
    files = DINFO_STRUCT_FILE_NONE;
  else if (MATCH (any_lbl, spec))
    files = DINFO_STRUCT_FILE_ANY;
  else if (MATCH (sys_lbl, spec))
    files = DINFO_STRUCT_FILE_SYS;
  else if (MATCH (base_lbl, spec))
    files = DINFO_STRUCT_FILE_BASE;
  else
    error_at (loc,
	      "argument %qs to %<-femit-struct-debug-detailed%> "
	      "not recognized",
	      spec);

  /* Effect the specification. */
  if (usage == DINFO_USAGE_NUM_ENUMS)
    {
      if (ord)
        {
          opts->x_debug_struct_ordinary[DINFO_USAGE_DFN] = files;
          opts->x_debug_struct_ordinary[DINFO_USAGE_DIR_USE] = files;
          opts->x_debug_struct_ordinary[DINFO_USAGE_IND_USE] = files;
        }
      if (gen)
        {
          opts->x_debug_struct_generic[DINFO_USAGE_DFN] = files;
          opts->x_debug_struct_generic[DINFO_USAGE_DIR_USE] = files;
          opts->x_debug_struct_generic[DINFO_USAGE_IND_USE] = files;
        }
    }
  else
    {
      if (ord)
        opts->x_debug_struct_ordinary[usage] = files;
      if (gen)
        opts->x_debug_struct_generic[usage] = files;
    }

  if (*spec == ',')
    set_struct_debug_option (opts, loc, spec+1);
  else
    {
      /* No more -femit-struct-debug-detailed specifications.
         Do final checks. */
      if (*spec != '\0')
	error_at (loc,
		  "argument %qs to %<-femit-struct-debug-detailed%> unknown",
		  spec);
      if (opts->x_debug_struct_ordinary[DINFO_USAGE_DIR_USE]
		< opts->x_debug_struct_ordinary[DINFO_USAGE_IND_USE]
	  || opts->x_debug_struct_generic[DINFO_USAGE_DIR_USE]
		< opts->x_debug_struct_generic[DINFO_USAGE_IND_USE])
	error_at (loc,
		  "%<-femit-struct-debug-detailed=dir:...%> must allow "
		  "at least as much as "
		  "%<-femit-struct-debug-detailed=ind:...%>");
    }
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

/* Find the base name of a path, stripping off both directories and
   a single final extension. */
int
base_of_path (const char *path, const char **base_out)
{
  const char *base = path;
  const char *dot = 0;
  const char *p = path;
  char c = *p;
  while (c)
    {
      if (IS_DIR_SEPARATOR (c))
        {
          base = p + 1;
          dot = 0;
        }
      else if (c == '.')
        dot = p;
      c = *++p;
    }
  if (!dot)
    dot = p;
  *base_out = base;
  return dot - base;
}

/* What to print when a switch has no documentation.  */
static const char undocumented_msg[] = N_("This switch lacks documentation");

typedef char *char_p; /* For DEF_VEC_P.  */

static void handle_param (struct gcc_options *opts,
			  struct gcc_options *opts_set, location_t loc,
			  const char *carg);
static void set_debug_level (enum debug_info_type type, int extended,
			     const char *arg, struct gcc_options *opts,
			     struct gcc_options *opts_set,
			     location_t loc);
static void set_fast_math_flags (struct gcc_options *opts, int set);
static void decode_d_option (const char *arg, struct gcc_options *opts,
			     location_t loc, diagnostic_context *dc);
static void set_unsafe_math_optimizations_flags (struct gcc_options *opts,
						 int set);
static void enable_warning_as_error (const char *arg, int value,
				     unsigned int lang_mask,
				     const struct cl_option_handlers *handlers,
				     struct gcc_options *opts,
				     struct gcc_options *opts_set,
				     location_t loc,
				     diagnostic_context *dc);

/* Handle a back-end option; arguments and return value as for
   handle_option.  */

bool
target_handle_option (struct gcc_options *opts,
		      struct gcc_options *opts_set,
		      const struct cl_decoded_option *decoded,
		      unsigned int lang_mask ATTRIBUTE_UNUSED, int kind,
		      location_t loc,
		      const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED,
		      diagnostic_context *dc)
{
  gcc_assert (dc == global_dc);
  gcc_assert (kind == DK_UNSPECIFIED);
  return targetm_common.handle_option (opts, opts_set, decoded, loc);
}

/* Add comma-separated strings to a char_p vector.  */

static void
add_comma_separated_to_vector (void **pvec, const char *arg)
{
  char *tmp;
  char *r;
  char *w;
  char *token_start;
  vec<char_p> *v = (vec<char_p> *) *pvec;
  
  vec_check_alloc (v, 1);

  /* We never free this string.  */
  tmp = xstrdup (arg);

  r = tmp;
  w = tmp;
  token_start = tmp;

  while (*r != '\0')
    {
      if (*r == ',')
	{
	  *w++ = '\0';
	  ++r;
	  v->safe_push (token_start);
	  token_start = w;
	}
      if (*r == '\\' && r[1] == ',')
	{
	  *w++ = ',';
	  r += 2;
	}
      else
	*w++ = *r++;
    }
  if (*token_start != '\0')
    v->safe_push (token_start);

  *pvec = v;
}

/* Initialize OPTS and OPTS_SET before using them in parsing options.  */

void
init_options_struct (struct gcc_options *opts, struct gcc_options *opts_set)
{
  size_t num_params = get_num_compiler_params ();

  gcc_obstack_init (&opts_obstack);

  *opts = global_options_init;

  if (opts_set)
    memset (opts_set, 0, sizeof (*opts_set));

  opts->x_param_values = XNEWVEC (int, num_params);

  if (opts_set)
    opts_set->x_param_values = XCNEWVEC (int, num_params);

  init_param_values (opts->x_param_values);

  /* Initialize whether `char' is signed.  */
  opts->x_flag_signed_char = DEFAULT_SIGNED_CHAR;
  /* Set this to a special "uninitialized" value.  The actual default
     is set after target options have been processed.  */
  opts->x_flag_short_enums = 2;

  /* Initialize target_flags before default_options_optimization
     so the latter can modify it.  */
  opts->x_target_flags = targetm_common.default_target_flags;

  /* Some targets have ABI-specified unwind tables.  */
  opts->x_flag_unwind_tables = targetm_common.unwind_tables_default;

  /* Some targets have other target-specific initialization.  */
  targetm_common.option_init_struct (opts);
}

/* If indicated by the optimization level LEVEL (-Os if SIZE is set,
   -Ofast if FAST is set, -Og if DEBUG is set), apply the option DEFAULT_OPT
   to OPTS and OPTS_SET, diagnostic context DC, location LOC, with language
   mask LANG_MASK and option handlers HANDLERS.  */

static void
maybe_default_option (struct gcc_options *opts,
		      struct gcc_options *opts_set,
		      const struct default_options *default_opt,
		      int level, bool size, bool fast, bool debug,
		      unsigned int lang_mask,
		      const struct cl_option_handlers *handlers,
		      location_t loc,
		      diagnostic_context *dc)
{
  const struct cl_option *option = &cl_options[default_opt->opt_index];
  bool enabled;

  if (size)
    gcc_assert (level == 2);
  if (fast)
    gcc_assert (level == 3);
  if (debug)
    gcc_assert (level == 1);

  switch (default_opt->levels)
    {
    case OPT_LEVELS_ALL:
      enabled = true;
      break;

    case OPT_LEVELS_0_ONLY:
      enabled = (level == 0);
      break;

    case OPT_LEVELS_1_PLUS:
      enabled = (level >= 1);
      break;

    case OPT_LEVELS_1_PLUS_SPEED_ONLY:
      enabled = (level >= 1 && !size && !debug);
      break;

    case OPT_LEVELS_1_PLUS_NOT_DEBUG:
      enabled = (level >= 1 && !debug);
      break;

    case OPT_LEVELS_2_PLUS:
      enabled = (level >= 2);
      break;

    case OPT_LEVELS_2_PLUS_SPEED_ONLY:
      enabled = (level >= 2 && !size && !debug);
      break;

    case OPT_LEVELS_3_PLUS:
      enabled = (level >= 3);
      break;

    case OPT_LEVELS_3_PLUS_AND_SIZE:
      enabled = (level >= 3 || size);
      break;

    case OPT_LEVELS_SIZE:
      enabled = size;
      break;

    case OPT_LEVELS_FAST:
      enabled = fast;
      break;

    case OPT_LEVELS_NONE:
    default:
      gcc_unreachable ();
    }

  if (enabled)
    handle_generated_option (opts, opts_set, default_opt->opt_index,
			     default_opt->arg, default_opt->value,
			     lang_mask, DK_UNSPECIFIED, loc,
			     handlers, dc);
  else if (default_opt->arg == NULL
	   && !option->cl_reject_negative)
    handle_generated_option (opts, opts_set, default_opt->opt_index,
			     default_opt->arg, !default_opt->value,
			     lang_mask, DK_UNSPECIFIED, loc,
			     handlers, dc);
}

/* As indicated by the optimization level LEVEL (-Os if SIZE is set,
   -Ofast if FAST is set), apply the options in array DEFAULT_OPTS to
   OPTS and OPTS_SET, diagnostic context DC, location LOC, with
   language mask LANG_MASK and option handlers HANDLERS.  */

static void
maybe_default_options (struct gcc_options *opts,
		       struct gcc_options *opts_set,
		       const struct default_options *default_opts,
		       int level, bool size, bool fast, bool debug,
		       unsigned int lang_mask,
		       const struct cl_option_handlers *handlers,
		       location_t loc,
		       diagnostic_context *dc)
{
  size_t i;

  for (i = 0; default_opts[i].levels != OPT_LEVELS_NONE; i++)
    maybe_default_option (opts, opts_set, &default_opts[i],
			  level, size, fast, debug,
			  lang_mask, handlers, loc, dc);
}

/* Table of options enabled by default at different levels.  */

static const struct default_options default_options_table[] =
  {
    /* -O1 optimizations.  */
    { OPT_LEVELS_1_PLUS, OPT_fdefer_pop, NULL, 1 },
#ifdef DELAY_SLOTS
    { OPT_LEVELS_1_PLUS, OPT_fdelayed_branch, NULL, 1 },
#endif
    { OPT_LEVELS_1_PLUS, OPT_fguess_branch_probability, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fcprop_registers, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fforward_propagate, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fif_conversion, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fif_conversion2, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_pure_const, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_reference, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_profile, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fmerge_constants, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fshrink_wrap, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fsplit_wide_types, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ccp, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_bit_ccp, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_dce, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_dominator_opts, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_dse, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ter, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_ftree_sra, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_copyrename, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_fre, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_copy_prop, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_sink, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ch, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fcombine_stack_adjustments, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fcompare_elim, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_slsr, NULL, 1 },

    /* -O2 optimizations.  */
    { OPT_LEVELS_2_PLUS, OPT_finline_small_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_findirect_inlining, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fpartial_inlining, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fthread_jumps, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcrossjumping, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_foptimize_sibling_calls, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcse_follow_jumps, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fgcse, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fexpensive_optimizations, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_frerun_cse_after_loop, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcaller_saves, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fpeephole2, NULL, 1 },
#ifdef INSN_SCHEDULING
  /* Only run the pre-regalloc scheduling pass if optimizing for speed.  */
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_fschedule_insns, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fschedule_insns2, NULL, 1 },
#endif
    { OPT_LEVELS_2_PLUS, OPT_fstrict_aliasing, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fstrict_overflow, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_freorder_blocks, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_freorder_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_vrp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_builtin_call_dce, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_pre, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_switch_conversion, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_cp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fdevirtualize, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fdevirtualize_speculatively, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_sra, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_falign_loops, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_falign_jumps, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_falign_labels, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_falign_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_tail_merge, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fvect_cost_model_, NULL, VECT_COST_MODEL_CHEAP },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_foptimize_strlen, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fhoist_adjacent_loads, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_sem_equality, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fisolate_erroneous_paths, NULL, 1 },

    /* -O3 optimizations.  */
    { OPT_LEVELS_3_PLUS, OPT_ftree_loop_distribute_patterns, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fpredictive_commoning, NULL, 1 },
    /* Inlining of functions reducing size is a good idea with -Os
       regardless of them being declared inline.  */
    { OPT_LEVELS_3_PLUS_AND_SIZE, OPT_finline_functions, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_finline_functions_called_once, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_funswitch_loops, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fgcse_after_reload, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_ftree_loop_vectorize, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_ftree_slp_vectorize, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fvect_cost_model_, NULL, VECT_COST_MODEL_DYNAMIC },
    { OPT_LEVELS_3_PLUS, OPT_fipa_cp_clone, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_ftree_partial_pre, NULL, 1 },

    /* -Ofast adds optimizations to -O3.  */
    { OPT_LEVELS_FAST, OPT_ffast_math, NULL, 1 },

    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Default the options in OPTS and OPTS_SET based on the optimization
   settings in DECODED_OPTIONS and DECODED_OPTIONS_COUNT.  */
void
default_options_optimization (struct gcc_options *opts,
			      struct gcc_options *opts_set,
			      struct cl_decoded_option *decoded_options,
			      unsigned int decoded_options_count,
			      location_t loc,
			      unsigned int lang_mask,
			      const struct cl_option_handlers *handlers,
			      diagnostic_context *dc)
{
  unsigned int i;
  int opt2;

  /* Scan to see what optimization level has been specified.  That will
     determine the default value of many flags.  */
  for (i = 1; i < decoded_options_count; i++)
    {
      struct cl_decoded_option *opt = &decoded_options[i];
      switch (opt->opt_index)
	{
	case OPT_O:
	  if (*opt->arg == '\0')
	    {
	      opts->x_optimize = 1;
	      opts->x_optimize_size = 0;
	      opts->x_optimize_fast = 0;
	      opts->x_optimize_debug = 0;
	    }
	  else
	    {
	      const int optimize_val = integral_argument (opt->arg);
	      if (optimize_val == -1)
		error_at (loc, "argument to %<-O%> should be a non-negative "
			       "integer, %<g%>, %<s%> or %<fast%>");
	      else
		{
		  opts->x_optimize = optimize_val;
		  if ((unsigned int) opts->x_optimize > 255)
		    opts->x_optimize = 255;
		  opts->x_optimize_size = 0;
		  opts->x_optimize_fast = 0;
		  opts->x_optimize_debug = 0;
		}
	    }
	  break;

	case OPT_Os:
	  opts->x_optimize_size = 1;

	  /* Optimizing for size forces optimize to be 2.  */
	  opts->x_optimize = 2;
	  opts->x_optimize_fast = 0;
	  opts->x_optimize_debug = 0;
	  break;

	case OPT_Ofast:
	  /* -Ofast only adds flags to -O3.  */
	  opts->x_optimize_size = 0;
	  opts->x_optimize = 3;
	  opts->x_optimize_fast = 1;
	  opts->x_optimize_debug = 0;
	  break;

	case OPT_Og:
	  /* -Og selects optimization level 1.  */
	  opts->x_optimize_size = 0;
	  opts->x_optimize = 1;
	  opts->x_optimize_fast = 0;
	  opts->x_optimize_debug = 1;
	  break;

	default:
	  /* Ignore other options in this prescan.  */
	  break;
	}
    }

  maybe_default_options (opts, opts_set, default_options_table,
			 opts->x_optimize, opts->x_optimize_size,
			 opts->x_optimize_fast, opts->x_optimize_debug,
			 lang_mask, handlers, loc, dc);

  /* -O2 param settings.  */
  opt2 = (opts->x_optimize >= 2);

  /* Track fields in field-sensitive alias analysis.  */
  maybe_set_param_value
    (PARAM_MAX_FIELDS_FOR_FIELD_SENSITIVE,
     opt2 ? 100 : default_param_value (PARAM_MAX_FIELDS_FOR_FIELD_SENSITIVE),
     opts->x_param_values, opts_set->x_param_values);

  /* For -O1 only do loop invariant motion for very small loops.  */
  maybe_set_param_value
    (PARAM_LOOP_INVARIANT_MAX_BBS_IN_LOOP,
     opt2 ? default_param_value (PARAM_LOOP_INVARIANT_MAX_BBS_IN_LOOP) : 1000,
     opts->x_param_values, opts_set->x_param_values);

  if (opts->x_optimize_size)
    /* We want to crossjump as much as possible.  */
    maybe_set_param_value (PARAM_MIN_CROSSJUMP_INSNS, 1,
			   opts->x_param_values, opts_set->x_param_values);
  else
    maybe_set_param_value (PARAM_MIN_CROSSJUMP_INSNS,
			   default_param_value (PARAM_MIN_CROSSJUMP_INSNS),
			   opts->x_param_values, opts_set->x_param_values);

  /* Allow default optimizations to be specified on a per-machine basis.  */
  maybe_default_options (opts, opts_set,
			 targetm_common.option_optimization_table,
			 opts->x_optimize, opts->x_optimize_size,
			 opts->x_optimize_fast, opts->x_optimize_debug,
			 lang_mask, handlers, loc, dc);
}

/* After all options at LOC have been read into OPTS and OPTS_SET,
   finalize settings of those options and diagnose incompatible
   combinations.  */
void
finish_options (struct gcc_options *opts, struct gcc_options *opts_set,
		location_t loc)
{
  enum unwind_info_type ui_except;

  if (opts->x_dump_base_name && ! IS_ABSOLUTE_PATH (opts->x_dump_base_name))
    {
      /* First try to make OPTS->X_DUMP_BASE_NAME relative to the
	 OPTS->X_DUMP_DIR_NAME directory.  Then try to make
	 OPTS->X_DUMP_BASE_NAME relative to the OPTS->X_AUX_BASE_NAME
	 directory, typically the directory to contain the object
	 file.  */
      if (opts->x_dump_dir_name)
	opts->x_dump_base_name = opts_concat (opts->x_dump_dir_name,
					      opts->x_dump_base_name, NULL);
      else if (opts->x_aux_base_name
	       && strcmp (opts->x_aux_base_name, HOST_BIT_BUCKET) != 0)
	{
	  const char *aux_base;

	  base_of_path (opts->x_aux_base_name, &aux_base);
	  if (opts->x_aux_base_name != aux_base)
	    {
	      int dir_len = aux_base - opts->x_aux_base_name;
	      char *new_dump_base_name
		= XOBNEWVEC (&opts_obstack, char,
			     strlen (opts->x_dump_base_name) + dir_len + 1);

	      /* Copy directory component from OPTS->X_AUX_BASE_NAME.  */
	      memcpy (new_dump_base_name, opts->x_aux_base_name, dir_len);
	      /* Append existing OPTS->X_DUMP_BASE_NAME.  */
	      strcpy (new_dump_base_name + dir_len, opts->x_dump_base_name);
	      opts->x_dump_base_name = new_dump_base_name;
	    }
	}
    }

  /* Handle related options for unit-at-a-time, toplevel-reorder, and
     section-anchors.  */
  if (!opts->x_flag_unit_at_a_time)
    {
      if (opts->x_flag_section_anchors && opts_set->x_flag_section_anchors)
	error_at (loc, "section anchors must be disabled when unit-at-a-time "
		  "is disabled");
      opts->x_flag_section_anchors = 0;
      if (opts->x_flag_toplevel_reorder == 1)
	error_at (loc, "toplevel reorder must be disabled when unit-at-a-time "
		  "is disabled");
      opts->x_flag_toplevel_reorder = 0;
    }

  if (opts->x_flag_tm && opts->x_flag_non_call_exceptions)
    sorry ("transactional memory is not supported with non-call exceptions");

  /* Unless the user has asked for section anchors, we disable toplevel
     reordering at -O0 to disable transformations that might be surprising
     to end users and to get -fno-toplevel-reorder tested.  */
  if (!opts->x_optimize
      && opts->x_flag_toplevel_reorder == 2
      && !(opts->x_flag_section_anchors && opts_set->x_flag_section_anchors))
    {
      opts->x_flag_toplevel_reorder = 0;
      opts->x_flag_section_anchors = 0;
    }
  if (!opts->x_flag_toplevel_reorder)
    {
      if (opts->x_flag_section_anchors && opts_set->x_flag_section_anchors)
	error_at (loc, "section anchors must be disabled when toplevel reorder"
		  " is disabled");
      opts->x_flag_section_anchors = 0;
    }

  if (!opts->x_flag_opts_finished)
    {
      if (opts->x_flag_pie)
	opts->x_flag_pic = opts->x_flag_pie;
      if (opts->x_flag_pic && !opts->x_flag_pie)
	opts->x_flag_shlib = 1;
      opts->x_flag_opts_finished = true;
    }

  if (opts->x_optimize == 0)
    {
      /* Inlining does not work if not optimizing,
	 so force it not to be done.  */
      opts->x_warn_inline = 0;
      opts->x_flag_no_inline = 1;
    }

  /* The optimization to partition hot and cold basic blocks into separate
     sections of the .o and executable files does not work (currently)
     with exception handling.  This is because there is no support for
     generating unwind info.  If opts->x_flag_exceptions is turned on
     we need to turn off the partitioning optimization.  */

  ui_except = targetm_common.except_unwind_info (opts);

  if (opts->x_flag_exceptions
      && opts->x_flag_reorder_blocks_and_partition
      && (ui_except == UI_SJLJ || ui_except >= UI_TARGET))
    {
      if (opts_set->x_flag_reorder_blocks_and_partition)
        inform (loc,
                "-freorder-blocks-and-partition does not work "
                "with exceptions on this architecture");
      opts->x_flag_reorder_blocks_and_partition = 0;
      opts->x_flag_reorder_blocks = 1;
    }

  /* If user requested unwind info, then turn off the partitioning
     optimization.  */

  if (opts->x_flag_unwind_tables
      && !targetm_common.unwind_tables_default
      && opts->x_flag_reorder_blocks_and_partition
      && (ui_except == UI_SJLJ || ui_except >= UI_TARGET))
    {
      if (opts_set->x_flag_reorder_blocks_and_partition)
        inform (loc,
                "-freorder-blocks-and-partition does not support "
                "unwind info on this architecture");
      opts->x_flag_reorder_blocks_and_partition = 0;
      opts->x_flag_reorder_blocks = 1;
    }

  /* If the target requested unwind info, then turn off the partitioning
     optimization with a different message.  Likewise, if the target does not
     support named sections.  */

  if (opts->x_flag_reorder_blocks_and_partition
      && (!targetm_common.have_named_sections
	  || (opts->x_flag_unwind_tables
	      && targetm_common.unwind_tables_default
	      && (ui_except == UI_SJLJ || ui_except >= UI_TARGET))))
    {
      if (opts_set->x_flag_reorder_blocks_and_partition)
        inform (loc,
                "-freorder-blocks-and-partition does not work "
                "on this architecture");
      opts->x_flag_reorder_blocks_and_partition = 0;
      opts->x_flag_reorder_blocks = 1;
    }

  if (opts->x_flag_reorder_blocks_and_partition
      && !opts_set->x_flag_reorder_functions)
    opts->x_flag_reorder_functions = 1;

  /* Pipelining of outer loops is only possible when general pipelining
     capabilities are requested.  */
  if (!opts->x_flag_sel_sched_pipelining)
    opts->x_flag_sel_sched_pipelining_outer_loops = 0;

  if (opts->x_flag_conserve_stack)
    {
      maybe_set_param_value (PARAM_LARGE_STACK_FRAME, 100,
			     opts->x_param_values, opts_set->x_param_values);
      maybe_set_param_value (PARAM_STACK_FRAME_GROWTH, 40,
			     opts->x_param_values, opts_set->x_param_values);
    }

  if (opts->x_flag_lto)
    {
#ifdef ENABLE_LTO
      opts->x_flag_generate_lto = 1;

      /* When generating IL, do not operate in whole-program mode.
	 Otherwise, symbols will be privatized too early, causing link
	 errors later.  */
      opts->x_flag_whole_program = 0;
#else
      error_at (loc, "LTO support has not been enabled in this configuration");
#endif
      if (!opts->x_flag_fat_lto_objects
	  && (!HAVE_LTO_PLUGIN
	      || (opts_set->x_flag_use_linker_plugin
		  && !opts->x_flag_use_linker_plugin)))
	{
	  if (opts_set->x_flag_fat_lto_objects)
            error_at (loc, "-fno-fat-lto-objects are supported only with linker plugin");
	  opts->x_flag_fat_lto_objects = 1;
	}
    }
  if ((opts->x_flag_lto_partition_balanced != 0) + (opts->x_flag_lto_partition_1to1 != 0)
       + (opts->x_flag_lto_partition_none != 0) >= 1)
    {
      if ((opts->x_flag_lto_partition_balanced != 0)
	   + (opts->x_flag_lto_partition_1to1 != 0)
	   + (opts->x_flag_lto_partition_none != 0) > 1)
	error_at (loc, "only one -flto-partition value can be specified");
    }

  /* We initialize opts->x_flag_split_stack to -1 so that targets can set a
     default value if they choose based on other options.  */
  if (opts->x_flag_split_stack == -1)
    opts->x_flag_split_stack = 0;
  else if (opts->x_flag_split_stack)
    {
      if (!targetm_common.supports_split_stack (true, opts))
	{
	  error_at (loc, "%<-fsplit-stack%> is not supported by "
		    "this compiler configuration");
	  opts->x_flag_split_stack = 0;
	}
    }

  /* Tune vectorization related parametees according to cost model.  */
  if (opts->x_flag_vect_cost_model == VECT_COST_MODEL_CHEAP)
    {
      maybe_set_param_value (PARAM_VECT_MAX_VERSION_FOR_ALIAS_CHECKS,
            6, opts->x_param_values, opts_set->x_param_values);
      maybe_set_param_value (PARAM_VECT_MAX_VERSION_FOR_ALIGNMENT_CHECKS,
            0, opts->x_param_values, opts_set->x_param_values);
      maybe_set_param_value (PARAM_VECT_MAX_PEELING_FOR_ALIGNMENT,
            0, opts->x_param_values, opts_set->x_param_values);
    }

  /* Set PARAM_MAX_STORES_TO_SINK to 0 if either vectorization or if-conversion
     is disabled.  */
  if ((!opts->x_flag_tree_loop_vectorize && !opts->x_flag_tree_slp_vectorize)
       || !opts->x_flag_tree_loop_if_convert)
    maybe_set_param_value (PARAM_MAX_STORES_TO_SINK, 0,
                           opts->x_param_values, opts_set->x_param_values);

  /* The -gsplit-dwarf option requires -gpubnames.  */
  if (opts->x_dwarf_split_debug_info)
    opts->x_debug_generate_pub_sections = 1;
}

#define LEFT_COLUMN	27

/* Output ITEM, of length ITEM_WIDTH, in the left column,
   followed by word-wrapped HELP in a second column.  */
static void
wrap_help (const char *help,
	   const char *item,
	   unsigned int item_width,
	   unsigned int columns)
{
  unsigned int col_width = LEFT_COLUMN;
  unsigned int remaining, room, len;

  remaining = strlen (help);

  do
    {
      room = columns - 3 - MAX (col_width, item_width);
      if (room > columns)
	room = 0;
      len = remaining;

      if (room < len)
	{
	  unsigned int i;

	  for (i = 0; help[i]; i++)
	    {
	      if (i >= room && len != remaining)
		break;
	      if (help[i] == ' ')
		len = i;
	      else if ((help[i] == '-' || help[i] == '/')
		       && help[i + 1] != ' '
		       && i > 0 && ISALPHA (help[i - 1]))
		len = i + 1;
	    }
	}

      printf ("  %-*.*s %.*s\n", col_width, item_width, item, len, help);
      item_width = 0;
      while (help[len] == ' ')
	len++;
      help += len;
      remaining -= len;
    }
  while (remaining);
}

/* Print help for a specific front-end, etc.  */
static void
print_filtered_help (unsigned int include_flags,
		     unsigned int exclude_flags,
		     unsigned int any_flags,
		     unsigned int columns,
		     struct gcc_options *opts,
		     unsigned int lang_mask)
{
  unsigned int i;
  const char *help;
  bool found = false;
  bool displayed = false;

  if (include_flags == CL_PARAMS)
    {
      for (i = 0; i < LAST_PARAM; i++)
	{
	  const char *param = compiler_params[i].option;

	  help = compiler_params[i].help;
	  if (help == NULL || *help == '\0')
	    {
	      if (exclude_flags & CL_UNDOCUMENTED)
		continue;
	      help = undocumented_msg;
	    }

	  /* Get the translation.  */
	  help = _(help);

	  wrap_help (help, param, strlen (param), columns);
	}
      putchar ('\n');
      return;
    }

  if (!opts->x_help_printed)
    opts->x_help_printed = XCNEWVAR (char, cl_options_count);

  if (!opts->x_help_enum_printed)
    opts->x_help_enum_printed = XCNEWVAR (char, cl_enums_count);

  for (i = 0; i < cl_options_count; i++)
    {
      char new_help[128];
      const struct cl_option *option = cl_options + i;
      unsigned int len;
      const char *opt;
      const char *tab;

      if (include_flags == 0
	  || ((option->flags & include_flags) != include_flags))
	{
	  if ((option->flags & any_flags) == 0)
	    continue;
	}

      /* Skip unwanted switches.  */
      if ((option->flags & exclude_flags) != 0)
	continue;

      /* The driver currently prints its own help text.  */
      if ((option->flags & CL_DRIVER) != 0
	  && (option->flags & (((1U << cl_lang_count) - 1)
			       | CL_COMMON | CL_TARGET)) == 0)
	continue;

      found = true;
      /* Skip switches that have already been printed.  */
      if (opts->x_help_printed[i])
	continue;

      opts->x_help_printed[i] = true;

      help = option->help;
      if (help == NULL)
	{
	  if (exclude_flags & CL_UNDOCUMENTED)
	    continue;
	  help = undocumented_msg;
	}

      /* Get the translation.  */
      help = _(help);

      /* Find the gap between the name of the
	 option and its descriptive text.  */
      tab = strchr (help, '\t');
      if (tab)
	{
	  len = tab - help;
	  opt = help;
	  help = tab + 1;
	}
      else
	{
	  opt = option->opt_text;
	  len = strlen (opt);
	}

      /* With the -Q option enabled we change the descriptive text associated
	 with an option to be an indication of its current setting.  */
      if (!opts->x_quiet_flag)
	{
	  void *flag_var = option_flag_var (i, opts);

	  if (len < (LEFT_COLUMN + 2))
	    strcpy (new_help, "\t\t");
	  else
	    strcpy (new_help, "\t");

	  if (flag_var != NULL
	      && option->var_type != CLVC_DEFER)
	    {
	      if (option->flags & CL_JOINED)
		{
		  if (option->var_type == CLVC_STRING)
		    {
		      if (* (const char **) flag_var != NULL)
			snprintf (new_help + strlen (new_help),
				  sizeof (new_help) - strlen (new_help),
				  * (const char **) flag_var);
		    }
		  else if (option->var_type == CLVC_ENUM)
		    {
		      const struct cl_enum *e = &cl_enums[option->var_enum];
		      int value;
		      const char *arg = NULL;

		      value = e->get (flag_var);
		      enum_value_to_arg (e->values, &arg, value, lang_mask);
		      if (arg == NULL)
			arg = _("[default]");
		      snprintf (new_help + strlen (new_help),
				sizeof (new_help) - strlen (new_help),
				arg);
		    }
		  else
		    sprintf (new_help + strlen (new_help),
			     "%#x", * (int *) flag_var);
		}
	      else
		strcat (new_help, option_enabled (i, opts)
			? _("[enabled]") : _("[disabled]"));
	    }

	  help = new_help;
	}

      wrap_help (help, opt, len, columns);
      displayed = true;

      if (option->var_type == CLVC_ENUM
	  && opts->x_help_enum_printed[option->var_enum] != 2)
	opts->x_help_enum_printed[option->var_enum] = 1;
    }

  if (! found)
    {
      unsigned int langs = include_flags & CL_LANG_ALL;

      if (langs == 0)
	printf (_(" No options with the desired characteristics were found\n"));
      else
	{
	  unsigned int i;

	  /* PR 31349: Tell the user how to see all of the
	     options supported by a specific front end.  */
	  for (i = 0; (1U << i) < CL_LANG_ALL; i ++)
	    if ((1U << i) & langs)
	      printf (_(" None found.  Use --help=%s to show *all* the options supported by the %s front-end\n"),
		      lang_names[i], lang_names[i]);
	}

    }
  else if (! displayed)
    printf (_(" All options with the desired characteristics have already been displayed\n"));

  putchar ('\n');

  /* Print details of enumerated option arguments, if those
     enumerations have help text headings provided.  If no help text
     is provided, presume that the possible values are listed in the
     help text for the relevant options.  */
  for (i = 0; i < cl_enums_count; i++)
    {
      unsigned int j, pos;

      if (opts->x_help_enum_printed[i] != 1)
	continue;
      if (cl_enums[i].help == NULL)
	continue;
      printf ("  %s\n    ", _(cl_enums[i].help));
      pos = 4;
      for (j = 0; cl_enums[i].values[j].arg != NULL; j++)
	{
	  unsigned int len = strlen (cl_enums[i].values[j].arg);

	  if (pos > 4 && pos + 1 + len <= columns)
	    {
	      printf (" %s", cl_enums[i].values[j].arg);
	      pos += 1 + len;
	    }
	  else
	    {
	      if (pos > 4)
		{
		  printf ("\n    ");
		  pos = 4;
		}
	      printf ("%s", cl_enums[i].values[j].arg);
	      pos += len;
	    }
	}
      printf ("\n\n");
      opts->x_help_enum_printed[i] = 2;
    }
}

/* Display help for a specified type of option.
   The options must have ALL of the INCLUDE_FLAGS set
   ANY of the flags in the ANY_FLAGS set
   and NONE of the EXCLUDE_FLAGS set.  The current option state is in
   OPTS; LANG_MASK is used for interpreting enumerated option state.  */
static void
print_specific_help (unsigned int include_flags,
		     unsigned int exclude_flags,
		     unsigned int any_flags,
		     struct gcc_options *opts,
		     unsigned int lang_mask)
{
  unsigned int all_langs_mask = (1U << cl_lang_count) - 1;
  const char * description = NULL;
  const char * descrip_extra = "";
  size_t i;
  unsigned int flag;

  /* Sanity check: Make sure that we do not have more
     languages than we have bits available to enumerate them.  */
  gcc_assert ((1U << cl_lang_count) <= CL_MIN_OPTION_CLASS);

  /* If we have not done so already, obtain
     the desired maximum width of the output.  */
  if (opts->x_help_columns == 0)
    {
      const char *p;

      p = getenv ("COLUMNS");
      if (p != NULL)
	{
	  int value = atoi (p);

	  if (value > 0)
	    opts->x_help_columns = value;
	}

      if (opts->x_help_columns == 0)
	/* Use a reasonable default.  */
	opts->x_help_columns = 80;
    }

  /* Decide upon the title for the options that we are going to display.  */
  for (i = 0, flag = 1; flag <= CL_MAX_OPTION_CLASS; flag <<= 1, i ++)
    {
      switch (flag & include_flags)
	{
	case 0:
	case CL_DRIVER:
	  break;

	case CL_TARGET:
	  description = _("The following options are target specific");
	  break;
	case CL_WARNING:
	  description = _("The following options control compiler warning messages");
	  break;
	case CL_OPTIMIZATION:
	  description = _("The following options control optimizations");
	  break;
	case CL_COMMON:
	  description = _("The following options are language-independent");
	  break;
	case CL_PARAMS:
	  description = _("The --param option recognizes the following as parameters");
	  break;
	default:
	  if (i >= cl_lang_count)
	    break;
	  if (exclude_flags & all_langs_mask)
	    description = _("The following options are specific to just the language ");
	  else
	    description = _("The following options are supported by the language ");
	  descrip_extra = lang_names [i];
	  break;
	}
    }

  if (description == NULL)
    {
      if (any_flags == 0)
	{
	  if (include_flags & CL_UNDOCUMENTED)
	    description = _("The following options are not documented");
	  else if (include_flags & CL_SEPARATE)
	    description = _("The following options take separate arguments");
	  else if (include_flags & CL_JOINED)
	    description = _("The following options take joined arguments");
	  else
	    {
	      internal_error ("unrecognized include_flags 0x%x passed to print_specific_help",
			      include_flags);
	      return;
	    }
	}
      else
	{
	  if (any_flags & all_langs_mask)
	    description = _("The following options are language-related");
	  else
	    description = _("The following options are language-independent");
	}
    }

  printf ("%s%s:\n", description, descrip_extra);
  print_filtered_help (include_flags, exclude_flags, any_flags,
		       opts->x_help_columns, opts, lang_mask);
}

/* Handle target- and language-independent options.  Return zero to
   generate an "unknown option" message.  Only options that need
   extra handling need to be listed here; if you simply want
   DECODED->value assigned to a variable, it happens automatically.  */

bool
common_handle_option (struct gcc_options *opts,
		      struct gcc_options *opts_set,
		      const struct cl_decoded_option *decoded,
		      unsigned int lang_mask, int kind ATTRIBUTE_UNUSED,
		      location_t loc,
		      const struct cl_option_handlers *handlers,
		      diagnostic_context *dc)
{
  size_t scode = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;
  enum opt_code code = (enum opt_code) scode;

  gcc_assert (decoded->canonical_option_num_elements <= 2);

  switch (code)
    {
    case OPT__param:
      handle_param (opts, opts_set, loc, arg);
      break;

    case OPT__help:
      {
	unsigned int all_langs_mask = (1U << cl_lang_count) - 1;
	unsigned int undoc_mask;
	unsigned int i;

	if (lang_mask == CL_DRIVER)
	  break;;

	undoc_mask = ((opts->x_verbose_flag | opts->x_extra_warnings)
		      ? 0
		      : CL_UNDOCUMENTED);
	/* First display any single language specific options.  */
	for (i = 0; i < cl_lang_count; i++)
	  print_specific_help
	    (1U << i, (all_langs_mask & (~ (1U << i))) | undoc_mask, 0, opts,
	     lang_mask);
	/* Next display any multi language specific options.  */
	print_specific_help (0, undoc_mask, all_langs_mask, opts, lang_mask);
	/* Then display any remaining, non-language options.  */
	for (i = CL_MIN_OPTION_CLASS; i <= CL_MAX_OPTION_CLASS; i <<= 1)
	  if (i != CL_DRIVER)
	    print_specific_help (i, undoc_mask, 0, opts, lang_mask);
	opts->x_exit_after_options = true;
	break;
      }

    case OPT__target_help:
      if (lang_mask == CL_DRIVER)
	break;

      print_specific_help (CL_TARGET, CL_UNDOCUMENTED, 0, opts, lang_mask);
      opts->x_exit_after_options = true;
      break;

    case OPT__help_:
      {
	const char * a = arg;
	unsigned int include_flags = 0;
	/* Note - by default we include undocumented options when listing
	   specific classes.  If you only want to see documented options
	   then add ",^undocumented" to the --help= option.  E.g.:

	   --help=target,^undocumented  */
	unsigned int exclude_flags = 0;

	if (lang_mask == CL_DRIVER)
	  break;

	/* Walk along the argument string, parsing each word in turn.
	   The format is:
	   arg = [^]{word}[,{arg}]
	   word = {optimizers|target|warnings|undocumented|
		   params|common|<language>}  */
	while (* a != 0)
	  {
	    static const struct
	    {
	      const char * string;
	      unsigned int flag;
	    }
	    specifics[] =
	    {
	      { "optimizers", CL_OPTIMIZATION },
	      { "target", CL_TARGET },
	      { "warnings", CL_WARNING },
	      { "undocumented", CL_UNDOCUMENTED },
	      { "params", CL_PARAMS },
	      { "joined", CL_JOINED },
	      { "separate", CL_SEPARATE },
	      { "common", CL_COMMON },
	      { NULL, 0 }
	    };
	    unsigned int * pflags;
	    const char * comma;
	    unsigned int lang_flag, specific_flag;
	    unsigned int len;
	    unsigned int i;

	    if (* a == '^')
	      {
		++ a;
		pflags = & exclude_flags;
	      }
	    else
	      pflags = & include_flags;

	    comma = strchr (a, ',');
	    if (comma == NULL)
	      len = strlen (a);
	    else
	      len = comma - a;
	    if (len == 0)
	      {
		a = comma + 1;
		continue;
	      }

	    /* Check to see if the string matches an option class name.  */
	    for (i = 0, specific_flag = 0; specifics[i].string != NULL; i++)
	      if (strncasecmp (a, specifics[i].string, len) == 0)
		{
		  specific_flag = specifics[i].flag;
		  break;
		}

	    /* Check to see if the string matches a language name.
	       Note - we rely upon the alpha-sorted nature of the entries in
	       the lang_names array, specifically that shorter names appear
	       before their longer variants.  (i.e. C before C++).  That way
	       when we are attempting to match --help=c for example we will
	       match with C first and not C++.  */
	    for (i = 0, lang_flag = 0; i < cl_lang_count; i++)
	      if (strncasecmp (a, lang_names[i], len) == 0)
		{
		  lang_flag = 1U << i;
		  break;
		}

	    if (specific_flag != 0)
	      {
		if (lang_flag == 0)
		  * pflags |= specific_flag;
		else
		  {
		    /* The option's argument matches both the start of a
		       language name and the start of an option class name.
		       We have a special case for when the user has
		       specified "--help=c", but otherwise we have to issue
		       a warning.  */
		    if (strncasecmp (a, "c", len) == 0)
		      * pflags |= lang_flag;
		    else
		      warning_at (loc, 0,
				  "--help argument %q.*s is ambiguous, "
				  "please be more specific",
				  len, a);
		  }
	      }
	    else if (lang_flag != 0)
	      * pflags |= lang_flag;
	    else
	      warning_at (loc, 0,
			  "unrecognized argument to --help= option: %q.*s",
			  len, a);

	    if (comma == NULL)
	      break;
	    a = comma + 1;
	  }

	if (include_flags)
	  print_specific_help (include_flags, exclude_flags, 0, opts,
			       lang_mask);
	opts->x_exit_after_options = true;
	break;
      }

    case OPT__version:
      if (lang_mask == CL_DRIVER)
	break;

      opts->x_exit_after_options = true;
      break;

    case OPT_fsanitize_:
      {
	const char *p = arg;
	while (*p != 0)
	  {
	    static const struct
	    {
	      const char *const name;
	      unsigned int flag;
	      size_t len;
	    } spec[] =
	    {
	      { "address", SANITIZE_ADDRESS, sizeof "address" - 1 },
	      { "thread", SANITIZE_THREAD, sizeof "thread" - 1 },
	      { "shift", SANITIZE_SHIFT, sizeof "shift" - 1 },
	      { "integer-divide-by-zero", SANITIZE_DIVIDE,
		sizeof "integer-divide-by-zero" - 1 },
	      { "undefined", SANITIZE_UNDEFINED, sizeof "undefined" - 1 },
	      { "unreachable", SANITIZE_UNREACHABLE,
		sizeof "unreachable" - 1 },
	      { "vla-bound", SANITIZE_VLA, sizeof "vla-bound" - 1 },
	      { "null", SANITIZE_NULL, sizeof "null" - 1 },
	      { NULL, 0, 0 }
	    };
	    const char *comma;
	    size_t len, i;
	    bool found = false;

	    comma = strchr (p, ',');
	    if (comma == NULL)
	      len = strlen (p);
	    else
	      len = comma - p;
	    if (len == 0)
	      {
		p = comma + 1;
		continue;
	      }

	    /* Check to see if the string matches an option class name.  */
	    for (i = 0; spec[i].name != NULL; ++i)
	      if (len == spec[i].len
		  && memcmp (p, spec[i].name, len) == 0)
		{
		  /* Handle both -fsanitize and -fno-sanitize cases.  */
		  if (value)
		    flag_sanitize |= spec[i].flag;
		  else
		    flag_sanitize &= ~spec[i].flag;
		  found = true;
		  break;
		}

	    if (! found)
	      warning_at (loc, 0,
			  "unrecognized argument to -fsanitize= option: %q.*s",
			  (int) len, p);

	    if (comma == NULL)
	      break;
	    p = comma + 1;
	  }

	/* When instrumenting the pointers, we don't want to remove
	   the null pointer checks.  */
	if (flag_sanitize & SANITIZE_NULL)
	  opts->x_flag_delete_null_pointer_checks = 0;
	break;
      }

    case OPT_O:
    case OPT_Os:
    case OPT_Ofast:
    case OPT_Og:
      /* Currently handled in a prescan.  */
      break;

    case OPT_Werror:
      dc->warning_as_error_requested = value;
      break;

    case OPT_Werror_:
      if (lang_mask == CL_DRIVER)
	break;

      enable_warning_as_error (arg, value, lang_mask, handlers,
			       opts, opts_set, loc, dc);
      break;

    case OPT_Wlarger_than_:
      opts->x_larger_than_size = value;
      opts->x_warn_larger_than = value != -1;
      break;

    case OPT_Wfatal_errors:
      dc->fatal_errors = value;
      break;

    case OPT_Wframe_larger_than_:
      opts->x_frame_larger_than_size = value;
      opts->x_warn_frame_larger_than = value != -1;
      break;

    case OPT_Wstack_usage_:
      opts->x_warn_stack_usage = value;
      opts->x_flag_stack_usage_info = value != -1;
      break;

    case OPT_Wstrict_aliasing:
      set_Wstrict_aliasing (opts, value);
      break;

    case OPT_Wstrict_overflow:
      opts->x_warn_strict_overflow = (value
				      ? (int) WARN_STRICT_OVERFLOW_CONDITIONAL
				      : 0);
      break;

    case OPT_Wsystem_headers:
      dc->dc_warn_system_headers = value;
      break;

    case OPT_aux_info:
      opts->x_flag_gen_aux_info = 1;
      break;

    case OPT_auxbase_strip:
      {
	char *tmp = xstrdup (arg);
	strip_off_ending (tmp, strlen (tmp));
	if (tmp[0])
	  opts->x_aux_base_name = tmp;
	else
	  free (tmp);
      }
      break;

    case OPT_d:
      decode_d_option (arg, opts, loc, dc);
      break;

    case OPT_fcall_used_:
    case OPT_fcall_saved_:
      /* Deferred.  */
      break;

    case OPT_fdbg_cnt_:
    case OPT_fdbg_cnt_list:
      /* Deferred.  */
      break;

    case OPT_fdebug_prefix_map_:
      /* Deferred.  */
      break;

    case OPT_fdiagnostics_show_location_:
      diagnostic_prefixing_rule (dc) = (diagnostic_prefixing_rule_t) value;
      break;
 
    case OPT_fdiagnostics_show_caret:
      dc->show_caret = value;
      break;

    case OPT_fdiagnostics_color_:
      pp_show_color (dc->printer)
	= colorize_init ((diagnostic_color_rule_t) value);
      break;

    case OPT_fdiagnostics_show_option:
      dc->show_option_requested = value;
      break;

    case OPT_fdump_:
      /* Deferred.  */
      break;

    case OPT_ffast_math:
      set_fast_math_flags (opts, value);
      break;

    case OPT_funsafe_math_optimizations:
      set_unsafe_math_optimizations_flags (opts, value);
      break;

    case OPT_ffixed_:
      /* Deferred.  */
      break;

    case OPT_finline_limit_:
      set_param_value ("max-inline-insns-single", value / 2,
		       opts->x_param_values, opts_set->x_param_values);
      set_param_value ("max-inline-insns-auto", value / 2,
		       opts->x_param_values, opts_set->x_param_values);
      break;

    case OPT_finstrument_functions_exclude_function_list_:
      add_comma_separated_to_vector
	(&opts->x_flag_instrument_functions_exclude_functions, arg);
      break;

    case OPT_finstrument_functions_exclude_file_list_:
      add_comma_separated_to_vector
	(&opts->x_flag_instrument_functions_exclude_files, arg);
      break;

    case OPT_fmessage_length_:
      pp_set_line_maximum_length (dc->printer, value);
      diagnostic_set_caret_max_width (dc, value);
      break;

    case OPT_fopt_info:
    case OPT_fopt_info_:
      /* Deferred.  */
      break;

    case OPT_fpack_struct_:
      if (value <= 0 || (value & (value - 1)) || value > 16)
	error_at (loc,
		  "structure alignment must be a small power of two, not %d",
		  value);
      else
	opts->x_initial_max_fld_align = value;
      break;

    case OPT_fplugin_:
    case OPT_fplugin_arg_:
      /* Deferred.  */
      break;

    case OPT_fprofile_use_:
      opts->x_profile_data_prefix = xstrdup (arg);
      opts->x_flag_profile_use = true;
      value = true;
      /* No break here - do -fprofile-use processing. */
    case OPT_fprofile_use:
      if (!opts_set->x_flag_branch_probabilities)
	opts->x_flag_branch_probabilities = value;
      if (!opts_set->x_flag_profile_values)
	opts->x_flag_profile_values = value;
      if (!opts_set->x_flag_unroll_loops)
	opts->x_flag_unroll_loops = value;
      if (!opts_set->x_flag_peel_loops)
	opts->x_flag_peel_loops = value;
      if (!opts_set->x_flag_tracer)
	opts->x_flag_tracer = value;
      if (!opts_set->x_flag_value_profile_transformations)
	opts->x_flag_value_profile_transformations = value;
      if (!opts_set->x_flag_inline_functions)
	opts->x_flag_inline_functions = value;
      if (!opts_set->x_flag_ipa_cp)
	opts->x_flag_ipa_cp = value;
      if (!opts_set->x_flag_ipa_cp_clone
	  && value && opts->x_flag_ipa_cp)
	opts->x_flag_ipa_cp_clone = value;
      if (!opts_set->x_flag_predictive_commoning)
	opts->x_flag_predictive_commoning = value;
      if (!opts_set->x_flag_unswitch_loops)
	opts->x_flag_unswitch_loops = value;
      if (!opts_set->x_flag_gcse_after_reload)
	opts->x_flag_gcse_after_reload = value;
      if (!opts_set->x_flag_tree_loop_vectorize
          && !opts_set->x_flag_tree_vectorize)
	opts->x_flag_tree_loop_vectorize = value;
      if (!opts_set->x_flag_tree_slp_vectorize
          && !opts_set->x_flag_tree_vectorize)
	opts->x_flag_tree_slp_vectorize = value;
      if (!opts_set->x_flag_vect_cost_model)
	opts->x_flag_vect_cost_model = VECT_COST_MODEL_DYNAMIC;
      if (!opts_set->x_flag_tree_loop_distribute_patterns)
	opts->x_flag_tree_loop_distribute_patterns = value;
      /* Indirect call profiling should do all useful transformations
 	 speculative devirutalization does.  */
      if (!opts_set->x_flag_devirtualize_speculatively
	  && opts->x_flag_value_profile_transformations)
	opts->x_flag_devirtualize_speculatively = false;
      break;

    case OPT_fprofile_generate_:
      opts->x_profile_data_prefix = xstrdup (arg);
      value = true;
      /* No break here - do -fprofile-generate processing. */
    case OPT_fprofile_generate:
      if (!opts_set->x_profile_arc_flag)
	opts->x_profile_arc_flag = value;
      if (!opts_set->x_flag_profile_values)
	opts->x_flag_profile_values = value;
      if (!opts_set->x_flag_inline_functions)
	opts->x_flag_inline_functions = value;
      /* FIXME: Instrumentation we insert makes ipa-reference bitmaps
	 quadratic.  Disable the pass until better memory representation
	 is done.  */
      if (!opts_set->x_flag_ipa_reference && opts->x_in_lto_p)
        opts->x_flag_ipa_reference = false;
      break;

    case OPT_ftree_vectorize:
      if (!opts_set->x_flag_tree_loop_vectorize)
        opts->x_flag_tree_loop_vectorize = value;
      if (!opts_set->x_flag_tree_slp_vectorize)
        opts->x_flag_tree_slp_vectorize = value;
      break;
    case OPT_fshow_column:
      dc->show_column = value;
      break;

    case OPT_frandom_seed:
      /* The real switch is -fno-random-seed.  */
      if (value)
	return false;
      /* Deferred.  */
      break;

    case OPT_frandom_seed_:
      /* Deferred.  */
      break;

    case OPT_fsched_verbose_:
#ifdef INSN_SCHEDULING
      /* Handled with Var in common.opt.  */
      break;
#else
      return false;
#endif

    case OPT_fsched_stalled_insns_:
      opts->x_flag_sched_stalled_insns = value;
      if (opts->x_flag_sched_stalled_insns == 0)
	opts->x_flag_sched_stalled_insns = -1;
      break;

    case OPT_fsched_stalled_insns_dep_:
      opts->x_flag_sched_stalled_insns_dep = value;
      break;

    case OPT_fstack_check_:
      if (!strcmp (arg, "no"))
	opts->x_flag_stack_check = NO_STACK_CHECK;
      else if (!strcmp (arg, "generic"))
	/* This is the old stack checking method.  */
	opts->x_flag_stack_check = STACK_CHECK_BUILTIN
			   ? FULL_BUILTIN_STACK_CHECK
			   : GENERIC_STACK_CHECK;
      else if (!strcmp (arg, "specific"))
	/* This is the new stack checking method.  */
	opts->x_flag_stack_check = STACK_CHECK_BUILTIN
			   ? FULL_BUILTIN_STACK_CHECK
			   : STACK_CHECK_STATIC_BUILTIN
			     ? STATIC_BUILTIN_STACK_CHECK
			     : GENERIC_STACK_CHECK;
      else
	warning_at (loc, 0, "unknown stack check parameter \"%s\"", arg);
      break;

    case OPT_fstack_limit:
      /* The real switch is -fno-stack-limit.  */
      if (value)
	return false;
      /* Deferred.  */
      break;

    case OPT_fstack_limit_register_:
    case OPT_fstack_limit_symbol_:
      /* Deferred.  */
      break;

    case OPT_fstack_usage:
      opts->x_flag_stack_usage = value;
      opts->x_flag_stack_usage_info = value != 0;
      break;

    case OPT_g:
      /* -g by itself should force -g2.  */
      if (*arg == '\0')
	set_debug_level (NO_DEBUG, DEFAULT_GDB_EXTENSIONS, "2", opts, opts_set,
			 loc);
      else
	set_debug_level (NO_DEBUG, DEFAULT_GDB_EXTENSIONS, arg, opts, opts_set,
			 loc);
      break;

    case OPT_gcoff:
      set_debug_level (SDB_DEBUG, false, arg, opts, opts_set, loc);
      break;

    case OPT_gdwarf:
      if (arg && strlen (arg) != 0)
        {
          error_at (loc, "%<-gdwarf%s%> is ambiguous; "
                    "use %<-gdwarf-%s%> for DWARF version "
                    "or %<-gdwarf -g%s%> for debug level", arg, arg, arg);
          break;
        }
      else
        value = opts->x_dwarf_version;
      
      /* FALLTHRU */
    case OPT_gdwarf_:
      if (value < 2 || value > 4)
	error_at (loc, "dwarf version %d is not supported", value);
      else
	opts->x_dwarf_version = value;
      set_debug_level (DWARF2_DEBUG, false, "", opts, opts_set, loc);
      break;

    case OPT_gsplit_dwarf:
      set_debug_level (NO_DEBUG, DEFAULT_GDB_EXTENSIONS, "", opts, opts_set,
		       loc);
      break;

    case OPT_ggdb:
      set_debug_level (NO_DEBUG, 2, arg, opts, opts_set, loc);
      break;

    case OPT_gstabs:
    case OPT_gstabs_:
      set_debug_level (DBX_DEBUG, code == OPT_gstabs_, arg, opts, opts_set,
		       loc);
      break;

    case OPT_gvms:
      set_debug_level (VMS_DEBUG, false, arg, opts, opts_set, loc);
      break;

    case OPT_gxcoff:
    case OPT_gxcoff_:
      set_debug_level (XCOFF_DEBUG, code == OPT_gxcoff_, arg, opts, opts_set,
		       loc);
      break;

    case OPT_pedantic_errors:
      dc->pedantic_errors = 1;
      control_warning_option (OPT_Wpedantic, DK_ERROR, value,
			      loc, lang_mask,
			      handlers, opts, opts_set,
                              dc);
      break;

    case OPT_flto:
      opts->x_flag_lto = value ? "" : NULL;
      break;

    case OPT_w:
      dc->dc_inhibit_warnings = true;
      break;

    case OPT_fmax_errors_:
      dc->max_errors = value;
      break;

    case OPT_fuse_ld_bfd:
    case OPT_fuse_ld_gold:
    case OPT_fuse_linker_plugin:
      /* No-op. Used by the driver and passed to us because it starts with f.*/
      break;

    case OPT_fwrapv:
      if (value)
	opts->x_flag_trapv = 0;
      break;

    case OPT_ftrapv:
      if (value)
	opts->x_flag_wrapv = 0;
      break;

    default:
      /* If the flag was handled in a standard way, assume the lack of
	 processing here is intentional.  */
      gcc_assert (option_flag_var (scode, opts));
      break;
    }

  common_handle_option_auto (opts, opts_set, decoded, lang_mask, kind,
                             loc, handlers, dc);
  return true;
}

/* Handle --param NAME=VALUE.  */
static void
handle_param (struct gcc_options *opts, struct gcc_options *opts_set,
	      location_t loc, const char *carg)
{
  char *equal, *arg;
  int value;

  arg = xstrdup (carg);
  equal = strchr (arg, '=');
  if (!equal)
    error_at (loc, "%s: --param arguments should be of the form NAME=VALUE",
	      arg);
  else
    {
      value = integral_argument (equal + 1);
      if (value == -1)
	error_at (loc, "invalid --param value %qs", equal + 1);
      else
	{
	  *equal = '\0';
	  set_param_value (arg, value,
			   opts->x_param_values, opts_set->x_param_values);
	}
    }

  free (arg);
}

/* Used to set the level of strict aliasing warnings in OPTS,
   when no level is specified (i.e., when -Wstrict-aliasing, and not
   -Wstrict-aliasing=level was given).
   ONOFF is assumed to take value 1 when -Wstrict-aliasing is specified,
   and 0 otherwise.  After calling this function, wstrict_aliasing will be
   set to the default value of -Wstrict_aliasing=level, currently 3.  */
static void
set_Wstrict_aliasing (struct gcc_options *opts, int onoff)
{
  gcc_assert (onoff == 0 || onoff == 1);
  if (onoff != 0)
    opts->x_warn_strict_aliasing = 3;
  else
    opts->x_warn_strict_aliasing = 0;
}

/* The following routines are useful in setting all the flags that
   -ffast-math and -fno-fast-math imply.  */
static void
set_fast_math_flags (struct gcc_options *opts, int set)
{
  if (!opts->frontend_set_flag_unsafe_math_optimizations)
    {
      opts->x_flag_unsafe_math_optimizations = set;
      set_unsafe_math_optimizations_flags (opts, set);
    }
  if (!opts->frontend_set_flag_finite_math_only)
    opts->x_flag_finite_math_only = set;
  if (!opts->frontend_set_flag_errno_math)
    opts->x_flag_errno_math = !set;
  if (set)
    {
      if (!opts->frontend_set_flag_signaling_nans)
	opts->x_flag_signaling_nans = 0;
      if (!opts->frontend_set_flag_rounding_math)
	opts->x_flag_rounding_math = 0;
      if (!opts->frontend_set_flag_cx_limited_range)
	opts->x_flag_cx_limited_range = 1;
    }
}

/* When -funsafe-math-optimizations is set the following
   flags are set as well.  */
static void
set_unsafe_math_optimizations_flags (struct gcc_options *opts, int set)
{
  if (!opts->frontend_set_flag_trapping_math)
    opts->x_flag_trapping_math = !set;
  if (!opts->frontend_set_flag_signed_zeros)
    opts->x_flag_signed_zeros = !set;
  if (!opts->frontend_set_flag_associative_math)
    opts->x_flag_associative_math = set;
  if (!opts->frontend_set_flag_reciprocal_math)
    opts->x_flag_reciprocal_math = set;
}

/* Return true iff flags in OPTS are set as if -ffast-math.  */
bool
fast_math_flags_set_p (const struct gcc_options *opts)
{
  return (!opts->x_flag_trapping_math
	  && opts->x_flag_unsafe_math_optimizations
	  && opts->x_flag_finite_math_only
	  && !opts->x_flag_signed_zeros
	  && !opts->x_flag_errno_math);
}

/* Return true iff flags are set as if -ffast-math but using the flags stored
   in the struct cl_optimization structure.  */
bool
fast_math_flags_struct_set_p (struct cl_optimization *opt)
{
  return (!opt->x_flag_trapping_math
	  && opt->x_flag_unsafe_math_optimizations
	  && opt->x_flag_finite_math_only
	  && !opt->x_flag_signed_zeros
	  && !opt->x_flag_errno_math);
}

/* Handle a debug output -g switch for options OPTS
   (OPTS_SET->x_write_symbols storing whether a debug type was passed
   explicitly), location LOC.  EXTENDED is true or false to support
   extended output (2 is special and means "-ggdb" was given).  */
static void
set_debug_level (enum debug_info_type type, int extended, const char *arg,
		 struct gcc_options *opts, struct gcc_options *opts_set,
		 location_t loc)
{
  opts->x_use_gnu_debug_info_extensions = extended;

  if (type == NO_DEBUG)
    {
      if (opts->x_write_symbols == NO_DEBUG)
	{
	  opts->x_write_symbols = PREFERRED_DEBUGGING_TYPE;

	  if (extended == 2)
	    {
#ifdef DWARF2_DEBUGGING_INFO
	      opts->x_write_symbols = DWARF2_DEBUG;
#elif defined DBX_DEBUGGING_INFO
	      opts->x_write_symbols = DBX_DEBUG;
#endif
	    }

	  if (opts->x_write_symbols == NO_DEBUG)
	    warning_at (loc, 0, "target system does not support debug output");
	}
    }
  else
    {
      /* Does it conflict with an already selected type?  */
      if (opts_set->x_write_symbols != NO_DEBUG
	  && opts->x_write_symbols != NO_DEBUG
	  && type != opts->x_write_symbols)
	error_at (loc, "debug format \"%s\" conflicts with prior selection",
		  debug_type_names[type]);
      opts->x_write_symbols = type;
      opts_set->x_write_symbols = type;
    }

  /* A debug flag without a level defaults to level 2.  */
  if (*arg == '\0')
    {
      if (!opts->x_debug_info_level)
	opts->x_debug_info_level = DINFO_LEVEL_NORMAL;
    }
  else
    {
      int argval = integral_argument (arg);
      if (argval == -1)
	error_at (loc, "unrecognised debug output level \"%s\"", arg);
      else if (argval > 3)
	error_at (loc, "debug output level %s is too high", arg);
      else
	opts->x_debug_info_level = (enum debug_info_levels) argval;
    }
}

/* Arrange to dump core on error for diagnostic context DC.  (The
   regular error message is still printed first, except in the case of
   abort ().)  */

static void
setup_core_dumping (diagnostic_context *dc)
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
  diagnostic_abort_on_error (dc);
}

/* Parse a -d<ARG> command line switch for OPTS, location LOC,
   diagnostic context DC.  */

static void
decode_d_option (const char *arg, struct gcc_options *opts,
		 location_t loc, diagnostic_context *dc)
{
  int c;

  while (*arg)
    switch (c = *arg++)
      {
      case 'A':
	opts->x_flag_debug_asm = 1;
	break;
      case 'p':
	opts->x_flag_print_asm_name = 1;
	break;
      case 'P':
	opts->x_flag_dump_rtl_in_asm = 1;
	opts->x_flag_print_asm_name = 1;
	break;
      case 'x':
	opts->x_rtl_dump_and_exit = 1;
	break;
      case 'D':	/* These are handled by the preprocessor.  */
      case 'I':
      case 'M':
      case 'N':
      case 'U':
	break;
      case 'H':
	setup_core_dumping (dc);
	break;
      case 'a':
	opts->x_flag_dump_all_passed = true;
	break;

      default:
	  warning_at (loc, 0, "unrecognized gcc debugging option: %c", c);
	break;
      }
}

/* Enable (or disable if VALUE is 0) a warning option ARG (language
   mask LANG_MASK, option handlers HANDLERS) as an error for option
   structures OPTS and OPTS_SET, diagnostic context DC (possibly
   NULL), location LOC.  This is used by -Werror=.  */

static void
enable_warning_as_error (const char *arg, int value, unsigned int lang_mask,
			 const struct cl_option_handlers *handlers,
			 struct gcc_options *opts,
			 struct gcc_options *opts_set,
			 location_t loc, diagnostic_context *dc)
{
  char *new_option;
  int option_index;

  new_option = XNEWVEC (char, strlen (arg) + 2);
  new_option[0] = 'W';
  strcpy (new_option + 1, arg);
  option_index = find_opt (new_option, lang_mask);
  if (option_index == OPT_SPECIAL_unknown)
    {
      error_at (loc, "-Werror=%s: no option -%s", arg, new_option);
    }
  else
    {
      const diagnostic_t kind = value ? DK_ERROR : DK_WARNING;

      control_warning_option (option_index, (int) kind, value,
			      loc, lang_mask,
			      handlers, opts, opts_set, dc);
    }
  free (new_option);
}

/* Return malloced memory for the name of the option OPTION_INDEX
   which enabled a diagnostic (context CONTEXT), originally of type
   ORIG_DIAG_KIND but possibly converted to DIAG_KIND by options such
   as -Werror.  */

char *
option_name (diagnostic_context *context, int option_index,
	     diagnostic_t orig_diag_kind, diagnostic_t diag_kind)
{
  if (option_index)
    {
      /* A warning classified as an error.  */
      if ((orig_diag_kind == DK_WARNING || orig_diag_kind == DK_PEDWARN)
	  && diag_kind == DK_ERROR)
	return concat (cl_options[OPT_Werror_].opt_text,
		       /* Skip over "-W".  */
		       cl_options[option_index].opt_text + 2,
		       NULL);
      /* A warning with option.  */
      else
	return xstrdup (cl_options[option_index].opt_text);
    }
  /* A warning without option classified as an error.  */
  else if (orig_diag_kind == DK_WARNING || orig_diag_kind == DK_PEDWARN
	   || diag_kind == DK_WARNING)
    {
      if (context->warning_as_error_requested)
	return xstrdup (cl_options[OPT_Werror].opt_text);
      else
	return xstrdup (_("enabled by default"));
    }
  else
    return NULL;
}
