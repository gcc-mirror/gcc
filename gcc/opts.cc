/* Command line option handling.
   Copyright (C) 2002-2024 Free Software Foundation, Inc.
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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "opts.h"
#include "tm.h"
#include "flags.h"
#include "diagnostic.h"
#include "opts-diagnostic.h"
#include "insn-attr-common.h"
#include "common/common-target.h"
#include "spellcheck.h"
#include "opt-suggestions.h"
#include "diagnostic-color.h"
#include "diagnostic-format.h"
#include "version.h"
#include "selftest.h"
#include "file-prefix-map.h"

/* In this file all option sets are explicit.  */
#undef OPTION_SET_P

/* Set by -fcanon-prefix-map.  */
bool flag_canon_prefix_map;

/* Set by finish_options when flag_stack_protector was set only because of
   -fhardened.  Yuck.  */
bool flag_stack_protector_set_by_fhardened_p;

static void set_Wstrict_aliasing (struct gcc_options *opts, int onoff);

/* Names of fundamental debug info formats indexed by enum
   debug_info_type.  */

const char *const debug_type_names[] =
{
  "none", "dwarf-2", "vms", "ctf", "btf", "codeview"
};

/* Bitmasks of fundamental debug info formats indexed by enum
   debug_info_type.  */

static uint32_t debug_type_masks[] =
{
  NO_DEBUG, DWARF2_DEBUG, VMS_DEBUG,
  CTF_DEBUG, BTF_DEBUG, CODEVIEW_DEBUG
};

/* Names of the set of debug formats requested by user.  Updated and accessed
   via debug_set_names.  */

static char df_set_names[sizeof "none dwarf-2 vms ctf btf codeview"];

/* Get enum debug_info_type of the specified debug format, for error messages.
   Can be used only for individual debug format types.  */

enum debug_info_type
debug_set_to_format (uint32_t debug_info_set)
{
  int idx = 0;
  enum debug_info_type dinfo_type = DINFO_TYPE_NONE;
  /* Find first set bit.  */
  if (debug_info_set)
    idx = exact_log2 (debug_info_set & - debug_info_set);
  /* Check that only one bit is set, if at all.  This function is meant to be
     used only for vanilla debug_info_set bitmask values, i.e. for individual
     debug format types upto DINFO_TYPE_MAX.  */
  gcc_assert ((debug_info_set & (debug_info_set - 1)) == 0);
  dinfo_type = (enum debug_info_type)idx;
  gcc_assert (dinfo_type <= DINFO_TYPE_MAX);
  return dinfo_type;
}

/* Get the number of debug formats enabled for output.  */

unsigned int
debug_set_count (uint32_t w_symbols)
{
  unsigned int count = 0;
  while (w_symbols)
    {
      ++ count;
      w_symbols &= ~ (w_symbols & - w_symbols);
    }
  return count;
}

/* Get the names of the debug formats enabled for output.  */

const char *
debug_set_names (uint32_t w_symbols)
{
  uint32_t df_mask = 0;
  /* Reset the string to be returned.  */
  memset (df_set_names, 0, sizeof (df_set_names));
  /* Get the popcount.  */
  int num_set_df = debug_set_count (w_symbols);
  /* Iterate over the debug formats.  Add name string for those enabled.  */
  for (int i = DINFO_TYPE_NONE; i <= DINFO_TYPE_MAX; i++)
    {
      df_mask = debug_type_masks[i];
      if (w_symbols & df_mask)
	{
	  strcat (df_set_names, debug_type_names[i]);
	  num_set_df--;
	  if (num_set_df)
	    strcat (df_set_names, " ");
	  else
	    break;
	}
      else if (!w_symbols)
	{
	  /* No debug formats enabled.  */
	  gcc_assert (i == DINFO_TYPE_NONE);
	  strcat (df_set_names, debug_type_names[i]);
	  break;
	}
    }
  return df_set_names;
}

/* Return TRUE iff BTF debug info is enabled.  */

bool
btf_debuginfo_p ()
{
  return (write_symbols & BTF_DEBUG);
}

/* Return TRUE iff BTF with CO-RE debug info is enabled.  */

bool
btf_with_core_debuginfo_p ()
{
  return (write_symbols & BTF_WITH_CORE_DEBUG);
}

/* Return TRUE iff CTF debug info is enabled.  */

bool
ctf_debuginfo_p ()
{
  return (write_symbols & CTF_DEBUG);
}

/* Return TRUE iff CodeView debug info is enabled.  */

bool
codeview_debuginfo_p ()
{
  return (write_symbols & CODEVIEW_DEBUG);
}

/* Return TRUE iff dwarf2 debug info is enabled.  */

bool
dwarf_debuginfo_p (struct gcc_options *opts)
{
  return (opts->x_write_symbols & DWARF2_DEBUG);
}

/* Return true iff the debug info format is to be generated based on DWARF
   DIEs (like CTF and BTF debug info formats).  */

bool dwarf_based_debuginfo_p ()
{
  return ((write_symbols & CTF_DEBUG)
	  || (write_symbols & BTF_DEBUG)
	  || (write_symbols & CODEVIEW_DEBUG));
}

/* All flag uses below need to explicitely reference the option sets
   to operate on.  */
#define global_options DO_NOT_USE
#define global_options_set DO_NOT_USE

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
   up to fource characters.  (C++ uses ".cpp".)  */

void
strip_off_ending (char *name, int len)
{
  int i;
  for (i = 2; i < 5 && len > i; i++)
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
static const char undocumented_msg[] = N_("This option lacks documentation.");
static const char use_diagnosed_msg[] = N_("Uses of this option are diagnosed.");

typedef char *char_p; /* For DEF_VEC_P.  */

static void set_debug_level (uint32_t dinfo, int extended,
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
		      diagnostic_context *dc, void (*) (void))
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

  *w = '\0';
  if (*token_start != '\0')
    v->safe_push (token_start);

  *pvec = v;
}

/* Initialize opts_obstack.  */

void
init_opts_obstack (void)
{
  gcc_obstack_init (&opts_obstack);
}

/* Initialize OPTS and OPTS_SET before using them in parsing options.  */

void
init_options_struct (struct gcc_options *opts, struct gcc_options *opts_set)
{
  /* Ensure that opts_obstack has already been initialized by the time
     that we initialize any gcc_options instances (PR jit/68446).  */
  gcc_assert (opts_obstack.chunk_size > 0);

  *opts = global_options_init;

  if (opts_set)
    memset (opts_set, 0, sizeof (*opts_set));

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
			     handlers, true, dc);
  else if (default_opt->arg == NULL
	   && !option->cl_reject_negative
	   && !(option->flags & CL_PARAMS))
    handle_generated_option (opts, opts_set, default_opt->opt_index,
			     default_opt->arg, !default_opt->value,
			     lang_mask, DK_UNSPECIFIED, loc,
			     handlers, true, dc);
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

/* Table of options enabled by default at different levels.
   Please keep this list sorted by level and alphabetized within
   each level; this makes it easier to keep the documentation
   in sync.  */

static const struct default_options default_options_table[] =
  {
    /* -O1 and -Og optimizations.  */
    { OPT_LEVELS_1_PLUS, OPT_fcombine_stack_adjustments, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fcompare_elim, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fcprop_registers, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fdefer_pop, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fforward_propagate, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fguess_branch_probability, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_profile, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_pure_const, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_reference, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fipa_reference_addressable, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fmerge_constants, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_freorder_blocks, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fshrink_wrap, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fsplit_wide_types, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fthread_jumps, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_builtin_call_dce, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ccp, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ch, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_coalesce_vars, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_copy_prop, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_dce, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_dominator_opts, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_fre, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_sink, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_slsr, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_ftree_ter, NULL, 1 },
    { OPT_LEVELS_1_PLUS, OPT_fvar_tracking, NULL, 1 },

    /* -O1 (and not -Og) optimizations.  */
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fbit_tests, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fbranch_count_reg, NULL, 1 },
#if DELAY_SLOTS
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fdelayed_branch, NULL, 1 },
#endif
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fdse, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fif_conversion, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fif_conversion2, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_finline_functions_called_once, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fjump_tables, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fmove_loop_invariants, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fmove_loop_stores, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fssa_phiopt, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_fipa_modref, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_ftree_bit_ccp, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_ftree_dse, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_ftree_pta, NULL, 1 },
    { OPT_LEVELS_1_PLUS_NOT_DEBUG, OPT_ftree_sra, NULL, 1 },

    /* -O2 and -Os optimizations.  */
    { OPT_LEVELS_2_PLUS, OPT_fcaller_saves, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcode_hoisting, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcrossjumping, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fcse_follow_jumps, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fdevirtualize, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fdevirtualize_speculatively, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fexpensive_optimizations, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fext_dce, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fgcse, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fhoist_adjacent_loads, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_findirect_inlining, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_finline_small_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_bit_cp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_cp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_icf, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_ra, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_sra, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fipa_vrp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fisolate_erroneous_paths_dereference, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_flra_remat, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_foptimize_sibling_calls, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fpartial_inlining, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fpeephole2, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_freorder_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_frerun_cse_after_loop, NULL, 1 },
#ifdef INSN_SCHEDULING
    { OPT_LEVELS_2_PLUS, OPT_fschedule_insns2, NULL, 1 },
#endif
    { OPT_LEVELS_2_PLUS, OPT_fstrict_aliasing, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fstore_merging, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_pre, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_switch_conversion, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_tail_merge, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_vrp, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_fvect_cost_model_, NULL,
      VECT_COST_MODEL_VERY_CHEAP },
    { OPT_LEVELS_2_PLUS, OPT_finline_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_ftree_loop_distribute_patterns, NULL, 1 },
    { OPT_LEVELS_2_PLUS, OPT_flate_combine_instructions, NULL, 1 },

    /* -O2 and above optimizations, but not -Os or -Og.  */
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_falign_functions, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_falign_jumps, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_falign_labels, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_falign_loops, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_foptimize_strlen, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_freorder_blocks_algorithm_, NULL,
      REORDER_BLOCKS_ALGORITHM_STC },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_ftree_loop_vectorize, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_ftree_slp_vectorize, NULL, 1 },
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_fopenmp_target_simd_clone_, NULL,
      OMP_TARGET_SIMD_CLONE_NOHOST },
#ifdef INSN_SCHEDULING
  /* Only run the pre-regalloc scheduling pass if optimizing for speed.  */
    { OPT_LEVELS_2_PLUS_SPEED_ONLY, OPT_fschedule_insns, NULL, 1 },
#endif

    /* -O3 and -Os optimizations.  */

    /* -O3 optimizations.  */
    { OPT_LEVELS_3_PLUS, OPT_fgcse_after_reload, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fipa_cp_clone, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_floop_interchange, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_floop_unroll_and_jam, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fpeel_loops, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fpredictive_commoning, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fsplit_loops, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fsplit_paths, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_ftree_loop_distribution, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_ftree_partial_pre, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_funswitch_loops, NULL, 1 },
    { OPT_LEVELS_3_PLUS, OPT_fvect_cost_model_, NULL, VECT_COST_MODEL_DYNAMIC },
    { OPT_LEVELS_3_PLUS, OPT_fversion_loops_for_strides, NULL, 1 },

    /* -O3 parameters.  */
    { OPT_LEVELS_3_PLUS, OPT__param_max_inline_insns_auto_, NULL, 30 },
    { OPT_LEVELS_3_PLUS, OPT__param_early_inlining_insns_, NULL, 14 },
    { OPT_LEVELS_3_PLUS, OPT__param_inline_heuristics_hint_percent_, NULL, 600 },
    { OPT_LEVELS_3_PLUS, OPT__param_inline_min_speedup_, NULL, 15 },
    { OPT_LEVELS_3_PLUS, OPT__param_max_inline_insns_single_, NULL, 200 },

    /* -Ofast adds optimizations to -O3.  */
    { OPT_LEVELS_FAST, OPT_ffast_math, NULL, 1 },
    { OPT_LEVELS_FAST, OPT_fallow_store_data_races, NULL, 1 },
    { OPT_LEVELS_FAST, OPT_fsemantic_interposition, NULL, 0 },

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
  bool openacc_mode = false;

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
			       "integer, %<g%>, %<s%>, %<z%> or %<fast%>");
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

	case OPT_Oz:
	  opts->x_optimize_size = 2;

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

	case OPT_fopenacc:
	  if (opt->value)
	    openacc_mode = true;
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

  if (openacc_mode)
    SET_OPTION_IF_UNSET (opts, opts_set, flag_ipa_pta, true);

  /* Track fields in field-sensitive alias analysis.  */
  if (opt2)
    SET_OPTION_IF_UNSET (opts, opts_set, param_max_fields_for_field_sensitive,
			 100);

  if (opts->x_optimize_size)
    /* We want to crossjump as much as possible.  */
    SET_OPTION_IF_UNSET (opts, opts_set, param_min_crossjump_insns, 1);

  /* Restrict the amount of work combine does at -Og while retaining
     most of its useful transforms.  */
  if (opts->x_optimize_debug)
    SET_OPTION_IF_UNSET (opts, opts_set, param_max_combine_insns, 2);

  /* Allow default optimizations to be specified on a per-machine basis.  */
  maybe_default_options (opts, opts_set,
			 targetm_common.option_optimization_table,
			 opts->x_optimize, opts->x_optimize_size,
			 opts->x_optimize_fast, opts->x_optimize_debug,
			 lang_mask, handlers, loc, dc);
}

/* Control IPA optimizations based on different live patching LEVEL.  */
static void
control_options_for_live_patching (struct gcc_options *opts,
				   struct gcc_options *opts_set,
				   enum live_patching_level level,
				   location_t loc)
{
  gcc_assert (level > LIVE_PATCHING_NONE);

  switch (level)
    {
    case LIVE_PATCHING_INLINE_ONLY_STATIC:
#define LIVE_PATCHING_OPTION "-flive-patching=inline-only-static"
      if (opts_set->x_flag_ipa_cp_clone && opts->x_flag_ipa_cp_clone)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-cp-clone", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_cp_clone = 0;

      if (opts_set->x_flag_ipa_sra && opts->x_flag_ipa_sra)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-sra", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_sra = 0;

      if (opts_set->x_flag_partial_inlining && opts->x_flag_partial_inlining)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fpartial-inlining", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_partial_inlining = 0;

      if (opts_set->x_flag_ipa_cp && opts->x_flag_ipa_cp)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-cp", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_cp = 0;

      /* FALLTHROUGH.  */
    case LIVE_PATCHING_INLINE_CLONE:
#undef LIVE_PATCHING_OPTION
#define LIVE_PATCHING_OPTION "-flive-patching=inline-only-static|inline-clone"
      /* live patching should disable whole-program optimization.  */
      if (opts_set->x_flag_whole_program && opts->x_flag_whole_program)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fwhole-program", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_whole_program = 0;

      /* visibility change should be excluded by !flag_whole_program
	 && !in_lto_p && !flag_ipa_cp_clone && !flag_ipa_sra
	 && !flag_partial_inlining.  */

      if (opts_set->x_flag_ipa_pta && opts->x_flag_ipa_pta)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-pta", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_pta = 0;

      if (opts_set->x_flag_ipa_reference && opts->x_flag_ipa_reference)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-reference", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_reference = 0;

      if (opts_set->x_flag_ipa_ra && opts->x_flag_ipa_ra)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-ra", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_ra = 0;

      if (opts_set->x_flag_ipa_icf && opts->x_flag_ipa_icf)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-icf", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_icf = 0;

      if (opts_set->x_flag_ipa_icf_functions && opts->x_flag_ipa_icf_functions)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-icf-functions", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_icf_functions = 0;

      if (opts_set->x_flag_ipa_icf_variables && opts->x_flag_ipa_icf_variables)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-icf-variables", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_icf_variables = 0;

      if (opts_set->x_flag_ipa_bit_cp && opts->x_flag_ipa_bit_cp)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-bit-cp", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_bit_cp = 0;

      if (opts_set->x_flag_ipa_vrp && opts->x_flag_ipa_vrp)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-vrp", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_vrp = 0;

      if (opts_set->x_flag_ipa_pure_const && opts->x_flag_ipa_pure_const)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-pure-const", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_pure_const = 0;

      if (opts_set->x_flag_ipa_modref && opts->x_flag_ipa_modref)
	error_at (loc,
		  "%<-fipa-modref%> is incompatible with %qs",
		  LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_modref = 0;

      /* FIXME: disable unreachable code removal.  */

      /* discovery of functions/variables with no address taken.  */
      if (opts_set->x_flag_ipa_reference_addressable
	  && opts->x_flag_ipa_reference_addressable)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-reference-addressable", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_reference_addressable = 0;

      /* ipa stack alignment propagation.  */
      if (opts_set->x_flag_ipa_stack_alignment
	  && opts->x_flag_ipa_stack_alignment)
	error_at (loc, "%qs is incompatible with %qs",
		  "-fipa-stack-alignment", LIVE_PATCHING_OPTION);
      else
	opts->x_flag_ipa_stack_alignment = 0;
      break;
    default:
      gcc_unreachable ();
    }

#undef LIVE_PATCHING_OPTION
}

/* --help option argument if set.  */
vec<const char *> help_option_arguments;

/* Return the string name describing a sanitizer argument which has been
   provided on the command line and has set this particular flag.  */
const char *
find_sanitizer_argument (struct gcc_options *opts, unsigned int flags)
{
  for (int i = 0; sanitizer_opts[i].name != NULL; ++i)
    {
      /* Need to find the sanitizer_opts element which:
	 a) Could have set the flags requested.
	 b) Has been set on the command line.

	 Can have (a) without (b) if the flag requested is e.g.
	 SANITIZE_ADDRESS, since both -fsanitize=address and
	 -fsanitize=kernel-address set this flag.

	 Can have (b) without (a) by requesting more than one sanitizer on the
	 command line.  */
      if ((sanitizer_opts[i].flag & opts->x_flag_sanitize)
	  != sanitizer_opts[i].flag)
	continue;
      if ((sanitizer_opts[i].flag & flags) != flags)
	continue;
      return sanitizer_opts[i].name;
    }
  return NULL;
}


/* Report an error to the user about sanitizer options they have requested
   which have set conflicting flags.

   LEFT and RIGHT indicate sanitizer flags which conflict with each other, this
   function reports an error if both have been set in OPTS->x_flag_sanitize and
   ensures the error identifies the requested command line options that have
   set these flags.  */
static void
report_conflicting_sanitizer_options (struct gcc_options *opts, location_t loc,
				      unsigned int left, unsigned int right)
{
  unsigned int left_seen = (opts->x_flag_sanitize & left);
  unsigned int right_seen = (opts->x_flag_sanitize & right);
  if (left_seen && right_seen)
    {
      const char* left_arg = find_sanitizer_argument (opts, left_seen);
      const char* right_arg = find_sanitizer_argument (opts, right_seen);
      gcc_assert (left_arg && right_arg);
      error_at (loc,
		"%<-fsanitize=%s%> is incompatible with %<-fsanitize=%s%>",
		left_arg, right_arg);
    }
}

/* After all options at LOC have been read into OPTS and OPTS_SET,
   finalize settings of those options and diagnose incompatible
   combinations.  */
void
finish_options (struct gcc_options *opts, struct gcc_options *opts_set,
		location_t loc)
{
  if (opts->x_dump_base_name
      && ! opts->x_dump_base_name_prefixed)
    {
      const char *sep = opts->x_dump_base_name;

      for (; *sep; sep++)
	if (IS_DIR_SEPARATOR (*sep))
	  break;

      if (*sep)
	/* If dump_base_path contains subdirectories, don't prepend
	   anything.  */;
      else if (opts->x_dump_dir_name)
	/* We have a DUMP_DIR_NAME, prepend that.  */
	opts->x_dump_base_name = opts_concat (opts->x_dump_dir_name,
					      opts->x_dump_base_name, NULL);

      /* It is definitely prefixed now.  */
      opts->x_dump_base_name_prefixed = true;
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

  /* -fself-test depends on the state of the compiler prior to
     compiling anything.  Ideally it should be run on an empty source
     file.  However, in case we get run with actual source, assume
     -fsyntax-only which will inhibit any compiler initialization
     which may confuse the self tests.  */
  if (opts->x_flag_self_test)
    opts->x_flag_syntax_only = 1;

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

  if (opts->x_flag_hardened)
    {
      if (!opts_set->x_flag_auto_var_init)
	opts->x_flag_auto_var_init = AUTO_INIT_ZERO;
      else if (opts->x_flag_auto_var_init != AUTO_INIT_ZERO)
	warning_at (loc, OPT_Whardened,
		    "%<-ftrivial-auto-var-init=zero%> is not enabled by "
		    "%<-fhardened%> because it was specified on the command "
		    "line");
    }

  if (!opts->x_flag_opts_finished)
    {
      /* We initialize opts->x_flag_pie to -1 so that targets can set a
	 default value.  */
      if (opts->x_flag_pie == -1)
	{
	  /* We initialize opts->x_flag_pic to -1 so that we can tell if
	     -fpic, -fPIC, -fno-pic or -fno-PIC is used.  */
	  if (opts->x_flag_pic == -1)
	    opts->x_flag_pie = (opts->x_flag_hardened
				? /*-fPIE*/ 2 : DEFAULT_FLAG_PIE);
	  else
	    opts->x_flag_pie = 0;
	}
      /* If -fPIE or -fpie is used, turn on PIC.  */
      if (opts->x_flag_pie)
	opts->x_flag_pic = opts->x_flag_pie;
      else if (opts->x_flag_pic == -1)
	opts->x_flag_pic = 0;
      if (opts->x_flag_pic && !opts->x_flag_pie)
	opts->x_flag_shlib = 1;
      opts->x_flag_opts_finished = true;
    }

  /* We initialize opts->x_flag_stack_protect to -1 so that targets
     can set a default value.  With --enable-default-ssp or -fhardened
     the default is -fstack-protector-strong.  */
  if (opts->x_flag_stack_protect == -1)
    {
      /* This should check FRAME_GROWS_DOWNWARD, but on some targets it's
	 defined in such a way that it uses flag_stack_protect which can't
	 be used here.  Moreover, some targets like BPF don't support
	 -fstack-protector at all but we don't know that here.  So remember
	 that flag_stack_protect was set at the behest of -fhardened.  */
      if (opts->x_flag_hardened)
	{
	  opts->x_flag_stack_protect = SPCT_FLAG_STRONG;
	  flag_stack_protector_set_by_fhardened_p = true;
	}
      else
	opts->x_flag_stack_protect = DEFAULT_FLAG_SSP;
    }
  else if (opts->x_flag_hardened
	   && opts->x_flag_stack_protect != SPCT_FLAG_STRONG)
    warning_at (UNKNOWN_LOCATION, OPT_Whardened,
		"%<-fstack-protector-strong%> is not enabled by "
		"%<-fhardened%> because it was specified on the command "
		"line");

  if (opts->x_optimize == 0)
    {
      /* Inlining does not work if not optimizing,
	 so force it not to be done.  */
      opts->x_warn_inline = 0;
      opts->x_flag_no_inline = 1;
    }

  /* At -O0 or -Og, turn __builtin_unreachable into a trap.  */
  if (!opts->x_optimize || opts->x_optimize_debug)
    SET_OPTION_IF_UNSET (opts, opts_set, flag_unreachable_traps, true);

  /* Pipelining of outer loops is only possible when general pipelining
     capabilities are requested.  */
  if (!opts->x_flag_sel_sched_pipelining)
    opts->x_flag_sel_sched_pipelining_outer_loops = 0;

  if (opts->x_flag_conserve_stack)
    {
      SET_OPTION_IF_UNSET (opts, opts_set, param_large_stack_frame, 100);
      SET_OPTION_IF_UNSET (opts, opts_set, param_stack_frame_growth, 40);
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
	    error_at (loc, "%<-fno-fat-lto-objects%> are supported only with "
		      "linker plugin");
	  opts->x_flag_fat_lto_objects = 1;
	}

      /* -gsplit-dwarf isn't compatible with LTO, see PR88389.  */
      if (opts->x_dwarf_split_debug_info)
	{
	  inform (loc, "%<-gsplit-dwarf%> is not supported with LTO,"
		  " disabling");
	  opts->x_dwarf_split_debug_info = 0;
	}
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

  /* If stack splitting is turned on, and the user did not explicitly
     request function partitioning, turn off partitioning, as it
     confuses the linker when trying to handle partitioned split-stack
     code that calls a non-split-stack functions.  But if partitioning
     was turned on explicitly just hope for the best.  */
  if (opts->x_flag_split_stack
      && opts->x_flag_reorder_blocks_and_partition)
    SET_OPTION_IF_UNSET (opts, opts_set, flag_reorder_blocks_and_partition, 0);

  if (opts->x_flag_reorder_blocks_and_partition)
    SET_OPTION_IF_UNSET (opts, opts_set, flag_reorder_functions, 1);

  /* The -gsplit-dwarf option requires -ggnu-pubnames.  */
  if (opts->x_dwarf_split_debug_info)
    opts->x_debug_generate_pub_sections = 2;

  if ((opts->x_flag_sanitize
       & (SANITIZE_USER_ADDRESS | SANITIZE_KERNEL_ADDRESS)) == 0)
    {
      if (opts->x_flag_sanitize & SANITIZE_POINTER_COMPARE)
	error_at (loc,
		  "%<-fsanitize=pointer-compare%> must be combined with "
		  "%<-fsanitize=address%> or %<-fsanitize=kernel-address%>");
      if (opts->x_flag_sanitize & SANITIZE_POINTER_SUBTRACT)
	error_at (loc,
		  "%<-fsanitize=pointer-subtract%> must be combined with "
		  "%<-fsanitize=address%> or %<-fsanitize=kernel-address%>");
    }

  /* Address sanitizers conflict with the thread sanitizer.  */
  report_conflicting_sanitizer_options (opts, loc, SANITIZE_THREAD,
					SANITIZE_ADDRESS);
  report_conflicting_sanitizer_options (opts, loc, SANITIZE_THREAD,
					SANITIZE_HWADDRESS);
  /* The leak sanitizer conflicts with the thread sanitizer.  */
  report_conflicting_sanitizer_options (opts, loc, SANITIZE_LEAK,
					SANITIZE_THREAD);

  /* No combination of HWASAN and ASAN work together.  */
  report_conflicting_sanitizer_options (opts, loc,
					SANITIZE_HWADDRESS, SANITIZE_ADDRESS);

  /* The userspace and kernel address sanitizers conflict with each other.  */
  report_conflicting_sanitizer_options (opts, loc, SANITIZE_USER_HWADDRESS,
					SANITIZE_KERNEL_HWADDRESS);
  report_conflicting_sanitizer_options (opts, loc, SANITIZE_USER_ADDRESS,
					SANITIZE_KERNEL_ADDRESS);

  /* Check error recovery for -fsanitize-recover option.  */
  for (int i = 0; sanitizer_opts[i].name != NULL; ++i)
    if ((opts->x_flag_sanitize_recover & sanitizer_opts[i].flag)
	&& !sanitizer_opts[i].can_recover)
      error_at (loc, "%<-fsanitize-recover=%s%> is not supported",
		sanitizer_opts[i].name);

  /* Check -fsanitize-trap option.  */
  for (int i = 0; sanitizer_opts[i].name != NULL; ++i)
    if ((opts->x_flag_sanitize_trap & sanitizer_opts[i].flag)
	&& !sanitizer_opts[i].can_trap
	/* Allow -fsanitize-trap=all or -fsanitize-trap=undefined
	   to set flag_sanitize_trap & SANITIZE_VPTR bit which will
	   effectively disable -fsanitize=vptr, just disallow
	   explicit -fsanitize-trap=vptr.  */
	&& sanitizer_opts[i].flag != SANITIZE_VPTR)
      error_at (loc, "%<-fsanitize-trap=%s%> is not supported",
		sanitizer_opts[i].name);

  /* When instrumenting the pointers, we don't want to remove
     the null pointer checks.  */
  if (opts->x_flag_sanitize & (SANITIZE_NULL | SANITIZE_NONNULL_ATTRIBUTE
				| SANITIZE_RETURNS_NONNULL_ATTRIBUTE))
    opts->x_flag_delete_null_pointer_checks = 0;

  /* Aggressive compiler optimizations may cause false negatives.  */
  if (opts->x_flag_sanitize & ~(SANITIZE_LEAK | SANITIZE_UNREACHABLE))
    opts->x_flag_aggressive_loop_optimizations = 0;

  /* Enable -fsanitize-address-use-after-scope if either address sanitizer is
     enabled.  */
  if (opts->x_flag_sanitize
      & (SANITIZE_USER_ADDRESS | SANITIZE_USER_HWADDRESS))
    SET_OPTION_IF_UNSET (opts, opts_set, flag_sanitize_address_use_after_scope,
			 true);

  /* Force -fstack-reuse=none in case -fsanitize-address-use-after-scope
     is enabled.  */
  if (opts->x_flag_sanitize_address_use_after_scope)
    {
      if (opts->x_flag_stack_reuse != SR_NONE
	  && opts_set->x_flag_stack_reuse != SR_NONE)
	error_at (loc,
		  "%<-fsanitize-address-use-after-scope%> requires "
		  "%<-fstack-reuse=none%> option");

      opts->x_flag_stack_reuse = SR_NONE;
    }

  if ((opts->x_flag_sanitize & SANITIZE_USER_ADDRESS) && opts->x_flag_tm)
    sorry ("transactional memory is not supported with %<-fsanitize=address%>");

  if ((opts->x_flag_sanitize & SANITIZE_KERNEL_ADDRESS) && opts->x_flag_tm)
    sorry ("transactional memory is not supported with "
	   "%<-fsanitize=kernel-address%>");

  /* Currently live patching is not support for LTO.  */
  if (opts->x_flag_live_patching == LIVE_PATCHING_INLINE_ONLY_STATIC && opts->x_flag_lto)
    sorry ("live patching (with %qs) is not supported with LTO",
	   "inline-only-static");

  /* Currently vtable verification is not supported for LTO */
  if (opts->x_flag_vtable_verify && opts->x_flag_lto)
    sorry ("vtable verification is not supported with LTO");

  /* Control IPA optimizations based on different -flive-patching level.  */
  if (opts->x_flag_live_patching)
    control_options_for_live_patching (opts, opts_set,
				       opts->x_flag_live_patching,
				       loc);

  /* Allow cunroll to grow size accordingly.  */
  if (!opts_set->x_flag_cunroll_grow_size)
    opts->x_flag_cunroll_grow_size
      = (opts->x_flag_unroll_loops
         || opts->x_flag_peel_loops
         || opts->x_optimize >= 3);

  /* With -fcx-limited-range, we do cheap and quick complex arithmetic.  */
  if (opts->x_flag_cx_limited_range)
    opts->x_flag_complex_method = 0;
  else if (opts_set->x_flag_cx_limited_range)
    opts->x_flag_complex_method = opts->x_flag_default_complex_method;

  /* With -fcx-fortran-rules, we do something in-between cheap and C99.  */
  if (opts->x_flag_cx_fortran_rules)
    opts->x_flag_complex_method = 1;
  else if (opts_set->x_flag_cx_fortran_rules)
    opts->x_flag_complex_method = opts->x_flag_default_complex_method;

  /* Use -fvect-cost-model=cheap instead of -fvect-cost-mode=very-cheap
     by default with explicit -ftree-{loop,slp}-vectorize.  */
  if (opts->x_optimize == 2
      && (opts_set->x_flag_tree_loop_vectorize
	  || opts_set->x_flag_tree_vectorize))
    SET_OPTION_IF_UNSET (opts, opts_set, flag_vect_cost_model,
			 VECT_COST_MODEL_CHEAP);

  if (opts->x_flag_gtoggle)
    {
      /* Make sure to process -gtoggle only once.  */
      opts->x_flag_gtoggle = false;
      if (opts->x_debug_info_level == DINFO_LEVEL_NONE)
	{
	  opts->x_debug_info_level = DINFO_LEVEL_NORMAL;

	  if (opts->x_write_symbols == NO_DEBUG)
	    opts->x_write_symbols = PREFERRED_DEBUGGING_TYPE;
	}
      else
	opts->x_debug_info_level = DINFO_LEVEL_NONE;
    }

  if (!opts_set->x_debug_nonbind_markers_p)
    opts->x_debug_nonbind_markers_p
      = (opts->x_optimize
	 && opts->x_debug_info_level >= DINFO_LEVEL_NORMAL
	 && (dwarf_debuginfo_p (opts) || codeview_debuginfo_p ())
	 && !(opts->x_flag_selective_scheduling
	      || opts->x_flag_selective_scheduling2));

  /* We know which debug output will be used so we can set flag_var_tracking
     and flag_var_tracking_uninit if the user has not specified them.  */
  if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL
      || (!dwarf_debuginfo_p (opts) && !codeview_debuginfo_p ())
      /* We have not yet initialized debug hooks so match that to check
	 whether we're only doing DWARF2_LINENO_DEBUGGING_INFO.  */
#ifndef DWARF2_DEBUGGING_INFO
      || true
#endif
     )
    {
      if ((opts_set->x_flag_var_tracking && opts->x_flag_var_tracking == 1)
	  || (opts_set->x_flag_var_tracking_uninit
	      && opts->x_flag_var_tracking_uninit == 1))
	{
	  if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL)
	    warning_at (UNKNOWN_LOCATION, 0,
			"variable tracking requested, but useless unless "
			"producing debug info");
	  else
	    warning_at (UNKNOWN_LOCATION, 0,
			"variable tracking requested, but not supported "
			"by this debug format");
	}
      opts->x_flag_var_tracking = 0;
      opts->x_flag_var_tracking_uninit = 0;
      opts->x_flag_var_tracking_assignments = 0;
    }

  /* One could use EnabledBy, but it would lead to a circular dependency.  */
  if (!opts_set->x_flag_var_tracking_uninit)
    opts->x_flag_var_tracking_uninit = opts->x_flag_var_tracking;

  if (!opts_set->x_flag_var_tracking_assignments)
    opts->x_flag_var_tracking_assignments
      = (opts->x_flag_var_tracking
	 && !(opts->x_flag_selective_scheduling
	      || opts->x_flag_selective_scheduling2));

  if (opts->x_flag_var_tracking_assignments_toggle)
    opts->x_flag_var_tracking_assignments
      = !opts->x_flag_var_tracking_assignments;

  if (opts->x_flag_var_tracking_assignments && !opts->x_flag_var_tracking)
    opts->x_flag_var_tracking = opts->x_flag_var_tracking_assignments = -1;

  if (opts->x_flag_var_tracking_assignments
      && (opts->x_flag_selective_scheduling
	  || opts->x_flag_selective_scheduling2))
    warning_at (loc, 0,
		"var-tracking-assignments changes selective scheduling");

  if (opts->x_flag_syntax_only)
    {
      opts->x_write_symbols = NO_DEBUG;
      opts->x_profile_flag = 0;
    }

  if (opts->x_warn_strict_flex_arrays)
    if (opts->x_flag_strict_flex_arrays == 0)
      {
	opts->x_warn_strict_flex_arrays = 0;
	warning_at (UNKNOWN_LOCATION, 0,
		    "%<-Wstrict-flex-arrays%> is ignored when"
		    " %<-fstrict-flex-arrays%> is not present");
      }

  diagnose_options (opts, opts_set, loc);
}

/* The function diagnoses incompatible combinations for provided options
   (OPTS and OPTS_SET) at a given LOCation.  The function is called both
   when command line is parsed (after the target optimization hook) and
   when an optimize/target attribute (or pragma) is used.  */

void diagnose_options (gcc_options *opts, gcc_options *opts_set,
		       location_t loc)
{
  /* The optimization to partition hot and cold basic blocks into separate
     sections of the .o and executable files does not work (currently)
     with exception handling.  This is because there is no support for
     generating unwind info.  If opts->x_flag_exceptions is turned on
     we need to turn off the partitioning optimization.  */

  enum unwind_info_type ui_except
    = targetm_common.except_unwind_info (opts);

  if (opts->x_flag_exceptions
      && opts->x_flag_reorder_blocks_and_partition
      && (ui_except == UI_SJLJ || ui_except >= UI_TARGET))
    {
      if (opts_set->x_flag_reorder_blocks_and_partition)
	inform (loc,
		"%<-freorder-blocks-and-partition%> does not work "
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
		"%<-freorder-blocks-and-partition%> does not support "
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
		"%<-freorder-blocks-and-partition%> does not work "
		"on this architecture");
      opts->x_flag_reorder_blocks_and_partition = 0;
      opts->x_flag_reorder_blocks = 1;
    }


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

/* Data structure used to print list of valid option values.  */

class option_help_tuple
{
public:
  option_help_tuple (int code, vec<const char *> values):
    m_code (code), m_values (values)
  {}

  /* Code of an option.  */
  int m_code;

  /* List of possible values.  */
  vec<const char *> m_values;
};

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
  char new_help[256];

  if (!opts->x_help_printed)
    opts->x_help_printed = XCNEWVAR (char, cl_options_count);

  if (!opts->x_help_enum_printed)
    opts->x_help_enum_printed = XCNEWVAR (char, cl_enums_count);

  auto_vec<option_help_tuple> help_tuples;

  for (i = 0; i < cl_options_count; i++)
    {
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

      /* If an option contains a language specification,
	 exclude it from common unless all languages are present.  */
      if ((include_flags & CL_COMMON)
	  && !(option->flags & CL_DRIVER)
	  && (option->flags & CL_LANG_ALL)
	  && (option->flags & CL_LANG_ALL) != CL_LANG_ALL)
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

      if (option->alias_target < N_OPTS
	  && cl_options [option->alias_target].help)
	{
	  const struct cl_option *target = cl_options + option->alias_target;
	  if (option->help == NULL)
	    {
	      /* The option is undocumented but is an alias for an option that
		 is documented.  If the option has alias arguments, then its
		 purpose is to provide certain arguments to the other option, so
		 inform the reader of this.  Otherwise, point the reader to the
		 other option in preference to the former.  */

	      if (option->alias_arg)
		{
		  if (option->neg_alias_arg)
		    snprintf (new_help, sizeof new_help,
			      _("Same as %s%s (or, in negated form, %s%s)."),
			      target->opt_text, option->alias_arg,
			      target->opt_text, option->neg_alias_arg);
		  else
		    snprintf (new_help, sizeof new_help,
			      _("Same as %s%s."),
			      target->opt_text, option->alias_arg);
		}
	      else
		snprintf (new_help, sizeof new_help,
			  _("Same as %s."),
			  target->opt_text);
	    }
	  else
	    {
	      /* For documented options with aliases, mention the aliased
		 option's name for reference.  */
	      snprintf (new_help, sizeof new_help,
			_("%s  Same as %s."),
			help, cl_options [option->alias_target].opt_text);
	    }

	  help = new_help;
	}

      if (option->warn_message)
	{
	  /* Mention that the use of the option will trigger a warning.  */
	  if (help == new_help)
	    snprintf (new_help + strlen (new_help),
		      sizeof new_help - strlen (new_help),
		      "  %s", _(use_diagnosed_msg));
	  else
	    snprintf (new_help, sizeof new_help,
		      "%s  %s", help, _(use_diagnosed_msg));

	  help = new_help;
	}

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

	  /* Set to print whether the option is enabled or disabled,
	     or, if it's an alias for another option, the name of
	     the aliased option.  */
	  bool print_state = false;

	  if (flag_var != NULL
	      && option->var_type != CLVC_DEFER)
	    {
	      /* If OPTION is only available for a specific subset
		 of languages other than this one, mention them.  */
	      bool avail_for_lang = true;
	      if (unsigned langset = option->flags & CL_LANG_ALL)
		{
		  if (!(langset & lang_mask))
		    {
		      avail_for_lang = false;
		      strcat (new_help, _("[available in "));
		      for (unsigned i = 0, n = 0; (1U << i) < CL_LANG_ALL; ++i)
			if (langset & (1U << i))
			  {
			    if (n++)
			      strcat (new_help, ", ");
			    strcat (new_help, lang_names[i]);
			  }
		      strcat (new_help, "]");
		    }
		}
	      if (!avail_for_lang)
		; /* Print nothing else if the option is not available
		     in the current language.  */
	      else if (option->flags & CL_JOINED)
		{
		  if (option->var_type == CLVC_STRING)
		    {
		      if (* (const char **) flag_var != NULL)
			snprintf (new_help + strlen (new_help),
				  sizeof (new_help) - strlen (new_help),
				  "%s", * (const char **) flag_var);
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
				"%s", arg);
		    }
		  else
		    {
		      if (option->cl_host_wide_int)
			sprintf (new_help + strlen (new_help),
				 _("%llu bytes"), (unsigned long long)
				 *(unsigned HOST_WIDE_INT *) flag_var);
		      else
			sprintf (new_help + strlen (new_help),
				 "%i", * (int *) flag_var);
		    }
		}
	      else
		print_state = true;
	    }
	  else
	    /* When there is no argument, print the option state only
	       if the option takes no argument.  */
	    print_state = !(option->flags & CL_JOINED);

	  if (print_state)
	    {
	      if (option->alias_target < N_OPTS
		  && option->alias_target != OPT_SPECIAL_warn_removed
		  && option->alias_target != OPT_SPECIAL_ignore
		  && option->alias_target != OPT_SPECIAL_input_file
		  && option->alias_target != OPT_SPECIAL_program_name
		  && option->alias_target != OPT_SPECIAL_unknown)
		{
		  const struct cl_option *target
		    = &cl_options[option->alias_target];
		  sprintf (new_help + strlen (new_help), "%s%s",
			   target->opt_text,
			   option->alias_arg ? option->alias_arg : "");
		}
	      else if (option->alias_target == OPT_SPECIAL_ignore)
		strcat (new_help, ("[ignored]"));
	      else
		{
		  /* Print the state for an on/off option.  */
		  int ena = option_enabled (i, lang_mask, opts);
		  if (ena > 0)
		    strcat (new_help, _("[enabled]"));
		  else if (ena == 0)
		    strcat (new_help, _("[disabled]"));
		}
	    }

	  help = new_help;
	}

      if (option->range_max != -1 && tab == NULL)
	{
	  char b[128];
	  snprintf (b, sizeof (b), "<%d,%d>", option->range_min,
		    option->range_max);
	  opt = concat (opt, b, NULL);
	  len += strlen (b);
	}

      wrap_help (help, opt, len, columns);
      displayed = true;

      if (option->var_type == CLVC_ENUM
	  && opts->x_help_enum_printed[option->var_enum] != 2)
	opts->x_help_enum_printed[option->var_enum] = 1;
      else
	{
	  vec<const char *> option_values
	    = targetm_common.get_valid_option_values (i, NULL);
	  if (!option_values.is_empty ())
	    help_tuples.safe_push (option_help_tuple (i, option_values));
	}
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
	      printf (_(" None found.  Use --help=%s to show *all* the options supported by the %s front-end.\n"),
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

  for (unsigned i = 0; i < help_tuples.length (); i++)
    {
      const struct cl_option *option = cl_options + help_tuples[i].m_code;
      printf (_("  Known valid arguments for %s option:\n   "),
	      option->opt_text);
      for (unsigned j = 0; j < help_tuples[i].m_values.length (); j++)
	printf (" %s", help_tuples[i].m_values[j]);
      printf ("\n\n");
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
      opts->x_help_columns = get_terminal_width ();
      if (opts->x_help_columns == INT_MAX)
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
	  description = _("The following options control parameters");
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
	      internal_error ("unrecognized %<include_flags 0x%x%> passed "
			      "to %<print_specific_help%>",
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

/* Enable FDO-related flags.  */

static void
enable_fdo_optimizations (struct gcc_options *opts,
			  struct gcc_options *opts_set,
			  int value)
{
  SET_OPTION_IF_UNSET (opts, opts_set, flag_branch_probabilities, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_profile_values, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_unroll_loops, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_peel_loops, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_tracer, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_value_profile_transformations,
		       value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_inline_functions, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_ipa_cp, value);
  if (value)
    {
      SET_OPTION_IF_UNSET (opts, opts_set, flag_ipa_cp_clone, 1);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_ipa_bit_cp, 1);
    }
  SET_OPTION_IF_UNSET (opts, opts_set, flag_predictive_commoning, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_split_loops, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_unswitch_loops, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_gcse_after_reload, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_tree_loop_vectorize, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_tree_slp_vectorize, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_version_loops_for_strides, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_vect_cost_model,
		       VECT_COST_MODEL_DYNAMIC);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_tree_loop_distribute_patterns,
		       value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_loop_interchange, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_unroll_jam, value);
  SET_OPTION_IF_UNSET (opts, opts_set, flag_tree_loop_distribution, value);
}

/* -f{,no-}sanitize{,-recover}= suboptions.  */
const struct sanitizer_opts_s sanitizer_opts[] =
{
#define SANITIZER_OPT(name, flags, recover, trap) \
    { #name, flags, sizeof #name - 1, recover, trap }
  SANITIZER_OPT (address, (SANITIZE_ADDRESS | SANITIZE_USER_ADDRESS), true,
		 false),
  SANITIZER_OPT (hwaddress, (SANITIZE_HWADDRESS | SANITIZE_USER_HWADDRESS),
		 true, false),
  SANITIZER_OPT (kernel-address, (SANITIZE_ADDRESS | SANITIZE_KERNEL_ADDRESS),
		 true, false),
  SANITIZER_OPT (kernel-hwaddress,
		 (SANITIZE_HWADDRESS | SANITIZE_KERNEL_HWADDRESS),
		 true, false),
  SANITIZER_OPT (pointer-compare, SANITIZE_POINTER_COMPARE, true, false),
  SANITIZER_OPT (pointer-subtract, SANITIZE_POINTER_SUBTRACT, true, false),
  SANITIZER_OPT (thread, SANITIZE_THREAD, false, false),
  SANITIZER_OPT (leak, SANITIZE_LEAK, false, false),
  SANITIZER_OPT (shift, SANITIZE_SHIFT, true, true),
  SANITIZER_OPT (shift-base, SANITIZE_SHIFT_BASE, true, true),
  SANITIZER_OPT (shift-exponent, SANITIZE_SHIFT_EXPONENT, true, true),
  SANITIZER_OPT (integer-divide-by-zero, SANITIZE_DIVIDE, true, true),
  SANITIZER_OPT (undefined, SANITIZE_UNDEFINED, true, true),
  SANITIZER_OPT (unreachable, SANITIZE_UNREACHABLE, false, true),
  SANITIZER_OPT (vla-bound, SANITIZE_VLA, true, true),
  SANITIZER_OPT (return, SANITIZE_RETURN, false, true),
  SANITIZER_OPT (null, SANITIZE_NULL, true, true),
  SANITIZER_OPT (signed-integer-overflow, SANITIZE_SI_OVERFLOW, true, true),
  SANITIZER_OPT (bool, SANITIZE_BOOL, true, true),
  SANITIZER_OPT (enum, SANITIZE_ENUM, true, true),
  SANITIZER_OPT (float-divide-by-zero, SANITIZE_FLOAT_DIVIDE, true, true),
  SANITIZER_OPT (float-cast-overflow, SANITIZE_FLOAT_CAST, true, true),
  SANITIZER_OPT (bounds, SANITIZE_BOUNDS, true, true),
  SANITIZER_OPT (bounds-strict, SANITIZE_BOUNDS | SANITIZE_BOUNDS_STRICT, true,
		 true),
  SANITIZER_OPT (alignment, SANITIZE_ALIGNMENT, true, true),
  SANITIZER_OPT (nonnull-attribute, SANITIZE_NONNULL_ATTRIBUTE, true, true),
  SANITIZER_OPT (returns-nonnull-attribute, SANITIZE_RETURNS_NONNULL_ATTRIBUTE,
		 true, true),
  SANITIZER_OPT (object-size, SANITIZE_OBJECT_SIZE, true, true),
  SANITIZER_OPT (vptr, SANITIZE_VPTR, true, false),
  SANITIZER_OPT (pointer-overflow, SANITIZE_POINTER_OVERFLOW, true, true),
  SANITIZER_OPT (builtin, SANITIZE_BUILTIN, true, true),
  SANITIZER_OPT (shadow-call-stack, SANITIZE_SHADOW_CALL_STACK, false, false),
  SANITIZER_OPT (all, ~0U, true, true),
#undef SANITIZER_OPT
  { NULL, 0U, 0UL, false, false }
};

/* -fzero-call-used-regs= suboptions.  */
const struct zero_call_used_regs_opts_s zero_call_used_regs_opts[] =
{
#define ZERO_CALL_USED_REGS_OPT(name, flags) \
    { #name, flags }
  ZERO_CALL_USED_REGS_OPT (skip, zero_regs_flags::SKIP),
  ZERO_CALL_USED_REGS_OPT (used-gpr-arg, zero_regs_flags::USED_GPR_ARG),
  ZERO_CALL_USED_REGS_OPT (used-gpr, zero_regs_flags::USED_GPR),
  ZERO_CALL_USED_REGS_OPT (used-arg, zero_regs_flags::USED_ARG),
  ZERO_CALL_USED_REGS_OPT (used, zero_regs_flags::USED),
  ZERO_CALL_USED_REGS_OPT (all-gpr-arg, zero_regs_flags::ALL_GPR_ARG),
  ZERO_CALL_USED_REGS_OPT (all-gpr, zero_regs_flags::ALL_GPR),
  ZERO_CALL_USED_REGS_OPT (all-arg, zero_regs_flags::ALL_ARG),
  ZERO_CALL_USED_REGS_OPT (all, zero_regs_flags::ALL),
  ZERO_CALL_USED_REGS_OPT (leafy-gpr-arg, zero_regs_flags::LEAFY_GPR_ARG),
  ZERO_CALL_USED_REGS_OPT (leafy-gpr, zero_regs_flags::LEAFY_GPR),
  ZERO_CALL_USED_REGS_OPT (leafy-arg, zero_regs_flags::LEAFY_ARG),
  ZERO_CALL_USED_REGS_OPT (leafy, zero_regs_flags::LEAFY),
#undef ZERO_CALL_USED_REGS_OPT
  {NULL, 0U}
};

/* A struct for describing a run of chars within a string.  */

class string_fragment
{
public:
  string_fragment (const char *start, size_t len)
  : m_start (start), m_len (len) {}

  const char *m_start;
  size_t m_len;
};

/* Specialization of edit_distance_traits for string_fragment,
   for use by get_closest_sanitizer_option.  */

template <>
struct edit_distance_traits<const string_fragment &>
{
  static size_t get_length (const string_fragment &fragment)
  {
    return fragment.m_len;
  }

  static const char *get_string (const string_fragment &fragment)
  {
    return fragment.m_start;
  }
};

/* Given ARG, an unrecognized sanitizer option, return the best
   matching sanitizer option, or NULL if there isn't one.
   OPTS is array of candidate sanitizer options.
   CODE is OPT_fsanitize_, OPT_fsanitize_recover_ or OPT_fsanitize_trap_.
   VALUE is non-zero for the regular form of the option, zero
   for the "no-" form (e.g. "-fno-sanitize-recover=").  */

static const char *
get_closest_sanitizer_option (const string_fragment &arg,
			      const struct sanitizer_opts_s *opts,
			      enum opt_code code, int value)
{
  best_match <const string_fragment &, const char*> bm (arg);
  for (int i = 0; opts[i].name != NULL; ++i)
    {
      /* -fsanitize=all is not valid, so don't offer it.  */
      if (code == OPT_fsanitize_
	  && opts[i].flag == ~0U
	  && value)
	continue;

      /* For -fsanitize-recover= (and not -fno-sanitize-recover=),
	 don't offer the non-recoverable options.  */
      if (code == OPT_fsanitize_recover_
	  && !opts[i].can_recover
	  && value)
	continue;

      /* For -fsanitize-trap= (and not -fno-sanitize-trap=),
	 don't offer the non-trapping options.  */
      if (code == OPT_fsanitize_trap_
	  && !opts[i].can_trap
	  && value)
	continue;

      bm.consider (opts[i].name);
    }
  return bm.get_best_meaningful_candidate ();
}

/* Parse comma separated sanitizer suboptions from P for option SCODE,
   adjust previous FLAGS and return new ones.  If COMPLAIN is false,
   don't issue diagnostics.  */

unsigned int
parse_sanitizer_options (const char *p, location_t loc, int scode,
			 unsigned int flags, int value, bool complain)
{
  enum opt_code code = (enum opt_code) scode;

  while (*p != 0)
    {
      size_t len, i;
      bool found = false;
      const char *comma = strchr (p, ',');

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
      for (i = 0; sanitizer_opts[i].name != NULL; ++i)
	if (len == sanitizer_opts[i].len
	    && memcmp (p, sanitizer_opts[i].name, len) == 0)
	  {
	    /* Handle both -fsanitize and -fno-sanitize cases.  */
	    if (value && sanitizer_opts[i].flag == ~0U)
	      {
		if (code == OPT_fsanitize_)
		  {
		    if (complain)
		      error_at (loc, "%<-fsanitize=all%> option is not valid");
		  }
		else if (code == OPT_fsanitize_recover_)
		  flags |= ~(SANITIZE_THREAD | SANITIZE_LEAK
			     | SANITIZE_UNREACHABLE | SANITIZE_RETURN
			     | SANITIZE_SHADOW_CALL_STACK);
		else /* if (code == OPT_fsanitize_trap_) */
		  flags |= (SANITIZE_UNDEFINED
			    | SANITIZE_UNDEFINED_NONDEFAULT);
	      }
	    else if (value)
	      {
		/* Do not enable -fsanitize-recover=unreachable and
		   -fsanitize-recover=return if -fsanitize-recover=undefined
		   is selected.  */
		if (code == OPT_fsanitize_recover_
		    && sanitizer_opts[i].flag == SANITIZE_UNDEFINED)
		  flags |= (SANITIZE_UNDEFINED
			    & ~(SANITIZE_UNREACHABLE | SANITIZE_RETURN));
		else if (code == OPT_fsanitize_trap_
			 && sanitizer_opts[i].flag == SANITIZE_VPTR)
		  error_at (loc, "%<-fsanitize-trap=%s%> is not supported",
			    sanitizer_opts[i].name);
		else
		  flags |= sanitizer_opts[i].flag;
	      }
	    else
	      {
		flags &= ~sanitizer_opts[i].flag;
		/* Don't always clear SANITIZE_ADDRESS if it was previously
		   set: -fsanitize=address -fno-sanitize=kernel-address should
		   leave SANITIZE_ADDRESS set.  */
		if (flags & (SANITIZE_KERNEL_ADDRESS | SANITIZE_USER_ADDRESS))
		  flags |= SANITIZE_ADDRESS;
	      }
	    found = true;
	    break;
	  }

      if (! found && complain)
	{
	  const char *hint
	    = get_closest_sanitizer_option (string_fragment (p, len),
					    sanitizer_opts, code, value);

	  const char *suffix;
	  if (code == OPT_fsanitize_recover_)
	    suffix = "-recover";
	  else if (code == OPT_fsanitize_trap_)
	    suffix = "-trap";
	  else
	    suffix = "";

	  if (hint)
	    error_at (loc,
		      "unrecognized argument to %<-f%ssanitize%s=%> "
		      "option: %q.*s; did you mean %qs?",
		      value ? "" : "no-",
		      suffix, (int) len, p, hint);
	  else
	    error_at (loc,
		      "unrecognized argument to %<-f%ssanitize%s=%> option: "
		      "%q.*s", value ? "" : "no-",
		      suffix, (int) len, p);
	}

      if (comma == NULL)
	break;
      p = comma + 1;
    }
  return flags;
}

/* Parse string values of no_sanitize attribute passed in VALUE.
   Values are separated with comma.  */

unsigned int
parse_no_sanitize_attribute (char *value)
{
  unsigned int flags = 0;
  unsigned int i;
  char *q = strtok (value, ",");

  while (q != NULL)
    {
      for (i = 0; sanitizer_opts[i].name != NULL; ++i)
	if (strcmp (sanitizer_opts[i].name, q) == 0)
	  {
	    flags |= sanitizer_opts[i].flag;
	    if (sanitizer_opts[i].flag == SANITIZE_UNDEFINED)
	      flags |= SANITIZE_UNDEFINED_NONDEFAULT;
	    break;
	  }

      if (sanitizer_opts[i].name == NULL)
	warning (OPT_Wattributes,
		 "%qs attribute directive ignored", q);

      q = strtok (NULL, ",");
    }

  return flags;
}

/* Parse -fzero-call-used-regs suboptions from ARG, return the FLAGS.  */

unsigned int
parse_zero_call_used_regs_options (const char *arg)
{
  unsigned int flags = 0;

  /* Check to see if the string matches a sub-option name.  */
  for (unsigned int i = 0; zero_call_used_regs_opts[i].name != NULL; ++i)
    if (strcmp (arg, zero_call_used_regs_opts[i].name) == 0)
      {
	flags = zero_call_used_regs_opts[i].flag;
	break;
      }

  if (!flags)
    error ("unrecognized argument to %<-fzero-call-used-regs=%>: %qs", arg);

  return flags;
}

/* Parse -falign-NAME format for a FLAG value.  Return individual
   parsed integer values into RESULT_VALUES array.  If REPORT_ERROR is
   set, print error message at LOC location.  */

bool
parse_and_check_align_values (const char *flag,
			      const char *name,
			      auto_vec<unsigned> &result_values,
			      bool report_error,
			      location_t loc)
{
  char *str = xstrdup (flag);
  for (char *p = strtok (str, ":"); p; p = strtok (NULL, ":"))
    {
      char *end;
      int v = strtol (p, &end, 10);
      if (*end != '\0' || v < 0)
	{
	  if (report_error)
	    error_at (loc, "invalid arguments for %<-falign-%s%> option: %qs",
		      name, flag);

	  return false;
	}

      result_values.safe_push ((unsigned)v);
    }

  free (str);

  /* Check that we have a correct number of values.  */
  if (result_values.is_empty () || result_values.length () > 4)
    {
      if (report_error)
	error_at (loc, "invalid number of arguments for %<-falign-%s%> "
		  "option: %qs", name, flag);
      return false;
    }

  for (unsigned i = 0; i < result_values.length (); i++)
    if (result_values[i] > MAX_CODE_ALIGN_VALUE)
      {
	if (report_error)
	  error_at (loc, "%<-falign-%s%> is not between 0 and %d",
		    name, MAX_CODE_ALIGN_VALUE);
	return false;
      }

  return true;
}

/* Check that alignment value FLAG for -falign-NAME is valid at a given
   location LOC. OPT_STR points to the stored -falign-NAME=argument and
   OPT_FLAG points to the associated -falign-NAME on/off flag.  */

static void
check_alignment_argument (location_t loc, const char *flag, const char *name,
			  int *opt_flag, const char **opt_str)
{
  auto_vec<unsigned> align_result;
  parse_and_check_align_values (flag, name, align_result, true, loc);

  if (align_result.length() >= 1 && align_result[0] == 0)
    {
      *opt_flag = 1;
      *opt_str = NULL;
    }
}

/* Parse argument of -fpatchable-function-entry option ARG and store
   corresponding values to PATCH_AREA_SIZE and PATCH_AREA_START.
   If REPORT_ERROR is set to true, generate error for a problematic
   option arguments.  */

void
parse_and_check_patch_area (const char *arg, bool report_error,
			    HOST_WIDE_INT *patch_area_size,
			    HOST_WIDE_INT *patch_area_start)
{
  *patch_area_size = 0;
  *patch_area_start = 0;

  if (arg == NULL)
    return;

  char *patch_area_arg = xstrdup (arg);
  char *comma = strchr (patch_area_arg, ',');
  if (comma)
    {
      *comma = '\0';
      *patch_area_size = integral_argument (patch_area_arg);
      *patch_area_start = integral_argument (comma + 1);
    }
  else
    *patch_area_size = integral_argument (patch_area_arg);

  if (*patch_area_size < 0
      || *patch_area_size > USHRT_MAX
      || *patch_area_start < 0
      || *patch_area_start > USHRT_MAX
      || *patch_area_size < *patch_area_start)
    if (report_error)
      error ("invalid arguments for %<-fpatchable-function-entry%>");

  free (patch_area_arg);
}

/* Print options enabled by -fhardened.  Keep this in sync with the manual!  */

static void
print_help_hardened ()
{
  printf ("%s\n", "The following options are enabled by -fhardened:");
  /* Unfortunately, I can't seem to use targetm.fortify_source_default_level
     here.  */
  printf ("  %s\n", "-D_FORTIFY_SOURCE=3 (or =2 for glibc < 2.35)");
  printf ("  %s\n", "-D_GLIBCXX_ASSERTIONS");
  printf ("  %s\n", "-ftrivial-auto-var-init=zero");
#ifdef HAVE_LD_PIE
  printf ("  %s  %s\n", "-fPIE", "-pie");
#endif
  if (HAVE_LD_NOW_SUPPORT)
    printf ("  %s\n", "-Wl,-z,now");
  if (HAVE_LD_RELRO_SUPPORT)
    printf ("  %s\n", "-Wl,-z,relro");
  printf ("  %s\n", "-fstack-protector-strong");
  printf ("  %s\n", "-fstack-clash-protection");
  printf ("  %s\n", "-fcf-protection=full");
  putchar ('\n');
}

/* Print help when OPT__help_ is set.  */

void
print_help (struct gcc_options *opts, unsigned int lang_mask,
	    const char *help_option_argument)
{
  const char *a = help_option_argument;
  unsigned int include_flags = 0;
  /* Note - by default we include undocumented options when listing
     specific classes.  If you only want to see documented options
     then add ",^undocumented" to the --help= option.  E.g.:

     --help=target,^undocumented  */
  unsigned int exclude_flags = 0;

  if (lang_mask == CL_DRIVER)
    return;

  /* Walk along the argument string, parsing each word in turn.
     The format is:
     arg = [^]{word}[,{arg}]
     word = {optimizers|target|warnings|undocumented|
     params|common|<language>}  */
  while (*a != 0)
    {
      static const struct
	{
	  const char *string;
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
      unsigned int *pflags;
      const char *comma;
      unsigned int lang_flag, specific_flag;
      unsigned int len;
      unsigned int i;

      if (*a == '^')
	{
	  ++a;
	  if (*a == '\0')
	    {
	      error ("missing argument to %qs", "--help=^");
	      break;
	    }
	  pflags = &exclude_flags;
	}
      else
	pflags = &include_flags;

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
	    *pflags |= specific_flag;
	  else
	    {
	      /* The option's argument matches both the start of a
		 language name and the start of an option class name.
		 We have a special case for when the user has
		 specified "--help=c", but otherwise we have to issue
		 a warning.  */
	      if (strncasecmp (a, "c", len) == 0)
		*pflags |= lang_flag;
	      else
		warning (0,
			 "%<--help%> argument %q.*s is ambiguous, "
			 "please be more specific",
			 len, a);
	    }
	}
      else if (lang_flag != 0)
	*pflags |= lang_flag;
      else if (strncasecmp (a, "hardened", len) == 0)
	print_help_hardened ();
      else
	warning (0,
		 "unrecognized argument to %<--help=%> option: %q.*s",
		 len, a);

      if (comma == NULL)
	break;
      a = comma + 1;
    }

  /* We started using PerFunction/Optimization for parameters and
     a warning.  We should exclude these from optimization options.  */
  if (include_flags & CL_OPTIMIZATION)
    exclude_flags |= CL_WARNING;
  if (!(include_flags & CL_PARAMS))
    exclude_flags |= CL_PARAMS;

  if (include_flags)
    print_specific_help (include_flags, exclude_flags, 0, opts,
			 lang_mask);
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
		      diagnostic_context *dc,
		      void (*target_option_override_hook) (void))
{
  size_t scode = decoded->opt_index;
  const char *arg = decoded->arg;
  HOST_WIDE_INT value = decoded->value;
  enum opt_code code = (enum opt_code) scode;

  gcc_assert (decoded->canonical_option_num_elements <= 2);

  switch (code)
    {
    case OPT__help:
      {
	unsigned int all_langs_mask = (1U << cl_lang_count) - 1;
	unsigned int undoc_mask;
	unsigned int i;

	if (lang_mask == CL_DRIVER)
	  break;

	undoc_mask = ((opts->x_verbose_flag | opts->x_extra_warnings)
		      ? 0
		      : CL_UNDOCUMENTED);
	target_option_override_hook ();
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

      target_option_override_hook ();
      print_specific_help (CL_TARGET, 0, 0, opts, lang_mask);
      opts->x_exit_after_options = true;
      break;

    case OPT__help_:
      {
	help_option_arguments.safe_push (arg);
	opts->x_exit_after_options = true;
	break;
      }

    case OPT__version:
      if (lang_mask == CL_DRIVER)
	break;

      opts->x_exit_after_options = true;
      break;

    case OPT__completion_:
      break;

    case OPT_fsanitize_:
      opts_set->x_flag_sanitize = true;
      opts->x_flag_sanitize
	= parse_sanitizer_options (arg, loc, code,
				   opts->x_flag_sanitize, value, true);

      /* Kernel ASan implies normal ASan but does not yet support
	 all features.  */
      if (opts->x_flag_sanitize & SANITIZE_KERNEL_ADDRESS)
	{
	  SET_OPTION_IF_UNSET (opts, opts_set,
			       param_asan_instrumentation_with_call_threshold,
			       0);
	  SET_OPTION_IF_UNSET (opts, opts_set, param_asan_globals, 0);
	  SET_OPTION_IF_UNSET (opts, opts_set, param_asan_stack, 0);
	  SET_OPTION_IF_UNSET (opts, opts_set, param_asan_protect_allocas, 0);
	  SET_OPTION_IF_UNSET (opts, opts_set, param_asan_use_after_return, 0);
	}
      if (opts->x_flag_sanitize & SANITIZE_KERNEL_HWADDRESS)
	{
	  SET_OPTION_IF_UNSET (opts, opts_set,
			       param_hwasan_instrument_stack, 0);
	  SET_OPTION_IF_UNSET (opts, opts_set,
			       param_hwasan_random_frame_tag, 0);
	  SET_OPTION_IF_UNSET (opts, opts_set,
			       param_hwasan_instrument_allocas, 0);
	}
      break;

    case OPT_fsanitize_recover_:
      opts->x_flag_sanitize_recover
	= parse_sanitizer_options (arg, loc, code,
				   opts->x_flag_sanitize_recover, value, true);
      break;

    case OPT_fsanitize_trap_:
      opts->x_flag_sanitize_trap
	= parse_sanitizer_options (arg, loc, code,
				   opts->x_flag_sanitize_trap, value, true);
      break;

    case OPT_fasan_shadow_offset_:
      /* Deferred.  */
      break;

    case OPT_fsanitize_address_use_after_scope:
      opts->x_flag_sanitize_address_use_after_scope = value;
      break;

    case OPT_fsanitize_recover:
      if (value)
	opts->x_flag_sanitize_recover
	  |= (SANITIZE_UNDEFINED | SANITIZE_UNDEFINED_NONDEFAULT)
	     & ~(SANITIZE_UNREACHABLE | SANITIZE_RETURN);
      else
	opts->x_flag_sanitize_recover
	  &= ~(SANITIZE_UNDEFINED | SANITIZE_UNDEFINED_NONDEFAULT);
      break;

    case OPT_fsanitize_trap:
      if (value)
	opts->x_flag_sanitize_trap
	  |= (SANITIZE_UNDEFINED | SANITIZE_UNDEFINED_NONDEFAULT);
      else
	opts->x_flag_sanitize_trap
	  &= ~(SANITIZE_UNDEFINED | SANITIZE_UNDEFINED_NONDEFAULT);
      break;

    case OPT_O:
    case OPT_Os:
    case OPT_Ofast:
    case OPT_Og:
    case OPT_Oz:
      /* Currently handled in a prescan.  */
      break;

    case OPT_Wattributes_:
      if (lang_mask == CL_DRIVER)
	break;

      if (value)
	{
	  error_at (loc, "arguments ignored for %<-Wattributes=%>; use "
		    "%<-Wno-attributes=%> instead");
	  break;
	}
      else if (arg[strlen (arg) - 1] == ',')
	{
	  error_at (loc, "trailing %<,%> in arguments for "
		    "%<-Wno-attributes=%>");
	  break;
	}

      add_comma_separated_to_vector (&opts->x_flag_ignored_attributes, arg);
      break;

    case OPT_Werror:
      dc->set_warning_as_error_requested (value);
      break;

    case OPT_Werror_:
      if (lang_mask == CL_DRIVER)
	break;

      enable_warning_as_error (arg, value, lang_mask, handlers,
			       opts, opts_set, loc, dc);
      break;

    case OPT_Wfatal_errors:
      dc->m_fatal_errors = value;
      break;

    case OPT_Wstack_usage_:
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
      dc->m_warn_system_headers = value;
      break;

    case OPT_aux_info:
      opts->x_flag_gen_aux_info = 1;
      break;

    case OPT_d:
      decode_d_option (arg, opts, loc, dc);
      break;

    case OPT_fcall_used_:
    case OPT_fcall_saved_:
      /* Deferred.  */
      break;

    case OPT_fdbg_cnt_:
      /* Deferred.  */
      break;

    case OPT_fdebug_prefix_map_:
    case OPT_ffile_prefix_map_:
    case OPT_fprofile_prefix_map_:
      /* Deferred.  */
      break;

    case OPT_fcanon_prefix_map:
      flag_canon_prefix_map = value;
      break;

    case OPT_fcallgraph_info:
      opts->x_flag_callgraph_info = CALLGRAPH_INFO_NAKED;
      break;

    case OPT_fcallgraph_info_:
      {
	char *my_arg, *p;
	my_arg = xstrdup (arg);
	p = strtok (my_arg, ",");
	while (p)
	  {
	    if (strcmp (p, "su") == 0)
	      {
		opts->x_flag_callgraph_info |= CALLGRAPH_INFO_STACK_USAGE;
		opts->x_flag_stack_usage_info = true;
	      }
	    else if (strcmp (p, "da") == 0)
	      opts->x_flag_callgraph_info |= CALLGRAPH_INFO_DYNAMIC_ALLOC;
	    else
	      return 0;
	    p = strtok (NULL, ",");
	  }
	free (my_arg);
      }
      break;

    case OPT_fdiagnostics_show_location_:
      dc->set_prefixing_rule ((diagnostic_prefixing_rule_t) value);
      break;

    case OPT_fdiagnostics_show_caret:
      dc->m_source_printing.enabled = value;
      break;

    case OPT_fdiagnostics_show_event_links:
      dc->m_source_printing.show_event_links_p = value;
      break;

    case OPT_fdiagnostics_show_labels:
      dc->m_source_printing.show_labels_p = value;
      break;

    case OPT_fdiagnostics_show_line_numbers:
      dc->m_source_printing.show_line_numbers_p = value;
      break;

    case OPT_fdiagnostics_color_:
      diagnostic_color_init (dc, value);
      break;

    case OPT_fdiagnostics_urls_:
      diagnostic_urls_init (dc, value);
      break;

    case OPT_fdiagnostics_format_:
	{
	  const char *basename = (opts->x_dump_base_name ? opts->x_dump_base_name
				  : opts->x_main_input_basename);
	  gcc_assert (dc);
	  diagnostic_output_format_init (*dc,
					 opts->x_main_input_filename, basename,
					 (enum diagnostics_output_format)value,
					 opts->x_flag_diagnostics_json_formatting);
	  break;
	}

    case OPT_fdiagnostics_add_output_:
      handle_OPT_fdiagnostics_add_output_ (*opts, *dc, arg, loc);
      break;

    case OPT_fdiagnostics_set_output_:
      handle_OPT_fdiagnostics_set_output_ (*opts, *dc, arg, loc);
      break;

    case OPT_fdiagnostics_text_art_charset_:
      dc->set_text_art_charset ((enum diagnostic_text_art_charset)value);
      break;

    case OPT_fdiagnostics_parseable_fixits:
      dc->set_extra_output_kind (value
				 ? EXTRA_DIAGNOSTIC_OUTPUT_fixits_v1
				 : EXTRA_DIAGNOSTIC_OUTPUT_none);
      break;

    case OPT_fdiagnostics_column_unit_:
      dc->m_column_unit = (enum diagnostics_column_unit)value;
      break;

    case OPT_fdiagnostics_column_origin_:
      dc->m_column_origin = value;
      break;

    case OPT_fdiagnostics_escape_format_:
      dc->set_escape_format ((enum diagnostics_escape_format)value);
      break;

    case OPT_fdiagnostics_show_highlight_colors:
      dc->set_show_highlight_colors (value);
      break;

    case OPT_fdiagnostics_show_cwe:
      dc->set_show_cwe (value);
      break;

    case OPT_fdiagnostics_show_rules:
      dc->set_show_rules (value);
      break;

    case OPT_fdiagnostics_path_format_:
      dc->set_path_format ((enum diagnostic_path_format)value);
      break;

    case OPT_fdiagnostics_show_path_depths:
      dc->set_show_path_depths (value);
      break;

    case OPT_fdiagnostics_show_option:
      dc->set_show_option_requested (value);
      break;

    case OPT_fdiagnostics_minimum_margin_width_:
      dc->m_source_printing.min_margin_width = value;
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
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_inline_insns_single,
			   value / 2);
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_inline_insns_auto,
			   value / 2);
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
      pp_set_line_maximum_length (dc->get_reference_printer (), value);
      diagnostic_set_caret_max_width (dc, value);
      break;

    case OPT_fopt_info:
    case OPT_fopt_info_:
      /* Deferred.  */
      break;

    case OPT_foffload_options_:
      /* Deferred.  */
      break;

    case OPT_foffload_abi_:
    case OPT_foffload_abi_host_opts_:
#ifdef ACCEL_COMPILER
      /* Handled in the 'mkoffload's.  */
#else
      error_at (loc,
		"%qs option can be specified only for offload compiler",
		(code == OPT_foffload_abi_) ? "-foffload-abi"
					    : "-foffload-abi-host-opts");
#endif
      break;

    case OPT_fpack_struct_:
      if (value <= 0 || (value & (value - 1)) || value > 16)
	error_at (loc,
		  "structure alignment must be a small power of two, not %wu",
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
      /* FALLTHRU */
    case OPT_fprofile_use:
      enable_fdo_optimizations (opts, opts_set, value);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_profile_reorder_functions,
			   value);
	/* Indirect call profiling should do all useful transformations
	   speculative devirtualization does.  */
      if (opts->x_flag_value_profile_transformations)
	SET_OPTION_IF_UNSET (opts, opts_set, flag_devirtualize_speculatively,
			     false);
      break;

    case OPT_fauto_profile_:
      opts->x_auto_profile_file = xstrdup (arg);
      opts->x_flag_auto_profile = true;
      value = true;
      /* No break here - do -fauto-profile processing. */
      /* FALLTHRU */
    case OPT_fauto_profile:
      enable_fdo_optimizations (opts, opts_set, value);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_profile_correction, value);
      break;

    case OPT_fprofile_generate_:
      opts->x_profile_data_prefix = xstrdup (arg);
      value = true;
      /* No break here - do -fprofile-generate processing. */
      /* FALLTHRU */
    case OPT_fprofile_generate:
      SET_OPTION_IF_UNSET (opts, opts_set, profile_arc_flag, value);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_profile_values, value);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_inline_functions, value);
      SET_OPTION_IF_UNSET (opts, opts_set, flag_ipa_bit_cp, value);
      break;

    case OPT_fprofile_info_section:
      opts->x_profile_info_section = ".gcov_info";
      break;

    case OPT_fpatchable_function_entry_:
      {
	HOST_WIDE_INT patch_area_size, patch_area_start;
	parse_and_check_patch_area (arg, true, &patch_area_size,
				    &patch_area_start);
      }
      break;

    case OPT_ftree_vectorize:
      /* Automatically sets -ftree-loop-vectorize and
	 -ftree-slp-vectorize.  Nothing more to do here.  */
      break;
    case OPT_fzero_call_used_regs_:
      opts->x_flag_zero_call_used_regs
	= parse_zero_call_used_regs_options (arg);
      break;

    case OPT_fshow_column:
      dc->m_show_column = value;
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
	warning_at (loc, 0, "unknown stack check parameter %qs", arg);
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
      set_debug_level (NO_DEBUG, DEFAULT_GDB_EXTENSIONS, arg, opts, opts_set,
                       loc);
      break;

    case OPT_gcodeview:
      set_debug_level (CODEVIEW_DEBUG, false, arg, opts, opts_set, loc);
      if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL)
	opts->x_debug_info_level = DINFO_LEVEL_NORMAL;
      break;

    case OPT_gbtf:
      set_debug_level (BTF_DEBUG, false, arg, opts, opts_set, loc);
      /* set the debug level to level 2, but if already at level 3,
	 don't lower it.  */
      if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL)
	opts->x_debug_info_level = DINFO_LEVEL_NORMAL;
      break;

    case OPT_gctf:
      set_debug_level (CTF_DEBUG, false, arg, opts, opts_set, loc);
      /* CTF generation feeds off DWARF dies.  For optimal CTF, switch debug
	 info level to 2.  If off or at level 1, set it to level 2, but if
	 already at level 3, don't lower it.  */
      if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL
	  && opts->x_ctf_debug_info_level > CTFINFO_LEVEL_NONE)
	opts->x_debug_info_level = DINFO_LEVEL_NORMAL;
      break;

    case OPT_gdwarf:
      if (arg && strlen (arg) != 0)
        {
	  error_at (loc, "%<-gdwarf%s%> is ambiguous; "
		    "use %<-gdwarf-%s%> for DWARF version "
		    "or %<-gdwarf%> %<-g%s%> for debug level", arg, arg, arg);
          break;
        }
      else
        value = opts->x_dwarf_version;

      /* FALLTHRU */
    case OPT_gdwarf_:
      if (value < 2 || value > 5)
	error_at (loc, "dwarf version %wu is not supported", value);
      else
	opts->x_dwarf_version = value;
      set_debug_level (DWARF2_DEBUG, false, "", opts, opts_set, loc);
      break;

    case OPT_ggdb:
      set_debug_level (NO_DEBUG, 2, arg, opts, opts_set, loc);
      break;

    case OPT_gvms:
      set_debug_level (VMS_DEBUG, false, arg, opts, opts_set, loc);
      break;

    case OPT_gz:
    case OPT_gz_:
      /* Handled completely via specs.  */
      break;

    case OPT_pedantic_errors:
      dc->m_pedantic_errors = 1;
      control_warning_option (OPT_Wpedantic, DK_ERROR, NULL, value,
			      loc, lang_mask,
			      handlers, opts, opts_set,
                              dc);
      break;

    case OPT_flto:
      opts->x_flag_lto = value ? "" : NULL;
      break;

    case OPT_flto_:
      if (strcmp (arg, "none") != 0
	  && strcmp (arg, "jobserver") != 0
	  && strcmp (arg, "auto") != 0
	  && atoi (arg) == 0)
	error_at (loc,
		  "unrecognized argument to %<-flto=%> option: %qs", arg);
      break;

    case OPT_w:
      dc->m_inhibit_warnings = true;
      break;

    case OPT_fmax_errors_:
      dc->set_max_errors (value);
      break;

    case OPT_fuse_ld_bfd:
    case OPT_fuse_ld_gold:
    case OPT_fuse_ld_lld:
    case OPT_fuse_ld_mold:
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

    case OPT_fstrict_overflow:
      opts->x_flag_wrapv = !value;
      opts->x_flag_wrapv_pointer = !value;
      if (!value)
	opts->x_flag_trapv = 0;
      break;

    case OPT_fipa_icf:
      opts->x_flag_ipa_icf_functions = value;
      opts->x_flag_ipa_icf_variables = value;
      break;

    case OPT_falign_loops_:
      check_alignment_argument (loc, arg, "loops",
				&opts->x_flag_align_loops,
				&opts->x_str_align_loops);
      break;

    case OPT_falign_jumps_:
      check_alignment_argument (loc, arg, "jumps",
				&opts->x_flag_align_jumps,
				&opts->x_str_align_jumps);
      break;

    case OPT_falign_labels_:
      check_alignment_argument (loc, arg, "labels",
				&opts->x_flag_align_labels,
				&opts->x_str_align_labels);
      break;

    case OPT_falign_functions_:
      check_alignment_argument (loc, arg, "functions",
				&opts->x_flag_align_functions,
				&opts->x_str_align_functions);
      break;

    case OPT_ftabstop_:
      /* It is documented that we silently ignore silly values.  */
      if (value >= 1 && value <= 100)
	dc->m_tabstop = value;
      break;

    case OPT_freport_bug:
      dc->set_report_bug (value);
      break;

    case OPT_fmultiflags:
      gcc_checking_assert (lang_mask == CL_DRIVER);
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
      if (opts->frontend_set_flag_excess_precision == EXCESS_PRECISION_DEFAULT)
	opts->x_flag_excess_precision
	  = set ? EXCESS_PRECISION_FAST : EXCESS_PRECISION_DEFAULT;
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
	  && !opts->x_flag_errno_math
	  && opts->x_flag_excess_precision == EXCESS_PRECISION_FAST);
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
   (OPTS_SET->x_write_symbols storing whether a debug format was passed
   explicitly), location LOC.  EXTENDED is true or false to support
   extended output (2 is special and means "-ggdb" was given).  */
static void
set_debug_level (uint32_t dinfo, int extended, const char *arg,
		 struct gcc_options *opts, struct gcc_options *opts_set,
		 location_t loc)
{
  if (dinfo == NO_DEBUG)
    {
      if (opts->x_write_symbols == NO_DEBUG)
	{
	  opts->x_write_symbols = PREFERRED_DEBUGGING_TYPE;

	  if (extended == 2)
	    {
#if defined DWARF2_DEBUGGING_INFO || defined DWARF2_LINENO_DEBUGGING_INFO
	      if (opts->x_write_symbols & CTF_DEBUG)
		opts->x_write_symbols |= DWARF2_DEBUG;
	      else
		opts->x_write_symbols = DWARF2_DEBUG;
#endif
	    }

	  if (opts->x_write_symbols == NO_DEBUG)
	    warning_at (loc, 0, "target system does not support debug output");
	}
      else if ((opts->x_write_symbols & CTF_DEBUG)
	       || (opts->x_write_symbols & BTF_DEBUG)
	       || (opts->x_write_symbols & CODEVIEW_DEBUG))
	{
	  opts->x_write_symbols |= DWARF2_DEBUG;
	  opts_set->x_write_symbols |= DWARF2_DEBUG;
	}
    }
  else
    {
      /* Make and retain the choice if both CTF and DWARF debug info are to
	 be generated.  */
      if (((dinfo == DWARF2_DEBUG) || (dinfo == CTF_DEBUG))
	  && ((opts->x_write_symbols == (DWARF2_DEBUG|CTF_DEBUG))
	      || (opts->x_write_symbols == DWARF2_DEBUG)
	      || (opts->x_write_symbols == CTF_DEBUG)))
	{
	  opts->x_write_symbols |= dinfo;
	  opts_set->x_write_symbols |= dinfo;
	}
      /* However, CTF and BTF are not allowed together at this time.  */
      else if (((dinfo == DWARF2_DEBUG) || (dinfo == BTF_DEBUG))
	       && ((opts->x_write_symbols == (DWARF2_DEBUG|BTF_DEBUG))
		   || (opts->x_write_symbols == DWARF2_DEBUG)
		   || (opts->x_write_symbols == BTF_DEBUG)))
	{
	  opts->x_write_symbols |= dinfo;
	  opts_set->x_write_symbols |= dinfo;
	}
      else
	{
	  /* Does it conflict with an already selected debug format?  */
	  if (opts_set->x_write_symbols != NO_DEBUG
	      && opts->x_write_symbols != NO_DEBUG
	      && dinfo != opts->x_write_symbols)
	    {
	      gcc_assert (debug_set_count (dinfo) <= 1);
	      error_at (loc, "debug format %qs conflicts with prior selection",
			debug_type_names[debug_set_to_format (dinfo)]);
	    }
	  opts->x_write_symbols = dinfo;
	  opts_set->x_write_symbols = dinfo;
	}
    }

  if (dinfo != BTF_DEBUG)
    {
      /* A debug flag without a level defaults to level 2.
	 If off or at level 1, set it to level 2, but if already
	 at level 3, don't lower it.  */
      if (*arg == '\0')
	{
	  if (dinfo == CTF_DEBUG)
	    opts->x_ctf_debug_info_level = CTFINFO_LEVEL_NORMAL;
	  else if (opts->x_debug_info_level < DINFO_LEVEL_NORMAL)
	    opts->x_debug_info_level = DINFO_LEVEL_NORMAL;
	}
      else
	{
	  int argval = integral_argument (arg);
	  if (argval == -1)
	    error_at (loc, "unrecognized debug output level %qs", arg);
	  else if (argval > 3)
	    error_at (loc, "debug output level %qs is too high", arg);
	  else
	    {
	      if (dinfo == CTF_DEBUG)
		opts->x_ctf_debug_info_level
		  = (enum ctf_debug_info_levels) argval;
	      else
		opts->x_debug_info_level = (enum debug_info_levels) argval;
	    }
	}
    }
  else if (*arg != '\0')
    error_at (loc, "unrecognized btf debug output level %qs", arg);
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
      fatal_error (input_location, "getting core file size maximum limit: %m");
    rlim.rlim_cur = rlim.rlim_max;
    if (setrlimit (RLIMIT_CORE, &rlim) != 0)
      fatal_error (input_location,
		   "setting core file size limit to maximum: %m");
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
      option_proposer op;
      const char *hint = op.suggest_option (new_option);
      if (hint)
	error_at (loc, "%<-W%serror=%s%>: no option %<-%s%>;"
		  " did you mean %<-%s%>?", value ? "" : "no-",
		  arg, new_option, hint);
      else
	error_at (loc, "%<-W%serror=%s%>: no option %<-%s%>",
		  value ? "" : "no-", arg, new_option);
    }
  else if (!(cl_options[option_index].flags & CL_WARNING))
    error_at (loc, "%<-Werror=%s%>: %<-%s%> is not an option that "
	      "controls warnings", arg, new_option);
  else
    {
      const diagnostic_t kind = value ? DK_ERROR : DK_WARNING;
      const char *arg = NULL;

      if (cl_options[option_index].flags & CL_JOINED)
	arg = new_option + cl_options[option_index].opt_len;
      control_warning_option (option_index, (int) kind, arg, value,
			      loc, lang_mask,
			      handlers, opts, opts_set, dc);
    }
  free (new_option);
}

/* Return malloced memory for the name of the option OPTION_INDEX
   which enabled a diagnostic, originally of type
   ORIG_DIAG_KIND but possibly converted to DIAG_KIND by options such
   as -Werror.  */

char *
compiler_diagnostic_option_manager::
make_option_name (diagnostic_option_id option_id,
		  diagnostic_t orig_diag_kind,
		  diagnostic_t diag_kind) const
{
  if (option_id.m_idx)
    {
      /* A warning classified as an error.  */
      if ((orig_diag_kind == DK_WARNING || orig_diag_kind == DK_PEDWARN)
	  && diag_kind == DK_ERROR)
	return concat (cl_options[OPT_Werror_].opt_text,
		       /* Skip over "-W".  */
		       cl_options[option_id.m_idx].opt_text + 2,
		       NULL);
      /* A warning with option.  */
      else
	return xstrdup (cl_options[option_id.m_idx].opt_text);
    }
  /* A warning without option classified as an error.  */
  else if ((orig_diag_kind == DK_WARNING || orig_diag_kind == DK_PEDWARN
	    || diag_kind == DK_WARNING)
	   && m_context.warning_as_error_requested_p ())
    return xstrdup (cl_options[OPT_Werror].opt_text);
  else
    return NULL;
}

/* Get the page within the documentation for this option.  */

static const char *
get_option_html_page (int option_index)
{
  const cl_option *cl_opt = &cl_options[option_index];

#ifdef CL_Fortran
  if ((cl_opt->flags & CL_Fortran) != 0
      /* If it is option common to both C/C++ and Fortran, it is documented
	 in gcc/ rather than gfortran/ docs.  */
      && (cl_opt->flags & CL_C) == 0
#ifdef CL_CXX
      && (cl_opt->flags & CL_CXX) == 0
#endif
     )
    return "gfortran/Error-and-Warning-Options.html";
#endif

  return nullptr;
}

/* Get the url within the documentation for this option, or NULL.  */

label_text
get_option_url_suffix (int option_index, unsigned lang_mask)
{
  if (const char *url = get_opt_url_suffix (option_index, lang_mask))

    return label_text::borrow (url);

  /* Fallback code for some options that aren't handled byt opt_url_suffixes
     e.g. links below "gfortran/".  */
  if (const char *html_page = get_option_html_page (option_index))
    return label_text::take
      (concat (html_page,
	       /* Expect an anchor of the form "index-Wfoo" e.g.
		  <a name="index-Wformat"></a>, and thus an id within
		  the page of "#index-Wformat".  */
	       "#index",
	       cl_options[option_index].opt_text,
	       NULL));

  return label_text ();
}

/* Return malloced memory for a URL describing the option OPTION_INDEX
   which enabled a diagnostic.  */

char *
gcc_diagnostic_option_manager::
make_option_url (diagnostic_option_id option_id) const
{
  if (option_id.m_idx)
    {
      label_text url_suffix = get_option_url_suffix (option_id.m_idx,
						     m_lang_mask);
      if (url_suffix.get ())
	return concat (DOCUMENTATION_ROOT_URL, url_suffix.get (), nullptr);
    }

  return nullptr;
}

/* Return a heap allocated producer with command line options.  */

char *
gen_command_line_string (cl_decoded_option *options,
			 unsigned int options_count)
{
  auto_vec<const char *> switches;
  char *options_string, *tail;
  const char *p;
  size_t len = 0;

  for (unsigned i = 0; i < options_count; i++)
    switch (options[i].opt_index)
      {
      case OPT_o:
      case OPT_d:
      case OPT_dumpbase:
      case OPT_dumpbase_ext:
      case OPT_dumpdir:
      case OPT_quiet:
      case OPT_version:
      case OPT_v:
      case OPT_w:
      case OPT_L:
      case OPT_D:
      case OPT_I:
      case OPT_U:
      case OPT_SPECIAL_unknown:
      case OPT_SPECIAL_ignore:
      case OPT_SPECIAL_warn_removed:
      case OPT_SPECIAL_program_name:
      case OPT_SPECIAL_input_file:
      case OPT_grecord_gcc_switches:
      case OPT_frecord_gcc_switches:
      case OPT__output_pch:
      case OPT_fdiagnostics_show_highlight_colors:
      case OPT_fdiagnostics_show_location_:
      case OPT_fdiagnostics_show_option:
      case OPT_fdiagnostics_show_caret:
      case OPT_fdiagnostics_show_event_links:
      case OPT_fdiagnostics_show_labels:
      case OPT_fdiagnostics_show_line_numbers:
      case OPT_fdiagnostics_color_:
      case OPT_fdiagnostics_format_:
      case OPT_fverbose_asm:
      case OPT____:
      case OPT__sysroot_:
      case OPT_nostdinc:
      case OPT_nostdinc__:
      case OPT_fpreprocessed:
      case OPT_fltrans_output_list_:
      case OPT_fresolution_:
      case OPT_fdebug_prefix_map_:
      case OPT_fmacro_prefix_map_:
      case OPT_ffile_prefix_map_:
      case OPT_fprofile_prefix_map_:
      case OPT_fcanon_prefix_map:
      case OPT_fcompare_debug:
      case OPT_fchecking:
      case OPT_fchecking_:
	/* Ignore these.  */
	continue;
      case OPT_flto_:
	{
	  const char *lto_canonical = "-flto";
	  switches.safe_push (lto_canonical);
	  len += strlen (lto_canonical) + 1;
	  break;
	}
      default:
	if (cl_options[options[i].opt_index].flags
	    & CL_NO_DWARF_RECORD)
	  continue;
	gcc_checking_assert (options[i].canonical_option[0][0] == '-');
	switch (options[i].canonical_option[0][1])
	  {
	  case 'M':
	  case 'i':
	  case 'W':
	    continue;
	  case 'f':
	    if (strncmp (options[i].canonical_option[0] + 2,
			 "dump", 4) == 0)
	      continue;
	    break;
	  default:
	    break;
	  }
	switches.safe_push (options[i].orig_option_with_args_text);
	len += strlen (options[i].orig_option_with_args_text) + 1;
	break;
      }

  options_string = XNEWVEC (char, len + 1);
  tail = options_string;

  unsigned i;
  FOR_EACH_VEC_ELT (switches, i, p)
    {
      len = strlen (p);
      memcpy (tail, p, len);
      tail += len;
      if (i != switches.length () - 1)
	{
	  *tail = ' ';
	  ++tail;
	}
    }

  *tail = '\0';
  return options_string;
}

/* Return a heap allocated producer string including command line options.  */

char *
gen_producer_string (const char *language_string, cl_decoded_option *options,
		     unsigned int options_count)
{
  char *cmdline = gen_command_line_string (options, options_count);
  char *combined = concat (language_string, " ", version_string, " ",
			   cmdline, NULL);
  free (cmdline);
  return combined;
}

#if CHECKING_P

namespace selftest {

/* Verify that get_option_url_suffix works as expected.  */

static void
test_get_option_url_suffix ()
{
  ASSERT_STREQ (get_option_url_suffix (OPT_Wcpp, 0).get (),
		"gcc/Warning-Options.html#index-Wcpp");
  ASSERT_STREQ (get_option_url_suffix (OPT_Wanalyzer_double_free, 0).get (),
		"gcc/Static-Analyzer-Options.html#index-Wanalyzer-double-free");

  /* Test of a D-specific option.  */
#ifdef CL_D
  ASSERT_EQ (get_option_url_suffix (OPT_fbounds_check_, 0).get (), nullptr);
  ASSERT_STREQ (get_option_url_suffix (OPT_fbounds_check_, CL_D).get (),
		"gdc/Runtime-Options.html#index-fbounds-check");

  /* Test of a D-specific override to an option URL.  */
  /* Generic URL.  */
  ASSERT_STREQ (get_option_url_suffix (OPT_fmax_errors_, 0).get (),
		"gcc/Warning-Options.html#index-fmax-errors");
  /* D-specific URL.  */
  ASSERT_STREQ (get_option_url_suffix (OPT_fmax_errors_, CL_D).get (),
		"gdc/Warnings.html#index-fmax-errors");
#endif

#ifdef CL_Fortran
  ASSERT_STREQ
    (get_option_url_suffix (OPT_Wline_truncation, CL_Fortran).get (),
     "gfortran/Error-and-Warning-Options.html#index-Wline-truncation");
#endif
}

/* Verify EnumSet and EnumBitSet requirements.  */

static void
test_enum_sets ()
{
  for (unsigned i = 0; i < cl_options_count; ++i)
    if (cl_options[i].var_type == CLVC_ENUM
	&& cl_options[i].var_value != CLEV_NORMAL)
      {
	const struct cl_enum *e = &cl_enums[cl_options[i].var_enum];
	unsigned HOST_WIDE_INT used_sets = 0;
	unsigned HOST_WIDE_INT mask = 0;
	unsigned highest_set = 0;
	for (unsigned j = 0; e->values[j].arg; ++j)
	  {
	    unsigned set = e->values[j].flags >> CL_ENUM_SET_SHIFT;
	    if (cl_options[i].var_value == CLEV_BITSET)
	      {
		/* For EnumBitSet Set shouldn't be used and Value should
		   be a power of two.  */
		ASSERT_TRUE (set == 0);
		ASSERT_TRUE (pow2p_hwi (e->values[j].value));
		continue;
	      }
	    /* Test that enumerators referenced in EnumSet have all
	       Set(n) on them within the valid range.  */
	    ASSERT_TRUE (set >= 1 && set <= HOST_BITS_PER_WIDE_INT);
	    highest_set = MAX (set, highest_set);
	    used_sets |= HOST_WIDE_INT_1U << (set - 1);
	  }
	if (cl_options[i].var_value == CLEV_BITSET)
	  continue;
	/* If there is just one set, no point to using EnumSet.  */
	ASSERT_TRUE (highest_set >= 2);
	/* Test that there are no gaps in between the sets.  */
	if (highest_set == HOST_BITS_PER_WIDE_INT)
	  ASSERT_TRUE (used_sets == HOST_WIDE_INT_M1U);
	else
	  ASSERT_TRUE (used_sets == (HOST_WIDE_INT_1U << highest_set) - 1);
	for (unsigned int j = 1; j <= highest_set; ++j)
	  {
	    unsigned HOST_WIDE_INT this_mask = 0;
	    for (unsigned k = 0; e->values[k].arg; ++k)
	      {
		unsigned set = e->values[j].flags >> CL_ENUM_SET_SHIFT;
		if (set == j)
		  this_mask |= e->values[j].value;
	      }
	    ASSERT_TRUE ((mask & this_mask) == 0);
	    mask |= this_mask;
	  }
      }
}

/* Run all of the selftests within this file.  */

void
opts_cc_tests ()
{
  test_get_option_url_suffix ();
  test_enum_sets ();
}

} // namespace selftest

#endif /* #if CHECKING_P */
