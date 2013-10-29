/* Command line option handling.  Code involving global state that
   should not be shared with the driver.
   Copyright (C) 2002-2013 Free Software Foundation, Inc.

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
#include "coretypes.h"
#include "diagnostic.h"
#include "opts.h"
#include "flags.h"
#include "ggc.h"
#include "tree.h" /* Required by langhooks.h.  */
#include "gimple.h"
#include "langhooks.h"
#include "tm.h" /* Required by rtl.h.  */
#include "rtl.h"
#include "dbgcnt.h"
#include "debug.h"
#include "lto-streamer.h"
#include "output.h"
#include "plugin.h"
#include "toplev.h"
#include "tree-pass.h"
#include "context.h"

typedef const char *const_char_p; /* For DEF_VEC_P.  */

static vec<const_char_p> ignored_options;

/* Input file names.  */
const char **in_fnames;
unsigned num_in_fnames;

/* Return a malloced slash-separated list of languages in MASK.  */

static char *
write_langs (unsigned int mask)
{
  unsigned int n = 0, len = 0;
  const char *lang_name;
  char *result;

  for (n = 0; (lang_name = lang_names[n]) != 0; n++)
    if (mask & (1U << n))
      len += strlen (lang_name) + 1;

  result = XNEWVEC (char, len);
  len = 0;
  for (n = 0; (lang_name = lang_names[n]) != 0; n++)
    if (mask & (1U << n))
      {
	if (len)
	  result[len++] = '/';
	strcpy (result + len, lang_name);
	len += strlen (lang_name);
      }

  result[len] = 0;

  return result;
}

/* Complain that switch DECODED does not apply to this front end (mask
   LANG_MASK).  */

static void
complain_wrong_lang (const struct cl_decoded_option *decoded,
		     unsigned int lang_mask)
{
  const struct cl_option *option = &cl_options[decoded->opt_index];
  const char *text = decoded->orig_option_with_args_text;
  char *ok_langs = NULL, *bad_lang = NULL;
  unsigned int opt_flags = option->flags;

  if (!lang_hooks.complain_wrong_lang_p (option))
    return;

  opt_flags &= ((1U << cl_lang_count) - 1) | CL_DRIVER;
  if (opt_flags != CL_DRIVER)
    ok_langs = write_langs (opt_flags);
  if (lang_mask != CL_DRIVER)
    bad_lang = write_langs (lang_mask);

  if (opt_flags == CL_DRIVER)
    error ("command line option %qs is valid for the driver but not for %s",
	   text, bad_lang);
  else if (lang_mask == CL_DRIVER)
    gcc_unreachable ();
  else
    /* Eventually this should become a hard error IMO.  */
    warning (0, "command line option %qs is valid for %s but not for %s",
	     text, ok_langs, bad_lang);

  free (ok_langs);
  free (bad_lang);
}

/* Buffer the unknown option described by the string OPT.  Currently,
   we only complain about unknown -Wno-* options if they may have
   prevented a diagnostic. Otherwise, we just ignore them.  Note that
   if we do complain, it is only as a warning, not an error; passing
   the compiler an unrecognised -Wno-* option should never change
   whether the compilation succeeds or fails.  */

static void
postpone_unknown_option_warning (const char *opt)
{
  ignored_options.safe_push (opt);
}

/* Produce a warning for each option previously buffered.  */

void
print_ignored_options (void)
{
  while (!ignored_options.is_empty ())
    {
      const char *opt;

      opt = ignored_options.pop ();
      warning_at (UNKNOWN_LOCATION, 0,
		  "unrecognized command line option \"%s\"", opt);
    }
}

/* Handle an unknown option DECODED, returning true if an error should
   be given.  */

static bool
unknown_option_callback (const struct cl_decoded_option *decoded)
{
  const char *opt = decoded->arg;

  if (opt[1] == 'W' && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-'
      && !(decoded->errors & CL_ERR_NEGATIVE))
    {
      /* We don't generate warnings for unknown -Wno-* options unless
	 we issue diagnostics.  */
      postpone_unknown_option_warning (opt);
      return false;
    }
  else
    return true;
}

/* Handle a front-end option; arguments and return value as for
   handle_option.  */

static bool
lang_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set,
		    const struct cl_decoded_option *decoded,
		    unsigned int lang_mask ATTRIBUTE_UNUSED, int kind,
		    location_t loc,
		    const struct cl_option_handlers *handlers,
		    diagnostic_context *dc)
{
  gcc_assert (opts == &global_options);
  gcc_assert (opts_set == &global_options_set);
  gcc_assert (dc == global_dc);
  gcc_assert (decoded->canonical_option_num_elements <= 2);
  return lang_hooks.handle_option (decoded->opt_index, decoded->arg,
				   decoded->value, kind, loc, handlers);
}

/* Handle FILENAME from the command line.  */

static void
add_input_filename (const char *filename)
{
  num_in_fnames++;
  in_fnames = XRESIZEVEC (const char *, in_fnames, num_in_fnames);
  in_fnames[num_in_fnames - 1] = filename;
}

/* Handle the vector of command line options (located at LOC), storing
   the results of processing DECODED_OPTIONS and DECODED_OPTIONS_COUNT
   in OPTS and OPTS_SET and using DC for diagnostic state.  LANG_MASK
   contains has a single bit set representing the current language.
   HANDLERS describes what functions to call for the options.  */

static void
read_cmdline_options (struct gcc_options *opts, struct gcc_options *opts_set,
		      struct cl_decoded_option *decoded_options,
		      unsigned int decoded_options_count,
		      location_t loc,
		      unsigned int lang_mask,
		      const struct cl_option_handlers *handlers,
		      diagnostic_context *dc)
{
  unsigned int i;

  for (i = 1; i < decoded_options_count; i++)
    {
      if (decoded_options[i].opt_index == OPT_SPECIAL_input_file)
	{
	  /* Input files should only ever appear on the main command
	     line.  */
	  gcc_assert (opts == &global_options);
	  gcc_assert (opts_set == &global_options_set);

	  if (opts->x_main_input_filename == NULL)
	    {
	      opts->x_main_input_filename = decoded_options[i].arg;
	      opts->x_main_input_baselength
		= base_of_path (opts->x_main_input_filename,
				&opts->x_main_input_basename);
	    }
	  add_input_filename (decoded_options[i].arg);
	  continue;
	}

      read_cmdline_option (opts, opts_set,
			   decoded_options + i, loc, lang_mask, handlers,
			   dc);
    }
}

/* Handle -ftree-vectorizer-verbose=ARG by remapping it to -fopt-info.
   It remaps the old verbosity values as following:

   REPORT_NONE ==> No dump is output
   REPORT_VECTORIZED_LOCATIONS ==> "-optimized"
   REPORT_UNVECTORIZED_LOCATIONS ==> "-missed"

   Any higher verbosity levels get mapped to "-all" flags.  */

static void
dump_remap_tree_vectorizer_verbose (const char *arg)
{
  int value = atoi (arg);
  const char *remapped_opt_info = NULL;

  switch (value)
    {
    case 0:
      break;
    case 1:
      remapped_opt_info = "optimized";
      break;
    case 2:
      remapped_opt_info = "missed";
      break;
    default:
      remapped_opt_info = "all";
      break;
    }

  if (remapped_opt_info)
    opt_info_switch_p (remapped_opt_info);
}

/* Language mask determined at initialization.  */
static unsigned int initial_lang_mask;

/* Initialize global options-related settings at start-up.  */

void
init_options_once (void)
{
  /* Perform language-specific options initialization.  */
  initial_lang_mask = lang_hooks.option_lang_mask ();

  lang_hooks.initialize_diagnostics (global_dc);
}

/* Decode command-line options to an array, like
   decode_cmdline_options_to_array and with the same arguments but
   using the default lang_mask.  */

void
decode_cmdline_options_to_array_default_mask (unsigned int argc,
					      const char **argv, 
					      struct cl_decoded_option **decoded_options,
					      unsigned int *decoded_options_count)
{
  decode_cmdline_options_to_array (argc, argv,
				   initial_lang_mask | CL_COMMON | CL_TARGET,
				   decoded_options, decoded_options_count);
}

/* Set *HANDLERS to the default set of option handlers for use in the
   compilers proper (not the driver).  */
void
set_default_handlers (struct cl_option_handlers *handlers)
{
  handlers->unknown_option_callback = unknown_option_callback;
  handlers->wrong_lang_callback = complain_wrong_lang;
  handlers->num_handlers = 3;
  handlers->handlers[0].handler = lang_handle_option;
  handlers->handlers[0].mask = initial_lang_mask;
  handlers->handlers[1].handler = common_handle_option;
  handlers->handlers[1].mask = CL_COMMON;
  handlers->handlers[2].handler = target_handle_option;
  handlers->handlers[2].mask = CL_TARGET;
}

/* Parse command line options and set default flag values.  Do minimal
   options processing.  The decoded options are in *DECODED_OPTIONS
   and *DECODED_OPTIONS_COUNT; settings go in OPTS, OPTS_SET and DC;
   the options are located at LOC.  */
void
decode_options (struct gcc_options *opts, struct gcc_options *opts_set,
		struct cl_decoded_option *decoded_options,
		unsigned int decoded_options_count,
		location_t loc, diagnostic_context *dc)
{
  struct cl_option_handlers handlers;

  unsigned int lang_mask;

  lang_mask = initial_lang_mask;

  set_default_handlers (&handlers);

  default_options_optimization (opts, opts_set,
				decoded_options, decoded_options_count,
				loc, lang_mask, &handlers, dc);

  read_cmdline_options (opts, opts_set,
			decoded_options, decoded_options_count,
			loc, lang_mask,
			&handlers, dc);

  finish_options (opts, opts_set, loc);
}

/* Process common options that have been deferred until after the
   handlers have been called for all options.  */

void
handle_common_deferred_options (void)
{
  unsigned int i;
  cl_deferred_option *opt;
  vec<cl_deferred_option> v;

  if (common_deferred_options)
    v = *((vec<cl_deferred_option> *) common_deferred_options);
  else
    v = vNULL;

  if (flag_dump_all_passed)
    enable_rtl_dump_file ();

  if (flag_opt_info)
    opt_info_switch_p (NULL);

  FOR_EACH_VEC_ELT (v, i, opt)
    {
      switch (opt->opt_index)
	{
	case OPT_fcall_used_:
	  fix_register (opt->arg, 0, 1);
	  break;

	case OPT_fcall_saved_:
	  fix_register (opt->arg, 0, 0);
	  break;

	case OPT_fdbg_cnt_:
	  dbg_cnt_process_opt (opt->arg);
	  break;

	case OPT_fdbg_cnt_list:
	  dbg_cnt_list_all_counters ();
	  break;

	case OPT_fdebug_prefix_map_:
	  add_debug_prefix_map (opt->arg);
	  break;

	case OPT_fdump_:
	  if (!g->get_dumps ()->dump_switch_p (opt->arg))
	    error ("unrecognized command line option %<-fdump-%s%>", opt->arg);
	  break;

        case OPT_fopt_info_:
	  if (!opt_info_switch_p (opt->arg))
	    error ("unrecognized command line option %<-fopt-info-%s%>",
                   opt->arg);
          break;

	case OPT_fenable_:
	case OPT_fdisable_:
	  if (opt->opt_index == OPT_fenable_)
	    enable_pass (opt->arg);
          else
	    disable_pass (opt->arg);
          break;

	case OPT_ffixed_:
	  /* Deferred.  */
	  fix_register (opt->arg, 1, 1);
	  break;

	case OPT_fplugin_:
#ifdef ENABLE_PLUGIN
	  add_new_plugin (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_fplugin_arg_:
#ifdef ENABLE_PLUGIN
	  parse_plugin_arg_opt (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_frandom_seed:
	  /* The real switch is -fno-random-seed.  */
	  if (!opt->value)
	    set_random_seed (NULL);
	  break;

	case OPT_frandom_seed_:
	  set_random_seed (opt->arg);
	  break;

	case OPT_fstack_limit:
	  /* The real switch is -fno-stack-limit.  */
	  if (!opt->value)
	    stack_limit_rtx = NULL_RTX;
	  break;

	case OPT_fstack_limit_register_:
	  {
	    int reg = decode_reg_name (opt->arg);
	    if (reg < 0)
	      error ("unrecognized register name %qs", opt->arg);
	    else
	      stack_limit_rtx = gen_rtx_REG (Pmode, reg);
	  }
	  break;

	case OPT_fstack_limit_symbol_:
	  stack_limit_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (opt->arg));
	  break;

        case OPT_ftree_vectorizer_verbose_:
	  dump_remap_tree_vectorizer_verbose (opt->arg);
          break;

	default:
	  gcc_unreachable ();
	}
    }
}
