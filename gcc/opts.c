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
   matches.  Returns its index in the option array, and N_OPTS on
   failure.

   Complications arise since some options can be suffixed with an
   argument, and multiple complete matches can occur, e.g. -pedantic
   and -pedantic-errors.  Also, some options are only accepted by some
   languages.  If a switch matches for a different language and
   doesn't match any alternatives for the true front end, the index of
   the matched switch is returned anyway.  The caller should check for
   this case.  */
static size_t
find_opt (const char *input, int lang_mask)
{
  size_t md, mn, mx;
  size_t opt_len;
  size_t result = cl_options_count;
  int comp;

  mn = 0;
  mx = cl_options_count;

  while (mx > mn)
    {
      md = (mn + mx) / 2;

      opt_len = cl_options[md].opt_len;
      comp = strncmp (input, cl_options[md].opt_text, opt_len);

      if (comp < 0)
	mx = md;
      else if (comp > 0)
	mn = md + 1;
      else
	{
	  /* The switch matches.  It it an exact match?  */
	  if (input[opt_len] == '\0')
	    return md;
	  else
	    {
	      mn = md + 1;

	      /* If the switch takes no arguments this is not a proper
		 match, so we continue the search (e.g. input="stdc++"
		 match was "stdc").  */
	      if (!(cl_options[md].flags & CL_JOINED))
		continue;

	      /* Is this switch valid for this front end?  */
	      if (!(cl_options[md].flags & lang_mask))
		{
		  /* If subsequently we don't find a better match,
		     return this and let the caller report it as a bad
		     match.  */
		  result = md;
		  continue;
		}

	      /* Two scenarios remain: we have the switch's argument,
		 or we match a longer option.  This can happen with
		 -iwithprefix and -withprefixbefore.  The longest
		 possible option match succeeds.

		 Scan forwards, and return an exact match.  Otherwise
		 return the longest valid option-accepting match (mx).
		 This loops at most twice with current options.  */
	      mx = md;
	      for (md = md + 1; md < cl_options_count; md++)
		{
		  opt_len = cl_options[md].opt_len;
		  comp = strncmp (input, cl_options[md].opt_text, opt_len);
		  if (comp < 0)
		    break;
		  if (comp > 0)
		    continue;
		  if (input[opt_len] == '\0')
		    return md;
		  if (cl_options[md].flags & lang_mask
		      && cl_options[md].flags & CL_JOINED)
		    mx = md;
		}

	      return mx;
	    }
	}
    }

  return result;
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

    case OPT_falign_functions_:
      align_functions = value;
      break;

    case OPT_falign_jumps_:
      align_jumps = value;
      break;

    case OPT_falign_labels_:
      align_labels = value;
      break;

    case OPT_falign_loops_:
      align_loops = value;
      break;

    case OPT_fcall_used_:
      fix_register (arg, 0, 1);
      break;

    case OPT_fcall_saved_:
      fix_register (arg, 0, 0);
      break;

    case OPT_ffast_math:
      set_fast_math_flags (value);
      break;

    case OPT_ffixed_:
      fix_register (arg, 1, 1);
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
