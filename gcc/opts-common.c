/* Command line option handling.
   Copyright (C) 2006, 2007, 2008, 2010 Free Software Foundation, Inc.

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
#include "diagnostic.h"
#include "tm.h" /* For SWITCH_TAKES_ARG and WORD_SWITCH_TAKES_ARG.  */

/* Perform a binary search to find which option the command-line INPUT
   matches.  Returns its index in the option array, and
   OPT_SPECIAL_unknown on failure.

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
   issue, 'optc-gen.awk' makes "-gen-decls" point, via the back_chain member,
   to "-g" so that failed searches that end between "-gen-decls" and
   the lexicographically subsequent switch know to go back and see if
   "-g" causes a match (which it does in this example).

   This search is done in such a way that the longest match for the
   front end in question wins.  If there is no match for the current
   front end, the longest match for a different front end is returned
   (or N_OPTS if none) and the caller emits an error message.  */
size_t
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
      comp = strncmp (input, cl_options[md].opt_text + 1, opt_len);

      if (comp < 0)
	mx = md;
      else
	mn = md;
    }

  /* This is the switch that is the best match but for a different
     front end, or OPT_SPECIAL_unknown if there is no match at all.  */
  match_wrong_lang = OPT_SPECIAL_unknown;

  /* Backtrace the chain of possible matches, returning the longest
     one, if any, that fits best.  With current GCC switches, this
     loop executes at most twice.  */
  do
    {
      const struct cl_option *opt = &cl_options[mn];

      /* Is the input either an exact match or a prefix that takes a
	 joined argument?  */
      if (!strncmp (input, opt->opt_text + 1, opt->opt_len)
	  && (input[opt->opt_len] == '\0' || (opt->flags & CL_JOINED)))
	{
	  /* If language is OK, return it.  */
	  if (opt->flags & lang_mask)
	    return mn;

	  /* If we haven't remembered a prior match, remember this
	     one.  Any prior match is necessarily better.  */
	  if (match_wrong_lang == OPT_SPECIAL_unknown)
	    match_wrong_lang = mn;
	}

      /* Try the next possibility.  This is cl_options_count if there
	 are no more.  */
      mn = opt->back_chain;
    }
  while (mn != cl_options_count);

  /* Return the best wrong match, or OPT_SPECIAL_unknown if none.  */
  return match_wrong_lang;
}

/* If ARG is a non-negative integer made up solely of digits, return its
   value, otherwise return -1.  */

int
integral_argument (const char *arg)
{
  const char *p = arg;

  while (*p && ISDIGIT (*p))
    p++;

  if (*p == '\0')
    return atoi (arg);

  return -1;
}

/* Decode the switch beginning at ARGV for the language indicated by
   LANG_MASK (including CL_COMMON and CL_TARGET if applicable), into
   the structure *DECODED.  Returns the number of switches
   consumed.  */

static unsigned int
decode_cmdline_option (const char **argv, unsigned int lang_mask,
		       struct cl_decoded_option *decoded)
{
  size_t opt_index;
  const char *opt, *arg = 0;
  char *dup = 0;
  int value = 1;
  unsigned int result = 1, i;
  size_t total_len;
  char *p;
  const struct cl_option *option;
  int errors = 0;

  opt = argv[0];

  opt_index = find_opt (opt + 1, lang_mask);
  if (opt_index == OPT_SPECIAL_unknown
      && (opt[1] == 'W' || opt[1] == 'f' || opt[1] == 'm')
      && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-')
    {
      /* Drop the "no-" from negative switches.  */
      size_t len = strlen (opt) - 3;

      dup = XNEWVEC (char, len + 1);
      dup[0] = '-';
      dup[1] = opt[1];
      memcpy (dup + 2, opt + 5, len - 2 + 1);
      opt = dup;
      value = 0;
      opt_index = find_opt (opt + 1, lang_mask);
    }

  if (opt_index == OPT_SPECIAL_unknown)
    {
      arg = argv[0];
      goto done;
    }

  option = &cl_options[opt_index];

  /* Reject negative form of switches that don't take negatives as
     unrecognized.  */
  if (!value && (option->flags & CL_REJECT_NEGATIVE))
    {
      opt_index = OPT_SPECIAL_unknown;
      arg = argv[0];
      goto done;
    }

  /* Check to see if the option is disabled for this configuration.  */
  if (option->flags & CL_DISABLED)
    errors |= CL_ERR_DISABLED;

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
	      if (arg == NULL)
		result = 1;
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
      if (arg == NULL)
	result = 1;
    }

  /* Check if this is a switch for a different front end.  */
  if (!(option->flags & lang_mask))
    errors |= CL_ERR_WRONG_LANG;
  else if ((option->flags & CL_TARGET)
	   && (option->flags & (CL_LANG_ALL | CL_DRIVER))
	   && !(option->flags & (lang_mask & ~CL_COMMON & ~CL_TARGET)))
    /* Complain for target flag language mismatches if any languages
       are specified.  */
      errors |= CL_ERR_WRONG_LANG;

  if (arg == NULL && (option->flags & (CL_JOINED | CL_SEPARATE)))
    errors |= CL_ERR_MISSING_ARG;

  /* If the switch takes an integer, convert it.  */
  if (arg && (option->flags & CL_UINTEGER))
    {
      value = integral_argument (arg);
      if (value == -1)
	errors |= CL_ERR_UINT_ARG;
    }

 done:
  if (dup)
    free (dup);
  decoded->opt_index = opt_index;
  decoded->arg = arg;
  decoded->value = value;
  decoded->errors = errors;

  if (opt_index == OPT_SPECIAL_unknown)
    {
      /* Skip the correct number of arguments for options handled
	 through specs.  */
      const char *popt = argv[0] + 1;
      int c = *popt;

      gcc_assert (result == 1);
      if (SWITCH_TAKES_ARG (c) > (popt[1] != 0))
	result += SWITCH_TAKES_ARG (c) - (popt[1] != 0);
      else if (WORD_SWITCH_TAKES_ARG (popt))
	result += WORD_SWITCH_TAKES_ARG (popt);
      if (result > 1)
	for (i = 1; i < result; i++)
	  if (argv[i] == NULL)
	    {
	      result = i;
	      break;
	    }
    }

  gcc_assert (result >= 1 && result <= ARRAY_SIZE (decoded->canonical_option));
  decoded->canonical_option_num_elements = result;
  total_len = 0;
  for (i = 0; i < ARRAY_SIZE (decoded->canonical_option); i++)
    {
      if (i < result)
	{
	  decoded->canonical_option[i] = argv[i];
	  total_len += strlen (argv[i]) + 1;
	}
      else
	decoded->canonical_option[i] = NULL;
    }
  decoded->orig_option_with_args_text = p = XNEWVEC (char, total_len);
  for (i = 0; i < result; i++)
    {
      size_t len = strlen (argv[i]);

      memcpy (p, argv[i], len);
      p += len;
      if (i == result - 1)
	*p++ = 0;
      else
	*p++ = ' ';
    }

  return result;
}

/* Decode command-line options (ARGC and ARGV being the arguments of
   main) into an array, setting *DECODED_OPTIONS to a pointer to that
   array and *DECODED_OPTIONS_COUNT to the number of entries in the
   array.  The first entry in the array is always one for the program
   name (OPT_SPECIAL_program_name).  LANG_MASK indicates the language
   flags applicable for decoding (including CL_COMMON and CL_TARGET if
   those options should be considered applicable).  Do not produce any
   diagnostics or set state outside of these variables.  */

void
decode_cmdline_options_to_array (unsigned int argc, const char **argv, 
				 unsigned int lang_mask,
				 struct cl_decoded_option **decoded_options,
				 unsigned int *decoded_options_count)
{
  unsigned int n, i;
  struct cl_decoded_option *opt_array;
  unsigned int num_decoded_options;

  opt_array = XNEWVEC (struct cl_decoded_option, argc);

  opt_array[0].opt_index = OPT_SPECIAL_program_name;
  opt_array[0].arg = argv[0];
  opt_array[0].orig_option_with_args_text = argv[0];
  opt_array[0].canonical_option_num_elements = 1;
  opt_array[0].canonical_option[0] = argv[0];
  opt_array[0].canonical_option[1] = NULL;
  opt_array[0].canonical_option[2] = NULL;
  opt_array[0].canonical_option[3] = NULL;
  opt_array[0].value = 1;
  opt_array[0].errors = 0;
  num_decoded_options = 1;

  for (i = 1; i < argc; i += n)
    {
      const char *opt = argv[i];

      /* Interpret "-" or a non-switch as a file name.  */
      if (opt[0] != '-' || opt[1] == '\0')
	{
	  opt_array[num_decoded_options].opt_index = OPT_SPECIAL_input_file;
	  opt_array[num_decoded_options].arg = opt;
	  opt_array[num_decoded_options].orig_option_with_args_text = opt;
	  opt_array[num_decoded_options].canonical_option_num_elements = 1;
	  opt_array[num_decoded_options].canonical_option[0] = opt;
	  opt_array[num_decoded_options].canonical_option[1] = NULL;
	  opt_array[num_decoded_options].canonical_option[2] = NULL;
	  opt_array[num_decoded_options].canonical_option[3] = NULL;
	  opt_array[num_decoded_options].value = 1;
	  opt_array[num_decoded_options].errors = 0;
	  num_decoded_options++;
	  n = 1;
	  continue;
	}

      n = decode_cmdline_option (argv + i, lang_mask,
				 &opt_array[num_decoded_options]);
      num_decoded_options++;
    }

  opt_array = XRESIZEVEC (struct cl_decoded_option, opt_array,
			  num_decoded_options);
  *decoded_options = opt_array;
  *decoded_options_count = num_decoded_options;
}

/* Return true if NEXT_OPT_IDX cancels OPT_IDX.  Return false if the
   next one is the same as ORIG_NEXT_OPT_IDX.  */

static bool
cancel_option (int opt_idx, int next_opt_idx, int orig_next_opt_idx)
{
  /* An option can be canceled by the same option or an option with
     Negative.  */
  if (cl_options [next_opt_idx].neg_index == opt_idx)
    return true;

  if (cl_options [next_opt_idx].neg_index != orig_next_opt_idx)
    return cancel_option (opt_idx, cl_options [next_opt_idx].neg_index,
			  orig_next_opt_idx);

  return false;
}

/* Filter out options canceled by the ones after them.  */

void
prune_options (int *argcp, char ***argvp)
{
  int argc = *argcp;
  int *options = XNEWVEC (int, argc);
  /* We will only return this replacement argv if we remove at least
     one argument, so it does not need to be size (argc + 1) to
     make room for the terminating NULL because we will always have
     freed up at least one slot when we end up using it at all.  */
  char **argv = XNEWVEC (char *, argc);
  int i, arg_count, need_prune = 0;
  const struct cl_option *option;
  size_t opt_index;

  /* Scan all arguments.  */
  for (i = 1; i < argc; i++)
    {
      int value = 1;
      const char *opt = (*argvp) [i];

      opt_index = find_opt (opt + 1, -1);
      if (opt_index == OPT_SPECIAL_unknown
	  && (opt[1] == 'W' || opt[1] == 'f' || opt[1] == 'm')
	  && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-')
	{
	  char *dup;

	  /* Drop the "no-" from negative switches.  */
	  size_t len = strlen (opt) - 3;

	  dup = XNEWVEC (char, len + 1);
	  dup[0] = '-';
	  dup[1] = opt[1];
	  memcpy (dup + 2, opt + 5, len - 2 + 1);
	  opt = dup;
	  value = 0;
	  opt_index = find_opt (opt + 1, -1);
	  free (dup);
	}

      if (opt_index == OPT_SPECIAL_unknown)
	{
cont:
	  options [i] = 0;
	  continue;
	}

      option = &cl_options[opt_index];
      if (option->neg_index < 0)
	goto cont;

      /* Skip joined switches.  */
      if ((option->flags & CL_JOINED))
	goto cont;

      /* Reject negative form of switches that don't take negatives as
	 unrecognized.  */
      if (!value && (option->flags & CL_REJECT_NEGATIVE))
	goto cont;

      options [i] = (int) opt_index;
      need_prune |= options [i];
    }

  if (!need_prune)
    goto done;

  /* Remove arguments which are negated by others after them.  */
  argv [0] = (*argvp) [0];
  arg_count = 1;
  for (i = 1; i < argc; i++)
    {
      int j, opt_idx;

      opt_idx = options [i];
      if (opt_idx)
	{
	  int next_opt_idx;
	  for (j = i + 1; j < argc; j++)
	    {
	      next_opt_idx = options [j];
	      if (next_opt_idx
		  && cancel_option (opt_idx, next_opt_idx,
				    next_opt_idx))
		break;
	    }
	}
      else
	goto keep;

      if (j == argc)
	{
keep:
	  argv [arg_count] = (*argvp) [i];
	  arg_count++;
	}
    }

  if (arg_count != argc)
    {
      *argcp = arg_count;
      *argvp = argv;
      /* Add NULL-termination.  Guaranteed not to overflow because
	 arg_count here can only be less than argc.  */
      argv[arg_count] = 0;
    }
  else
    {
done:
      free (argv);
    }

  free (options);
}

/* Handle option DECODED for the language indicated by LANG_MASK,
   using the handlers in HANDLERS.  KIND is the diagnostic_t if this
   is a diagnostics option, DK_UNSPECIFIED otherwise.  Returns false
   if the switch was invalid.  */

bool
handle_option (const struct cl_decoded_option *decoded,
	       unsigned int lang_mask, int kind,
	       const struct cl_option_handlers *handlers)
{
  size_t opt_index = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;
  const struct cl_option *option = &cl_options[opt_index];
  size_t i;

  if (option->flag_var)
    set_option (opt_index, value, arg, kind);

  for (i = 0; i < handlers->num_handlers; i++)
    if (option->flags & handlers->handlers[i].mask)
      {
	if (!handlers->handlers[i].handler (decoded,
					    lang_mask, kind, handlers))
	  return false;
	else
	  handlers->post_handling_callback (decoded,
					    handlers->handlers[i].mask);
      }
  
  return true;
}

/* Like handle_option, but OPT_INDEX, ARG and VALUE describe the
   option instead of DECODED.  This is used for callbacks when one
   option implies another instead of an option being decoded from the
   command line.  */

bool
handle_generated_option (size_t opt_index, const char *arg, int value,
			 unsigned int lang_mask, int kind,
			 const struct cl_option_handlers *handlers)
{
  const struct cl_option *option = &cl_options[opt_index];
  struct cl_decoded_option decoded;

  decoded.opt_index = opt_index;
  decoded.arg = arg;
  decoded.canonical_option[2] = NULL;
  decoded.canonical_option[3] = NULL;
  decoded.value = value;
  decoded.errors = 0;

  if (arg)
    {
      if (option->flags & CL_SEPARATE)
	{
	  decoded.orig_option_with_args_text = concat (option->opt_text, " ",
						       arg, NULL);
	  decoded.canonical_option[0] = option->opt_text;
	  decoded.canonical_option[1] = arg;
	  decoded.canonical_option_num_elements = 2;
	}
      else
	{
	  gcc_assert (option->flags & CL_JOINED);
	  decoded.orig_option_with_args_text = concat (option->opt_text, arg,
						       NULL);
	  decoded.canonical_option[0] = decoded.orig_option_with_args_text;
	  decoded.canonical_option[1] = NULL;
	  decoded.canonical_option_num_elements = 1;
	}
    }
  else
    {
      decoded.orig_option_with_args_text = option->opt_text;
      decoded.canonical_option[0] = option->opt_text;
      decoded.canonical_option[1] = NULL;
      decoded.canonical_option_num_elements = 1;
    }

  return handle_option (&decoded, lang_mask, kind, handlers);
}

/* Handle the switch DECODED for the language indicated by LANG_MASK,
   using the handlers in *HANDLERS.  */

void
read_cmdline_option (struct cl_decoded_option *decoded,
		     unsigned int lang_mask,
		     const struct cl_option_handlers *handlers)
{
  const struct cl_option *option;
  const char *opt;

  if (decoded->opt_index == OPT_SPECIAL_unknown)
    {
      if (handlers->unknown_option_callback (decoded))
	error ("unrecognized command line option %qs", decoded->arg);
      return;
    }

  option = &cl_options[decoded->opt_index];
  opt = decoded->orig_option_with_args_text;

  if (decoded->errors & CL_ERR_DISABLED)
    {
      error ("command line option %qs"
	     " is not supported by this configuration", opt);
      return;
    }

  if (decoded->errors & CL_ERR_WRONG_LANG)
    {
      handlers->wrong_lang_callback (decoded, lang_mask);
      return;
    }

  if (decoded->errors & CL_ERR_MISSING_ARG)
    {
      if (option->missing_argument_error)
	error (option->missing_argument_error, opt);
      else
	error ("missing argument to %qs", opt);
      return;
    }

  if (decoded->errors & CL_ERR_UINT_ARG)
    {
      error ("argument to %qs should be a non-negative integer",
	     option->opt_text);
      return;
    }

  gcc_assert (!decoded->errors);

  if (!handle_option (decoded, lang_mask, DK_UNSPECIFIED, handlers))
    error ("unrecognized command line option %qs", opt);
}

/* Set any variable for option OPT_INDEX according to VALUE and ARG,
   diagnostic kind KIND.  */

void
set_option (int opt_index, int value, const char *arg, int kind)
{
  const struct cl_option *option = &cl_options[opt_index];

  if (!option->flag_var)
    return;

  switch (option->var_type)
    {
    case CLVC_BOOLEAN:
	*(int *) option->flag_var = value;
	break;

    case CLVC_EQUAL:
	*(int *) option->flag_var = (value
				     ? option->var_value
				     : !option->var_value);
	break;

    case CLVC_BIT_CLEAR:
    case CLVC_BIT_SET:
	if ((value != 0) == (option->var_type == CLVC_BIT_SET))
	  *(int *) option->flag_var |= option->var_value;
	else
	  *(int *) option->flag_var &= ~option->var_value;
	if (option->flag_var == &target_flags)
	  target_flags_explicit |= option->var_value;
	break;

    case CLVC_STRING:
	*(const char **) option->flag_var = arg;
	break;
    }

  if ((diagnostic_t) kind != DK_UNSPECIFIED)
    diagnostic_classify_diagnostic (global_dc, opt_index, (diagnostic_t) kind,
				    UNKNOWN_LOCATION);
}
