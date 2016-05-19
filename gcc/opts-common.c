/* Command line option handling.
   Copyright (C) 2006-2016 Free Software Foundation, Inc.

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
#include "spellcheck.h"

static void prune_options (struct cl_decoded_option **, unsigned int *);

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
find_opt (const char *input, unsigned int lang_mask)
{
  size_t mn, mn_orig, mx, md, opt_len;
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

  mn_orig = mn;

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

  if (match_wrong_lang == OPT_SPECIAL_unknown && input[0] == '-')
    {
      /* Long options, starting "--", may be abbreviated if the
	 abbreviation is unambiguous.  This only applies to options
	 not taking a joined argument, and abbreviations of "--option"
	 are permitted even if there is a variant "--option=".  */
      size_t mnc = mn_orig + 1;
      size_t cmp_len = strlen (input);
      while (mnc < cl_options_count
	     && strncmp (input, cl_options[mnc].opt_text + 1, cmp_len) == 0)
	{
	  /* Option matching this abbreviation.  OK if it is the first
	     match and that does not take a joined argument, or the
	     second match, taking a joined argument and with only '='
	     added to the first match; otherwise considered
	     ambiguous.  */
	  if (mnc == mn_orig + 1
	      && !(cl_options[mnc].flags & CL_JOINED))
	    match_wrong_lang = mnc;
	  else if (mnc == mn_orig + 2
		   && match_wrong_lang == mn_orig + 1
		   && (cl_options[mnc].flags & CL_JOINED)
		   && (cl_options[mnc].opt_len
		       == cl_options[mn_orig + 1].opt_len + 1)
		   && strncmp (cl_options[mnc].opt_text + 1,
			       cl_options[mn_orig + 1].opt_text + 1,
			       cl_options[mn_orig + 1].opt_len) == 0)
	    ; /* OK, as long as there are no more matches.  */
	  else
	    return OPT_SPECIAL_unknown;
	  mnc++;
	}
    }

  /* Return the best wrong match, or OPT_SPECIAL_unknown if none.  */
  return match_wrong_lang;
}

/* If ARG is a non-negative decimal or hexadecimal integer, return its
   value, otherwise return -1.  */

int
integral_argument (const char *arg)
{
  const char *p = arg;

  while (*p && ISDIGIT (*p))
    p++;

  if (*p == '\0')
    return atoi (arg);

  /* It wasn't a decimal number - try hexadecimal.  */
  if (arg[0] == '0' && (arg[1] == 'x' || arg[1] == 'X'))
    {
      p = arg + 2;
      while (*p && ISXDIGIT (*p))
	p++;

      if (p != arg + 2 && *p == '\0')
	return strtol (arg, NULL, 16);
    }

  return -1;
}

/* Return whether OPTION is OK for the language given by
   LANG_MASK.  */
static bool
option_ok_for_language (const struct cl_option *option,
			unsigned int lang_mask)
{
  if (!(option->flags & lang_mask))
    return false;
  else if ((option->flags & CL_TARGET)
	   && (option->flags & (CL_LANG_ALL | CL_DRIVER))
	   && !(option->flags & (lang_mask & ~CL_COMMON & ~CL_TARGET)))
    /* Complain for target flag language mismatches if any languages
       are specified.  */
    return false;
  return true;
}

/* Return whether ENUM_ARG is OK for the language given by
   LANG_MASK.  */

static bool
enum_arg_ok_for_language (const struct cl_enum_arg *enum_arg,
			  unsigned int lang_mask)
{
  return (lang_mask & CL_DRIVER) || !(enum_arg->flags & CL_ENUM_DRIVER_ONLY);
}

/* Look up ARG in ENUM_ARGS for language LANG_MASK, returning true and
   storing the value in *VALUE if found, and returning false without
   modifying *VALUE if not found.  */

static bool
enum_arg_to_value (const struct cl_enum_arg *enum_args,
		   const char *arg, int *value, unsigned int lang_mask)
{
  unsigned int i;

  for (i = 0; enum_args[i].arg != NULL; i++)
    if (strcmp (arg, enum_args[i].arg) == 0
	&& enum_arg_ok_for_language (&enum_args[i], lang_mask))
      {
	*value = enum_args[i].value;
	return true;
      }

  return false;
}

/* Look up ARG in the enum used by option OPT_INDEX for language
   LANG_MASK, returning true and storing the value in *VALUE if found,
   and returning false without modifying *VALUE if not found.  */

bool
opt_enum_arg_to_value (size_t opt_index, const char *arg, int *value,
		       unsigned int lang_mask)
{
  const struct cl_option *option = &cl_options[opt_index];

  gcc_assert (option->var_type == CLVC_ENUM);

  return enum_arg_to_value (cl_enums[option->var_enum].values, arg,
			    value, lang_mask);
}

/* Look of VALUE in ENUM_ARGS for language LANG_MASK and store the
   corresponding string in *ARGP, returning true if the found string
   was marked as canonical, false otherwise.  If VALUE is not found
   (which may be the case for uninitialized values if the relevant
   option has not been passed), set *ARGP to NULL and return
   false.  */

bool
enum_value_to_arg (const struct cl_enum_arg *enum_args,
		   const char **argp, int value, unsigned int lang_mask)
{
  unsigned int i;

  for (i = 0; enum_args[i].arg != NULL; i++)
    if (enum_args[i].value == value
	&& (enum_args[i].flags & CL_ENUM_CANONICAL)
	&& enum_arg_ok_for_language (&enum_args[i], lang_mask))
      {
	*argp = enum_args[i].arg;
	return true;
      }

  for (i = 0; enum_args[i].arg != NULL; i++)
    if (enum_args[i].value == value
	&& enum_arg_ok_for_language (&enum_args[i], lang_mask))
      {
	*argp = enum_args[i].arg;
	return false;
      }

  *argp = NULL;
  return false;
}

/* Fill in the canonical option part of *DECODED with an option
   described by OPT_INDEX, ARG and VALUE.  */

static void
generate_canonical_option (size_t opt_index, const char *arg, int value,
			   struct cl_decoded_option *decoded)
{
  const struct cl_option *option = &cl_options[opt_index];
  const char *opt_text = option->opt_text;

  if (value == 0
      && !option->cl_reject_negative
      && (opt_text[1] == 'W' || opt_text[1] == 'f' || opt_text[1] == 'm'))
    {
      char *t = XOBNEWVEC (&opts_obstack, char, option->opt_len + 5);
      t[0] = '-';
      t[1] = opt_text[1];
      t[2] = 'n';
      t[3] = 'o';
      t[4] = '-';
      memcpy (t + 5, opt_text + 2, option->opt_len);
      opt_text = t;
    }

  decoded->canonical_option[2] = NULL;
  decoded->canonical_option[3] = NULL;

  if (arg)
    {
      if ((option->flags & CL_SEPARATE)
	  && !option->cl_separate_alias)
	{
	  decoded->canonical_option[0] = opt_text;
	  decoded->canonical_option[1] = arg;
	  decoded->canonical_option_num_elements = 2;
	}
      else
	{
	  gcc_assert (option->flags & CL_JOINED);
	  decoded->canonical_option[0] = opts_concat (opt_text, arg, NULL);
	  decoded->canonical_option[1] = NULL;
	  decoded->canonical_option_num_elements = 1;
	}
    }
  else
    {
      decoded->canonical_option[0] = opt_text;
      decoded->canonical_option[1] = NULL;
      decoded->canonical_option_num_elements = 1;
    }
}

/* Structure describing mappings from options on the command line to
   options to look up with find_opt.  */
struct option_map
{
  /* Prefix of the option on the command line.  */
  const char *opt0;
  /* If two argv elements are considered to be merged into one option,
     prefix for the second element, otherwise NULL.  */
  const char *opt1;
  /* The new prefix to map to.  */
  const char *new_prefix;
  /* Whether at least one character is needed following opt1 or opt0
     for this mapping to be used.  (--optimize= is valid for -O, but
     --warn- is not valid for -W.)  */
  bool another_char_needed;
  /* Whether the original option is a negated form of the option
     resulting from this map.  */
  bool negated;
};
static const struct option_map option_map[] =
  {
    { "-Wno-", NULL, "-W", false, true },
    { "-fno-", NULL, "-f", false, true },
    { "-mno-", NULL, "-m", false, true },
    { "--debug=", NULL, "-g", false, false },
    { "--machine-", NULL, "-m", true, false },
    { "--machine-no-", NULL, "-m", false, true },
    { "--machine=", NULL, "-m", false, false },
    { "--machine=no-", NULL, "-m", false, true },
    { "--machine", "", "-m", false, false },
    { "--machine", "no-", "-m", false, true },
    { "--optimize=", NULL, "-O", false, false },
    { "--std=", NULL, "-std=", false, false },
    { "--std", "", "-std=", false, false },
    { "--warn-", NULL, "-W", true, false },
    { "--warn-no-", NULL, "-W", false, true },
    { "--", NULL, "-f", true, false },
    { "--no-", NULL, "-f", false, true }
  };

/* Helper function for gcc.c's driver::suggest_option, for populating the
   vec of suggestions for misspelled options.

   option_map above provides various prefixes for spelling command-line
   options, which decode_cmdline_option uses to map spellings of options
   to specific options.  We want to do the reverse: to find all the ways
   that a user could validly spell an option.

   Given valid OPT_TEXT (with a leading dash), add it and all of its valid
   variant spellings to CANDIDATES, each without a leading dash.

   For example, given "-Wabi-tag", the following are added to CANDIDATES:
     "Wabi-tag"
     "Wno-abi-tag"
     "-warn-abi-tag"
     "-warn-no-abi-tag".

   The added strings must be freed using free.  */

void
add_misspelling_candidates (auto_vec<char *> *candidates,
			    const char *opt_text)
{
  gcc_assert (candidates);
  gcc_assert (opt_text);
  candidates->safe_push (xstrdup (opt_text + 1));
  for (unsigned i = 0; i < ARRAY_SIZE (option_map); i++)
    {
      const char *opt0 = option_map[i].opt0;
      const char *new_prefix = option_map[i].new_prefix;
      size_t new_prefix_len = strlen (new_prefix);

      if (strncmp (opt_text, new_prefix, new_prefix_len) == 0)
	{
	  char *alternative = concat (opt0 + 1, opt_text + new_prefix_len,
				      NULL);
	  candidates->safe_push (alternative);
	}
    }
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
  const char *arg = 0;
  int value = 1;
  unsigned int result = 1, i, extra_args, separate_args = 0;
  int adjust_len = 0;
  size_t total_len;
  char *p;
  const struct cl_option *option;
  int errors = 0;
  const char *warn_message = NULL;
  bool separate_arg_flag;
  bool joined_arg_flag;
  bool have_separate_arg = false;

  extra_args = 0;

  opt_index = find_opt (argv[0] + 1, lang_mask);
  i = 0;
  while (opt_index == OPT_SPECIAL_unknown
	 && i < ARRAY_SIZE (option_map))
    {
      const char *opt0 = option_map[i].opt0;
      const char *opt1 = option_map[i].opt1;
      const char *new_prefix = option_map[i].new_prefix;
      bool another_char_needed = option_map[i].another_char_needed;
      size_t opt0_len = strlen (opt0);
      size_t opt1_len = (opt1 == NULL ? 0 : strlen (opt1));
      size_t optn_len = (opt1 == NULL ? opt0_len : opt1_len);
      size_t new_prefix_len = strlen (new_prefix);

      extra_args = (opt1 == NULL ? 0 : 1);
      value = !option_map[i].negated;

      if (strncmp (argv[0], opt0, opt0_len) == 0
	  && (opt1 == NULL
	      || (argv[1] != NULL && strncmp (argv[1], opt1, opt1_len) == 0))
	  && (!another_char_needed
	      || argv[extra_args][optn_len] != 0))
	{
	  size_t arglen = strlen (argv[extra_args]);
	  char *dup;

	  adjust_len = (int) optn_len - (int) new_prefix_len;
	  dup = XNEWVEC (char, arglen + 1 - adjust_len);
	  memcpy (dup, new_prefix, new_prefix_len);
	  memcpy (dup + new_prefix_len, argv[extra_args] + optn_len,
		  arglen - optn_len + 1);
	  opt_index = find_opt (dup + 1, lang_mask);
	  free (dup);
	}
      i++;
    }

  if (opt_index == OPT_SPECIAL_unknown)
    {
      arg = argv[0];
      extra_args = 0;
      value = 1;
      goto done;
    }

  option = &cl_options[opt_index];

  /* Reject negative form of switches that don't take negatives as
     unrecognized.  */
  if (!value && option->cl_reject_negative)
    {
      opt_index = OPT_SPECIAL_unknown;
      errors |= CL_ERR_NEGATIVE;
      arg = argv[0];
      goto done;
    }

  result = extra_args + 1;
  warn_message = option->warn_message;

  /* Check to see if the option is disabled for this configuration.  */
  if (option->cl_disabled)
    errors |= CL_ERR_DISABLED;

  /* Determine whether there may be a separate argument based on
     whether this option is being processed for the driver, and, if
     so, how many such arguments.  */
  separate_arg_flag = ((option->flags & CL_SEPARATE)
		       && !(option->cl_no_driver_arg
			    && (lang_mask & CL_DRIVER)));
  separate_args = (separate_arg_flag
		   ? option->cl_separate_nargs + 1
		   : 0);
  joined_arg_flag = (option->flags & CL_JOINED) != 0;

  /* Sort out any argument the switch takes.  */
  if (joined_arg_flag)
    {
      /* Have arg point to the original switch.  This is because
	 some code, such as disable_builtin_function, expects its
	 argument to be persistent until the program exits.  */
      arg = argv[extra_args] + cl_options[opt_index].opt_len + 1 + adjust_len;

      if (*arg == '\0' && !option->cl_missing_ok)
	{
	  if (separate_arg_flag)
	    {
	      arg = argv[extra_args + 1];
	      result = extra_args + 2;
	      if (arg == NULL)
		result = extra_args + 1;
	      else
		have_separate_arg = true;
	    }
	  else
	    /* Missing argument.  */
	    arg = NULL;
	}
    }
  else if (separate_arg_flag)
    {
      arg = argv[extra_args + 1];
      for (i = 0; i < separate_args; i++)
	if (argv[extra_args + 1 + i] == NULL)
	  {
	    errors |= CL_ERR_MISSING_ARG;
	    break;
	  }
      result = extra_args + 1 + i;
      if (arg != NULL)
	have_separate_arg = true;
    }

  if (arg == NULL && (separate_arg_flag || joined_arg_flag))
    errors |= CL_ERR_MISSING_ARG;

  /* Is this option an alias (or an ignored option, marked as an alias
     of OPT_SPECIAL_ignore)?  */
  if (option->alias_target != N_OPTS
      && (!option->cl_separate_alias || have_separate_arg))
    {
      size_t new_opt_index = option->alias_target;

      if (new_opt_index == OPT_SPECIAL_ignore)
	{
	  gcc_assert (option->alias_arg == NULL);
	  gcc_assert (option->neg_alias_arg == NULL);
	  opt_index = new_opt_index;
	  arg = NULL;
	  value = 1;
	}
      else
	{
	  const struct cl_option *new_option = &cl_options[new_opt_index];

	  /* The new option must not be an alias itself.  */
	  gcc_assert (new_option->alias_target == N_OPTS
		      || new_option->cl_separate_alias);

	  if (option->neg_alias_arg)
	    {
	      gcc_assert (option->alias_arg != NULL);
	      gcc_assert (arg == NULL);
	      gcc_assert (!option->cl_negative_alias);
	      if (value)
		arg = option->alias_arg;
	      else
		arg = option->neg_alias_arg;
	      value = 1;
	    }
	  else if (option->alias_arg)
	    {
	      gcc_assert (value == 1);
	      gcc_assert (arg == NULL);
	      gcc_assert (!option->cl_negative_alias);
	      arg = option->alias_arg;
	    }

	  if (option->cl_negative_alias)
	    value = !value;

	  opt_index = new_opt_index;
	  option = new_option;

	  if (value == 0)
	    gcc_assert (!option->cl_reject_negative);

	  /* Recompute what arguments are allowed.  */
	  separate_arg_flag = ((option->flags & CL_SEPARATE)
			       && !(option->cl_no_driver_arg
				    && (lang_mask & CL_DRIVER)));
	  joined_arg_flag = (option->flags & CL_JOINED) != 0;

	  if (separate_args > 1 || option->cl_separate_nargs)
	    gcc_assert (separate_args
			== (unsigned int) option->cl_separate_nargs + 1);

	  if (!(errors & CL_ERR_MISSING_ARG))
	    {
	      if (separate_arg_flag || joined_arg_flag)
		{
		  if (option->cl_missing_ok && arg == NULL)
		    arg = "";
		  gcc_assert (arg != NULL);
		}
	      else
		gcc_assert (arg == NULL);
	    }

	  /* Recheck for warnings and disabled options.  */
	  if (option->warn_message)
	    {
	      gcc_assert (warn_message == NULL);
	      warn_message = option->warn_message;
	    }
	  if (option->cl_disabled)
	    errors |= CL_ERR_DISABLED;
	}
    }

  /* Check if this is a switch for a different front end.  */
  if (!option_ok_for_language (option, lang_mask))
    errors |= CL_ERR_WRONG_LANG;

  /* Convert the argument to lowercase if appropriate.  */
  if (arg && option->cl_tolower)
    {
      size_t j;
      size_t len = strlen (arg);
      char *arg_lower = XOBNEWVEC (&opts_obstack, char, len + 1);

      for (j = 0; j < len; j++)
	arg_lower[j] = TOLOWER ((unsigned char) arg[j]);
      arg_lower[len] = 0;
      arg = arg_lower;
    }

  /* If the switch takes an integer, convert it.  */
  if (arg && option->cl_uinteger)
    {
      value = integral_argument (arg);
      if (value == -1)
	errors |= CL_ERR_UINT_ARG;
    }

  /* If the switch takes an enumerated argument, convert it.  */
  if (arg && (option->var_type == CLVC_ENUM))
    {
      const struct cl_enum *e = &cl_enums[option->var_enum];

      gcc_assert (value == 1);
      if (enum_arg_to_value (e->values, arg, &value, lang_mask))
	{
	  const char *carg = NULL;

	  if (enum_value_to_arg (e->values, &carg, value, lang_mask))
	    arg = carg;
	  gcc_assert (carg != NULL);
	}
      else
	errors |= CL_ERR_ENUM_ARG;
    }

 done:
  decoded->opt_index = opt_index;
  decoded->arg = arg;
  decoded->value = value;
  decoded->errors = errors;
  decoded->warn_message = warn_message;

  if (opt_index == OPT_SPECIAL_unknown)
    gcc_assert (result == 1);

  gcc_assert (result >= 1 && result <= ARRAY_SIZE (decoded->canonical_option));
  decoded->canonical_option_num_elements = result;
  total_len = 0;
  for (i = 0; i < ARRAY_SIZE (decoded->canonical_option); i++)
    {
      if (i < result)
	{
	  size_t len;
	  if (opt_index == OPT_SPECIAL_unknown)
	    decoded->canonical_option[i] = argv[i];
	  else
	    decoded->canonical_option[i] = NULL;
	  len = strlen (argv[i]);
	  /* If the argument is an empty string, we will print it as "" in
	     orig_option_with_args_text.  */
	  total_len += (len != 0 ? len : 2) + 1;
	}
      else
	decoded->canonical_option[i] = NULL;
    }
  if (opt_index != OPT_SPECIAL_unknown && opt_index != OPT_SPECIAL_ignore)
    {
      generate_canonical_option (opt_index, arg, value, decoded);
      if (separate_args > 1)
	{
	  for (i = 0; i < separate_args; i++)
	    {
	      if (argv[extra_args + 1 + i] == NULL)
		  break;
	      else
		decoded->canonical_option[1 + i] = argv[extra_args + 1 + i];
	    }
	  gcc_assert (result == 1 + i);
	  decoded->canonical_option_num_elements = result;
	}
    }
  decoded->orig_option_with_args_text
    = p = XOBNEWVEC (&opts_obstack, char, total_len);
  for (i = 0; i < result; i++)
    {
      size_t len = strlen (argv[i]);

      /* Print the empty string verbally.  */
      if (len == 0)
	{
	  *p++ = '"';
	  *p++ = '"';
	}
      else
	memcpy (p, argv[i], len);
      p += len;
      if (i == result - 1)
	*p++ = 0;
      else
	*p++ = ' ';
    }

  return result;
}

/* Obstack for option strings.  */

struct obstack opts_obstack;

/* Like libiberty concat, but allocate using opts_obstack.  */

char *
opts_concat (const char *first, ...)
{
  char *newstr, *end;
  size_t length = 0;
  const char *arg;
  va_list ap;

  /* First compute the size of the result and get sufficient memory.  */
  va_start (ap, first);
  for (arg = first; arg; arg = va_arg (ap, const char *))
    length += strlen (arg);
  newstr = XOBNEWVEC (&opts_obstack, char, length + 1);
  va_end (ap);

  /* Now copy the individual pieces to the result string. */
  va_start (ap, first);
  for (arg = first, end = newstr; arg; arg = va_arg (ap, const char *))
    {
      length = strlen (arg);
      memcpy (end, arg, length);
      end += length;
    }
  *end = '\0';
  va_end (ap);
  return newstr;
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
  opt_array[0].warn_message = NULL;
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
	  generate_option_input_file (opt, &opt_array[num_decoded_options]);
	  num_decoded_options++;
	  n = 1;
	  continue;
	}

      n = decode_cmdline_option (argv + i, lang_mask,
				 &opt_array[num_decoded_options]);
      num_decoded_options++;
    }

  *decoded_options = opt_array;
  *decoded_options_count = num_decoded_options;
  prune_options (decoded_options, decoded_options_count);
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

static void
prune_options (struct cl_decoded_option **decoded_options,
	       unsigned int *decoded_options_count)
{
  unsigned int old_decoded_options_count = *decoded_options_count;
  struct cl_decoded_option *old_decoded_options = *decoded_options;
  unsigned int new_decoded_options_count;
  struct cl_decoded_option *new_decoded_options
    = XNEWVEC (struct cl_decoded_option, old_decoded_options_count);
  unsigned int i;
  const struct cl_option *option;
  unsigned int fdiagnostics_color_idx = 0;

  /* Remove arguments which are negated by others after them.  */
  new_decoded_options_count = 0;
  for (i = 0; i < old_decoded_options_count; i++)
    {
      unsigned int j, opt_idx, next_opt_idx;

      if (old_decoded_options[i].errors & ~CL_ERR_WRONG_LANG)
	goto keep;

      opt_idx = old_decoded_options[i].opt_index;
      switch (opt_idx)
	{
	case OPT_SPECIAL_unknown:
	case OPT_SPECIAL_ignore:
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
	  goto keep;

	/* Do not save OPT_fdiagnostics_color_, just remember the last one.  */
	case OPT_fdiagnostics_color_:
	  fdiagnostics_color_idx = i;
	  continue;

	default:
	  gcc_assert (opt_idx < cl_options_count);
	  option = &cl_options[opt_idx];
	  if (option->neg_index < 0)
	    goto keep;

	  /* Skip joined switches.  */
	  if ((option->flags & CL_JOINED))
	    goto keep;

	  for (j = i + 1; j < old_decoded_options_count; j++)
	    {
	      if (old_decoded_options[j].errors & ~CL_ERR_WRONG_LANG)
		continue;
	      next_opt_idx = old_decoded_options[j].opt_index;
	      if (next_opt_idx >= cl_options_count)
		continue;
	      if (cl_options[next_opt_idx].neg_index < 0)
		continue;
	      if ((cl_options[next_opt_idx].flags & CL_JOINED))
		  continue;
	      if (cancel_option (opt_idx, next_opt_idx, next_opt_idx))
		break;
	    }
	  if (j == old_decoded_options_count)
	    {
keep:
	      new_decoded_options[new_decoded_options_count]
		= old_decoded_options[i];
	      new_decoded_options_count++;
	    }
	  break;
	}
    }

  if (fdiagnostics_color_idx >= 1)
    {
      /* We put the last -fdiagnostics-color= at the first position
	 after argv[0] so it can take effect immediately.  */
      memmove (new_decoded_options + 2, new_decoded_options + 1,
	       sizeof (struct cl_decoded_option) 
	       * (new_decoded_options_count - 1));
      new_decoded_options[1] = old_decoded_options[fdiagnostics_color_idx];
      new_decoded_options_count++;
    }

  free (old_decoded_options);
  new_decoded_options = XRESIZEVEC (struct cl_decoded_option,
				    new_decoded_options,
				    new_decoded_options_count);
  *decoded_options = new_decoded_options;
  *decoded_options_count = new_decoded_options_count;
}

/* Handle option DECODED for the language indicated by LANG_MASK,
   using the handlers in HANDLERS and setting fields in OPTS and
   OPTS_SET.  KIND is the diagnostic_t if this is a diagnostics
   option, DK_UNSPECIFIED otherwise, and LOC is the location of the
   option for options from the source file, UNKNOWN_LOCATION
   otherwise.  GENERATED_P is true for an option generated as part of
   processing another option or otherwise generated internally, false
   for one explicitly passed by the user.  Returns false if the switch
   was invalid.  DC is the diagnostic context for options affecting
   diagnostics state, or NULL.  */

static bool
handle_option (struct gcc_options *opts,
	       struct gcc_options *opts_set,
	       const struct cl_decoded_option *decoded,
	       unsigned int lang_mask, int kind, location_t loc,
	       const struct cl_option_handlers *handlers,
	       bool generated_p, diagnostic_context *dc)
{
  size_t opt_index = decoded->opt_index;
  const char *arg = decoded->arg;
  int value = decoded->value;
  const struct cl_option *option = &cl_options[opt_index];
  void *flag_var = option_flag_var (opt_index, opts);
  size_t i;

  if (flag_var)
    set_option (opts, (generated_p ? NULL : opts_set),
		opt_index, value, arg, kind, loc, dc);

  for (i = 0; i < handlers->num_handlers; i++)
    if (option->flags & handlers->handlers[i].mask)
      {
	if (!handlers->handlers[i].handler (opts, opts_set, decoded,
					    lang_mask, kind, loc,
					    handlers, dc))
	  return false;
      }
  
  return true;
}

/* Like handle_option, but OPT_INDEX, ARG and VALUE describe the
   option instead of DECODED.  This is used for callbacks when one
   option implies another instead of an option being decoded from the
   command line.  */

bool
handle_generated_option (struct gcc_options *opts,
			 struct gcc_options *opts_set,
			 size_t opt_index, const char *arg, int value,
			 unsigned int lang_mask, int kind, location_t loc,
			 const struct cl_option_handlers *handlers,
			 diagnostic_context *dc)
{
  struct cl_decoded_option decoded;

  generate_option (opt_index, arg, value, lang_mask, &decoded);
  return handle_option (opts, opts_set, &decoded, lang_mask, kind, loc,
			handlers, true, dc);
}

/* Fill in *DECODED with an option described by OPT_INDEX, ARG and
   VALUE for a front end using LANG_MASK.  This is used when the
   compiler generates options internally.  */

void
generate_option (size_t opt_index, const char *arg, int value,
		 unsigned int lang_mask, struct cl_decoded_option *decoded)
{
  const struct cl_option *option = &cl_options[opt_index];

  decoded->opt_index = opt_index;
  decoded->warn_message = NULL;
  decoded->arg = arg;
  decoded->value = value;
  decoded->errors = (option_ok_for_language (option, lang_mask)
		     ? 0
		     : CL_ERR_WRONG_LANG);

  generate_canonical_option (opt_index, arg, value, decoded);
  switch (decoded->canonical_option_num_elements)
    {
    case 1:
      decoded->orig_option_with_args_text = decoded->canonical_option[0];
      break;

    case 2:
      decoded->orig_option_with_args_text
	= opts_concat (decoded->canonical_option[0], " ",
		       decoded->canonical_option[1], NULL);
      break;

    default:
      gcc_unreachable ();
    }
}

/* Fill in *DECODED with an option for input file FILE.  */

void
generate_option_input_file (const char *file,
			    struct cl_decoded_option *decoded)
{
  decoded->opt_index = OPT_SPECIAL_input_file;
  decoded->warn_message = NULL;
  decoded->arg = file;
  decoded->orig_option_with_args_text = file;
  decoded->canonical_option_num_elements = 1;
  decoded->canonical_option[0] = file;
  decoded->canonical_option[1] = NULL;
  decoded->canonical_option[2] = NULL;
  decoded->canonical_option[3] = NULL;
  decoded->value = 1;
  decoded->errors = 0;
}

/* Perform diagnostics for read_cmdline_option and control_warning_option
   functions.  Returns true if an error has been diagnosed.
   LOC and LANG_MASK arguments like in read_cmdline_option.
   OPTION is the option to report diagnostics for, OPT the name
   of the option as text, ARG the argument of the option (for joined
   options), ERRORS is bitmask of CL_ERR_* values.  */

static bool
cmdline_handle_error (location_t loc, const struct cl_option *option,
		      const char *opt, const char *arg, int errors,
		      unsigned int lang_mask)
{
  if (errors & CL_ERR_DISABLED)
    {
      error_at (loc, "command line option %qs"
		     " is not supported by this configuration", opt);
      return true;
    }

  if (errors & CL_ERR_MISSING_ARG)
    {
      if (option->missing_argument_error)
	error_at (loc, option->missing_argument_error, opt);
      else
	error_at (loc, "missing argument to %qs", opt);
      return true;
    }

  if (errors & CL_ERR_UINT_ARG)
    {
      error_at (loc, "argument to %qs should be a non-negative integer",
		option->opt_text);
      return true;
    }

  if (errors & CL_ERR_ENUM_ARG)
    {
      const struct cl_enum *e = &cl_enums[option->var_enum];
      unsigned int i;
      size_t len;
      char *s, *p;

      if (e->unknown_error)
	error_at (loc, e->unknown_error, arg);
      else
	error_at (loc, "unrecognized argument in option %qs", opt);

      len = 0;
      for (i = 0; e->values[i].arg != NULL; i++)
	len += strlen (e->values[i].arg) + 1;

      auto_vec <const char *> candidates;
      s = XALLOCAVEC (char, len);
      p = s;
      for (i = 0; e->values[i].arg != NULL; i++)
	{
	  if (!enum_arg_ok_for_language (&e->values[i], lang_mask))
	    continue;
	  size_t arglen = strlen (e->values[i].arg);
	  memcpy (p, e->values[i].arg, arglen);
	  p[arglen] = ' ';
	  p += arglen + 1;
	  candidates.safe_push (e->values[i].arg);
	}
      p[-1] = 0;
      const char *hint = find_closest_string (arg, &candidates);
      if (hint)
	inform (loc, "valid arguments to %qs are: %s; did you mean %qs?",
		option->opt_text, s, hint);
      else
	inform (loc, "valid arguments to %qs are: %s", option->opt_text, s);

      return true;
    }

  return false;
}

/* Handle the switch DECODED (location LOC) for the language indicated
   by LANG_MASK, using the handlers in *HANDLERS and setting fields in
   OPTS and OPTS_SET and using diagnostic context DC (if not NULL) for
   diagnostic options.  */

void
read_cmdline_option (struct gcc_options *opts,
		     struct gcc_options *opts_set,
		     struct cl_decoded_option *decoded,
		     location_t loc,
		     unsigned int lang_mask,
		     const struct cl_option_handlers *handlers,
		     diagnostic_context *dc)
{
  const struct cl_option *option;
  const char *opt = decoded->orig_option_with_args_text;

  if (decoded->warn_message)
    warning_at (loc, 0, decoded->warn_message, opt);

  if (decoded->opt_index == OPT_SPECIAL_unknown)
    {
      if (handlers->unknown_option_callback (decoded))
	error_at (loc, "unrecognized command line option %qs", decoded->arg);
      return;
    }

  if (decoded->opt_index == OPT_SPECIAL_ignore)
    return;

  option = &cl_options[decoded->opt_index];

  if (decoded->errors
      && cmdline_handle_error (loc, option, opt, decoded->arg,
			       decoded->errors, lang_mask))
    return;

  if (decoded->errors & CL_ERR_WRONG_LANG)
    {
      handlers->wrong_lang_callback (decoded, lang_mask);
      return;
    }

  gcc_assert (!decoded->errors);

  if (!handle_option (opts, opts_set, decoded, lang_mask, DK_UNSPECIFIED,
		      loc, handlers, false, dc))
    error_at (loc, "unrecognized command line option %qs", opt);
}

/* Set any field in OPTS, and OPTS_SET if not NULL, for option
   OPT_INDEX according to VALUE and ARG, diagnostic kind KIND,
   location LOC, using diagnostic context DC if not NULL for
   diagnostic classification.  */

void
set_option (struct gcc_options *opts, struct gcc_options *opts_set,
	    int opt_index, int value, const char *arg, int kind,
	    location_t loc, diagnostic_context *dc)
{
  const struct cl_option *option = &cl_options[opt_index];
  void *flag_var = option_flag_var (opt_index, opts);
  void *set_flag_var = NULL;

  if (!flag_var)
    return;

  if ((diagnostic_t) kind != DK_UNSPECIFIED && dc != NULL)
    diagnostic_classify_diagnostic (dc, opt_index, (diagnostic_t) kind, loc);

  if (opts_set != NULL)
    set_flag_var = option_flag_var (opt_index, opts_set);

  switch (option->var_type)
    {
    case CLVC_BOOLEAN:
	*(int *) flag_var = value;
	if (set_flag_var)
	  *(int *) set_flag_var = 1;
	break;

    case CLVC_EQUAL:
	if (option->cl_host_wide_int) 
	  *(HOST_WIDE_INT *) flag_var = (value
					 ? option->var_value
					 : !option->var_value);
	else
	  *(int *) flag_var = (value
			       ? option->var_value
			       : !option->var_value);
	if (set_flag_var)
	  *(int *) set_flag_var = 1;
	break;

    case CLVC_BIT_CLEAR:
    case CLVC_BIT_SET:
	if ((value != 0) == (option->var_type == CLVC_BIT_SET))
	  {
	    if (option->cl_host_wide_int) 
	      *(HOST_WIDE_INT *) flag_var |= option->var_value;
	    else 
	      *(int *) flag_var |= option->var_value;
	  }
	else
	  {
	    if (option->cl_host_wide_int) 
	      *(HOST_WIDE_INT *) flag_var &= ~option->var_value;
	    else 
	      *(int *) flag_var &= ~option->var_value;
	  }
	if (set_flag_var)
	  {
	    if (option->cl_host_wide_int) 
	      *(HOST_WIDE_INT *) set_flag_var |= option->var_value;
	    else
	      *(int *) set_flag_var |= option->var_value;
	  }
	break;

    case CLVC_STRING:
	*(const char **) flag_var = arg;
	if (set_flag_var)
	  *(const char **) set_flag_var = "";
	break;

    case CLVC_ENUM:
      {
	const struct cl_enum *e = &cl_enums[option->var_enum];

	e->set (flag_var, value);
	if (set_flag_var)
	  e->set (set_flag_var, 1);
      }
      break;

    case CLVC_DEFER:
	{
	  vec<cl_deferred_option> *v
	    = (vec<cl_deferred_option> *) *(void **) flag_var;
	  cl_deferred_option p = {opt_index, arg, value};
	  if (!v)
	    v = XCNEW (vec<cl_deferred_option>);
	  v->safe_push (p);
	  *(void **) flag_var = v;
	  if (set_flag_var)
	    *(void **) set_flag_var = v;
	}
	break;
    }
}

/* Return the address of the flag variable for option OPT_INDEX in
   options structure OPTS, or NULL if there is no flag variable.  */

void *
option_flag_var (int opt_index, struct gcc_options *opts)
{
  const struct cl_option *option = &cl_options[opt_index];

  if (option->flag_var_offset == (unsigned short) -1)
    return NULL;
  return (void *)(((char *) opts) + option->flag_var_offset);
}

/* Return 1 if option OPT_IDX is enabled in OPTS, 0 if it is disabled,
   or -1 if it isn't a simple on-off switch.  */

int
option_enabled (int opt_idx, void *opts)
{
  const struct cl_option *option = &(cl_options[opt_idx]);
  struct gcc_options *optsg = (struct gcc_options *) opts;
  void *flag_var = option_flag_var (opt_idx, optsg);

  if (flag_var)
    switch (option->var_type)
      {
      case CLVC_BOOLEAN:
	return *(int *) flag_var != 0;

      case CLVC_EQUAL:
	if (option->cl_host_wide_int) 
	  return *(HOST_WIDE_INT *) flag_var == option->var_value;
	else
	  return *(int *) flag_var == option->var_value;

      case CLVC_BIT_CLEAR:
	if (option->cl_host_wide_int) 
	  return (*(HOST_WIDE_INT *) flag_var & option->var_value) == 0;
	else
	  return (*(int *) flag_var & option->var_value) == 0;

      case CLVC_BIT_SET:
	if (option->cl_host_wide_int) 
	  return (*(HOST_WIDE_INT *) flag_var & option->var_value) != 0;
	else 
	  return (*(int *) flag_var & option->var_value) != 0;

      case CLVC_STRING:
      case CLVC_ENUM:
      case CLVC_DEFER:
	break;
      }
  return -1;
}

/* Fill STATE with the current state of option OPTION in OPTS.  Return
   true if there is some state to store.  */

bool
get_option_state (struct gcc_options *opts, int option,
		  struct cl_option_state *state)
{
  void *flag_var = option_flag_var (option, opts);

  if (flag_var == 0)
    return false;

  switch (cl_options[option].var_type)
    {
    case CLVC_BOOLEAN:
    case CLVC_EQUAL:
      state->data = flag_var;
      state->size = (cl_options[option].cl_host_wide_int
		     ? sizeof (HOST_WIDE_INT)
		     : sizeof (int));
      break;

    case CLVC_BIT_CLEAR:
    case CLVC_BIT_SET:
      state->ch = option_enabled (option, opts);
      state->data = &state->ch;
      state->size = 1;
      break;

    case CLVC_STRING:
      state->data = *(const char **) flag_var;
      if (state->data == 0)
	state->data = "";
      state->size = strlen ((const char *) state->data) + 1;
      break;

    case CLVC_ENUM:
      state->data = flag_var;
      state->size = cl_enums[cl_options[option].var_enum].var_size;
      break;

    case CLVC_DEFER:
      return false;
    }
  return true;
}

/* Set a warning option OPT_INDEX (language mask LANG_MASK, option
   handlers HANDLERS) to have diagnostic kind KIND for option
   structures OPTS and OPTS_SET and diagnostic context DC (possibly
   NULL), at location LOC (UNKNOWN_LOCATION for -Werror=).  ARG is the
   argument of the option for joined options, or NULL otherwise.  If IMPLY,
   the warning option in question is implied at this point.  This is
   used by -Werror= and #pragma GCC diagnostic.  */

void
control_warning_option (unsigned int opt_index, int kind, const char *arg,
			bool imply, location_t loc, unsigned int lang_mask,
			const struct cl_option_handlers *handlers,
			struct gcc_options *opts,
			struct gcc_options *opts_set,
			diagnostic_context *dc)
{
  if (cl_options[opt_index].alias_target != N_OPTS)
    {
      gcc_assert (!cl_options[opt_index].cl_separate_alias
		  && !cl_options[opt_index].cl_negative_alias);
      if (cl_options[opt_index].alias_arg)
	arg = cl_options[opt_index].alias_arg;
      opt_index = cl_options[opt_index].alias_target;
    }
  if (opt_index == OPT_SPECIAL_ignore)
    return;
  if (dc)
    diagnostic_classify_diagnostic (dc, opt_index, (diagnostic_t) kind, loc);
  if (imply)
    {
      const struct cl_option *option = &cl_options[opt_index];

      /* -Werror=foo implies -Wfoo.  */
      if (option->var_type == CLVC_BOOLEAN || option->var_type == CLVC_ENUM)
	{
	  int value = 1;

	  if (arg && *arg == '\0' && !option->cl_missing_ok)
	    arg = NULL;

	  if ((option->flags & CL_JOINED) && arg == NULL)
	    {
	      cmdline_handle_error (loc, option, option->opt_text, arg,
				    CL_ERR_MISSING_ARG, lang_mask);
	      return;
	    }

	  /* If the switch takes an integer, convert it.  */
	  if (arg && option->cl_uinteger)
	    {
	      value = integral_argument (arg);
	      if (value == -1)
		{
		  cmdline_handle_error (loc, option, option->opt_text, arg,
					CL_ERR_UINT_ARG, lang_mask);
		  return;
		}
	    }

	  /* If the switch takes an enumerated argument, convert it.  */
	  if (arg && option->var_type == CLVC_ENUM)
	    {
	      const struct cl_enum *e = &cl_enums[option->var_enum];

	      if (enum_arg_to_value (e->values, arg, &value, lang_mask))
		{
		  const char *carg = NULL;

		  if (enum_value_to_arg (e->values, &carg, value, lang_mask))
		    arg = carg;
		  gcc_assert (carg != NULL);
		}
	      else
		{
		  cmdline_handle_error (loc, option, option->opt_text, arg,
					CL_ERR_ENUM_ARG, lang_mask);
		  return;
		}
	    }

	  handle_generated_option (opts, opts_set,
				   opt_index, arg, value, lang_mask,
				   kind, loc, handlers, dc);
	}
    }
}
