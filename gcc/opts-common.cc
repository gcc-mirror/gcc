/* Command line option handling.
   Copyright (C) 2006-2024 Free Software Foundation, Inc.

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
#define INCLUDE_STRING
#include "config.h"
#include "system.h"
#include "intl.h"
#include "coretypes.h"
#include "opts.h"
#include "options.h"
#include "diagnostic.h"
#include "opts-diagnostic.h"
#include "spellcheck.h"
#include "opts-jobserver.h"

static void prune_options (struct cl_decoded_option **, unsigned int *);

/* An option that is undocumented, that takes a joined argument, and
   that doesn't fit any of the classes of uses (language/common,
   driver, target) is assumed to be a prefix used to catch
   e.g. negated options, and stop them from being further shortened to
   a prefix that could use the negated option as an argument.  For
   example, we want -gno-statement-frontiers to be taken as a negation
   of -gstatement-frontiers, but without catching the gno- prefix and
   signaling it's to be used for option remapping, it would end up
   backtracked to g with no-statemnet-frontiers as the debug level.  */

static bool
remapping_prefix_p (const struct cl_option *opt)
{
  return opt->flags & CL_UNDOCUMENTED
    && opt->flags & CL_JOINED
    && !(opt->flags & (CL_DRIVER | CL_TARGET | CL_COMMON | CL_LANG_ALL));
}

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

	  if (remapping_prefix_p (opt))
	    return OPT_SPECIAL_unknown;

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

/* If ARG is a non-negative decimal or hexadecimal integer representable
   in HOST_WIDE_INT return its value, otherwise return -1.  If ERR is not
   null set *ERR to zero on success or to EINVAL or to the value of errno
   otherwise.  */

HOST_WIDE_INT
integral_argument (const char *arg, int *err, bool byte_size_suffix)
{
  if (!err)
    err = &errno;

  if (!ISDIGIT (*arg))
    {
      *err = EINVAL;
      return -1;
    }

  *err = 0;
  errno = 0;

  char *end = NULL;
  unsigned HOST_WIDE_INT unit = 1;
  unsigned HOST_WIDE_INT value = strtoull (arg, &end, 10);

  /* If the value is too large to be represented use the maximum
     representable value that strtoull sets VALUE to (setting
     errno to ERANGE).  */

  if (end && *end)
    {
      if (!byte_size_suffix)
	{
	  errno = 0;
	  value = strtoull (arg, &end, 0);
	  if (*end)
	    {
	      if (errno)
		*err = errno;
	      else
		*err = EINVAL;
	      return -1;
	    }

	  return value;
	}

      /* Numeric option arguments are at most INT_MAX.  Make it
	 possible to specify a larger value by accepting common
	 suffixes.  */
      if (!strcmp (end, "kB"))
	unit = 1000;
      else if (!strcasecmp (end, "KiB") || !strcmp (end, "KB"))
	unit = 1024;
      else if (!strcmp (end, "MB"))
	unit = HOST_WIDE_INT_UC (1000) * 1000;
      else if (!strcasecmp (end, "MiB"))
	unit = HOST_WIDE_INT_UC (1024) * 1024;
      else if (!strcasecmp (end, "GB"))
	unit = HOST_WIDE_INT_UC (1000) * 1000 * 1000;
      else if (!strcasecmp (end, "GiB"))
	unit = HOST_WIDE_INT_UC (1024) * 1024 * 1024;
      else if (!strcasecmp (end, "TB"))
	unit = HOST_WIDE_INT_UC (1000) * 1000 * 1000 * 1000;
      else if (!strcasecmp (end, "TiB"))
	unit = HOST_WIDE_INT_UC (1024) * 1024 * 1024 * 1024;
      else if (!strcasecmp (end, "PB"))
	unit = HOST_WIDE_INT_UC (1000) * 1000 * 1000 * 1000 * 1000;
      else if (!strcasecmp (end, "PiB"))
	unit = HOST_WIDE_INT_UC (1024) * 1024 * 1024 * 1024 * 1024;
      else if (!strcasecmp (end, "EB"))
	unit = HOST_WIDE_INT_UC (1000) * 1000 * 1000 * 1000 * 1000
	  * 1000;
      else if (!strcasecmp (end, "EiB"))
	unit = HOST_WIDE_INT_UC (1024) * 1024 * 1024 * 1024 * 1024
	  * 1024;
      else
	{
	  /* This could mean an unknown suffix or a bad prefix, like
	     "+-1".  */
	  *err = EINVAL;
	  return -1;
	}
    }

  if (unit)
    {
      unsigned HOST_WIDE_INT prod = value * unit;
      value = prod < value ? HOST_WIDE_INT_M1U : prod;
    }

  return value;
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

/* Look up ARG in ENUM_ARGS for language LANG_MASK, returning the cl_enum_arg
   index and storing the value in *VALUE if found, and returning -1 without
   modifying *VALUE if not found.  */

static int
enum_arg_to_value (const struct cl_enum_arg *enum_args,
		   const char *arg, size_t len, HOST_WIDE_INT *value,
		   unsigned int lang_mask)
{
  unsigned int i;

  for (i = 0; enum_args[i].arg != NULL; i++)
    if ((len
	 ? (strncmp (arg, enum_args[i].arg, len) == 0
	    && enum_args[i].arg[len] == '\0')
	 : strcmp (arg, enum_args[i].arg) == 0)
	&& enum_arg_ok_for_language (&enum_args[i], lang_mask))
      {
	*value = enum_args[i].value;
	return i;
      }

  return -1;
}

/* Look up ARG in the enum used by option OPT_INDEX for language
   LANG_MASK, returning true and storing the value in *VALUE if found,
   and returning false without modifying *VALUE if not found.  */

bool
opt_enum_arg_to_value (size_t opt_index, const char *arg,
		       int *value, unsigned int lang_mask)
{
  const struct cl_option *option = &cl_options[opt_index];

  gcc_assert (option->var_type == CLVC_ENUM);

  HOST_WIDE_INT wideval;
  if (enum_arg_to_value (cl_enums[option->var_enum].values, arg, 0,
			 &wideval, lang_mask) >= 0)
    {
      *value = wideval;
      return true;
    }

  return false;
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
generate_canonical_option (size_t opt_index, const char *arg,
			   HOST_WIDE_INT value,
			   struct cl_decoded_option *decoded)
{
  const struct cl_option *option = &cl_options[opt_index];
  const char *opt_text = option->opt_text;

  if (value == 0
      && !option->cl_reject_negative
      && (opt_text[1] == 'W' || opt_text[1] == 'f'
	  || opt_text[1] == 'g' || opt_text[1] == 'm'))
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
    { "-gno-", NULL, "-g", false, true },
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

/* Given buffer P of size SZ, look for a prefix within OPTION_MAP;
   if found, return the prefix and write the new prefix to *OUT_NEW_PREFIX.
   Otherwise return nullptr.  */

const char *
get_option_prefix_remapping (const char *p, size_t sz,
			     const char **out_new_prefix)
{
  for (unsigned i = 0; i < ARRAY_SIZE (option_map); i++)
    {
      const char * const old_prefix = option_map[i].opt0;
      const size_t old_prefix_len = strlen (old_prefix);
      if (old_prefix_len <= sz
	  && !memcmp (p, old_prefix, old_prefix_len))
	{
	  *out_new_prefix = option_map[i].new_prefix;
	  return old_prefix;
	}
    }
  return nullptr;
}

/* Helper function for gcc.cc's driver::suggest_option, for populating the
   vec of suggestions for misspelled options.

   option_map above provides various prefixes for spelling command-line
   options, which decode_cmdline_option uses to map spellings of options
   to specific options.  We want to do the reverse: to find all the ways
   that a user could validly spell an option.

   Given valid OPT_TEXT (with a leading dash) for OPTION, add it and all
   of its valid variant spellings to CANDIDATES, each without a leading
   dash.

   For example, given "-Wabi-tag", the following are added to CANDIDATES:
     "Wabi-tag"
     "Wno-abi-tag"
     "-warn-abi-tag"
     "-warn-no-abi-tag".

   The added strings must be freed using free.  */

void
add_misspelling_candidates (auto_vec<char *> *candidates,
			    const struct cl_option *option,
			    const char *opt_text)
{
  gcc_assert (candidates);
  gcc_assert (option);
  gcc_assert (opt_text);
  if (remapping_prefix_p (option))
    return;
  candidates->safe_push (xstrdup (opt_text + 1));
  for (unsigned i = 0; i < ARRAY_SIZE (option_map); i++)
    {
      const char *opt0 = option_map[i].opt0;
      const char *opt1 = option_map[i].opt1;
      const char *new_prefix = option_map[i].new_prefix;
      size_t new_prefix_len = strlen (new_prefix);

      if (option->cl_reject_negative && option_map[i].negated)
	continue;

      if (strncmp (opt_text, new_prefix, new_prefix_len) == 0)
	{
	  char *alternative
	    = concat (opt0 + 1, opt1 ? " " : "", opt1 ? opt1 : "",
		      opt_text + new_prefix_len, NULL);
	  candidates->safe_push (alternative);
	}
    }

  /* For all params (e.g. --param=key=value),
     include also '--param key=value'.  */
  const char *prefix = "--param=";
  if (strstr (opt_text, prefix) == opt_text)
    {
      char *param = xstrdup (opt_text + 1);
      gcc_assert (param[6] == '=');
      param[6] = ' ';
      candidates->safe_push (param);
    }
}

/* Decode the switch beginning at ARGV for the language indicated by
   LANG_MASK (including CL_COMMON and CL_TARGET if applicable), into
   the structure *DECODED.  Returns the number of switches
   consumed.  */

static unsigned int
decode_cmdline_option (const char *const *argv, unsigned int lang_mask,
		       struct cl_decoded_option *decoded)
{
  size_t opt_index;
  const char *arg = 0;
  HOST_WIDE_INT value = 1, mask = 0;
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

  const char *opt_value = argv[0] + 1;
  opt_index = find_opt (opt_value, lang_mask);
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

  /* Clear the initial value for size options (it will be overwritten
     later based on the Init(value) specification in the opt file.  */
  if (option->var_type == CLVC_SIZE)
    value = 0;

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

      if (new_opt_index == OPT_SPECIAL_ignore
	  || new_opt_index == OPT_SPECIAL_warn_removed)
	{
	  gcc_assert (option->alias_arg == NULL);
	  gcc_assert (option->neg_alias_arg == NULL);
	  opt_index = new_opt_index;
	  arg = NULL;
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
  else if (strcmp (option->opt_text, "-Werror=") == 0
	   && strchr (opt_value, ',') == NULL)
    {
      /* Verify that -Werror argument is a valid warning
	 for a language.  */
      char *werror_arg = xstrdup (opt_value + 6);
      werror_arg[0] = 'W';

      size_t warning_index = find_opt (werror_arg, lang_mask);
      free (werror_arg);
      if (warning_index != OPT_SPECIAL_unknown)
	{
	  const struct cl_option *warning_option
	    = &cl_options[warning_index];
	  if (!option_ok_for_language (warning_option, lang_mask))
	    errors |= CL_ERR_WRONG_LANG;
	}
    }

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

  /* If the switch takes an integer argument, convert it.  */
  if (arg && (option->cl_uinteger || option->cl_host_wide_int))
    {
      int error = 0;
      value = *arg ? integral_argument (arg, &error, option->cl_byte_size) : 0;
      if (error)
	errors |= CL_ERR_UINT_ARG;

      /* Reject value out of a range.  */
      if (option->range_max != -1
	  && (value < option->range_min || value > option->range_max))
	errors |= CL_ERR_INT_RANGE_ARG;
    }

  /* If the switch takes an enumerated argument, convert it.  */
  if (arg && (option->var_type == CLVC_ENUM))
    {
      const struct cl_enum *e = &cl_enums[option->var_enum];

      gcc_assert (option->var_value != CLEV_NORMAL || value == 1);
      if (option->var_value != CLEV_NORMAL)
	{
	  const char *p = arg;
	  HOST_WIDE_INT sum_value = 0;
	  unsigned HOST_WIDE_INT used_sets = 0;
	  do
	    {
	      const char *q = strchr (p, ',');
	      HOST_WIDE_INT this_value = 0;
	      if (q && q == p)
		{
		  errors |= CL_ERR_ENUM_SET_ARG;
		  break;
		}
	      int idx = enum_arg_to_value (e->values, p, q ? q - p : 0,
					   &this_value, lang_mask);
	      if (idx < 0)
		{
		  errors |= CL_ERR_ENUM_SET_ARG;
		  break;
		}

	      HOST_WIDE_INT this_mask = 0;
	      if (option->var_value == CLEV_SET)
		{
		  unsigned set = e->values[idx].flags >> CL_ENUM_SET_SHIFT;
		  gcc_checking_assert (set >= 1
				       && set <= HOST_BITS_PER_WIDE_INT);
		  if ((used_sets & (HOST_WIDE_INT_1U << (set - 1))) != 0)
		    {
		      errors |= CL_ERR_ENUM_SET_ARG;
		      break;
		    }
		  used_sets |= HOST_WIDE_INT_1U << (set - 1);

		  for (int i = 0; e->values[i].arg != NULL; i++)
		    if (set == (e->values[i].flags >> CL_ENUM_SET_SHIFT))
		      this_mask |= e->values[i].value;
		}
	      else
		{
		  gcc_assert (option->var_value == CLEV_BITSET
			      && ((e->values[idx].flags >> CL_ENUM_SET_SHIFT)
				  == 0));
		  this_mask = this_value;
		}

	      sum_value |= this_value;
	      mask |= this_mask;
	      if (q == NULL)
		break;
	      p = q + 1;
	    }
	  while (1);
	  if (value == 1)
	    value = sum_value;
	  else
	    gcc_checking_assert (value == 0);
	}
      else if (enum_arg_to_value (e->values, arg, 0, &value, lang_mask) >= 0)
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
  decoded->mask = mask;
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
  if (opt_index != OPT_SPECIAL_unknown && opt_index != OPT_SPECIAL_ignore
      && opt_index != OPT_SPECIAL_warn_removed)
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

  int opt_array_len = argc;
  opt_array = XNEWVEC (struct cl_decoded_option, opt_array_len);

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
  opt_array[0].mask = 0;
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

      /* Interpret "--param" "key=name" as "--param=key=name".  */
      const char *needle = "--param";
      if (i + 1 < argc && strcmp (opt, needle) == 0)
	{
	  const char *replacement
	    = opts_concat (needle, "=", argv[i + 1], NULL);
	  argv[++i] = replacement;
	}

      /* Expand -fdiagnostics-plain-output to its constituents.  This needs
	 to happen here so that prune_options can handle -fdiagnostics-color
	 specially.  */
      if (!strcmp (opt, "-fdiagnostics-plain-output"))
	{
	  /* If you have changed the default diagnostics output, and this new
	     output is not appropriately "plain" (e.g., the change needs to be
	     undone in order for the testsuite to work properly), then please do
	     the following:
		 1.  Add the necessary option to undo the new behavior to
		     the array below.
		 2.  Update the documentation for -fdiagnostics-plain-output
		     in invoke.texi.  */
	  const char *const expanded_args[] = {
	    "-fno-diagnostics-show-caret",
	    "-fno-diagnostics-show-line-numbers",
	    "-fdiagnostics-color=never",
	    "-fdiagnostics-urls=never",
	    "-fdiagnostics-path-format=separate-events",
	    "-fdiagnostics-text-art-charset=none",
	    "-fno-diagnostics-show-event-links"
	    /* We don't put "-fno-diagnostics-show-highlight-colors" here
	       as -fdiagnostics-color=never makes it redundant.  */
	  };
	  const int num_expanded = ARRAY_SIZE (expanded_args);
	  opt_array_len += num_expanded - 1;
	  opt_array = XRESIZEVEC (struct cl_decoded_option,
				  opt_array, opt_array_len);
	  for (int j = 0, nj; j < num_expanded; j += nj)
	    {
	      nj = decode_cmdline_option (expanded_args + j, lang_mask,
					  &opt_array[num_decoded_options]);
	      num_decoded_options++;
	    }

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

/* Filter out options canceled by the ones after them, and related
   rearrangement.  */

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
  unsigned int options_to_prepend = 0;
  unsigned int Wcomplain_wrong_lang_idx = 0;
  unsigned int fdiagnostics_color_idx = 0;
  unsigned int fdiagnostics_urls_idx = 0;

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
	case OPT_SPECIAL_warn_removed:
	case OPT_SPECIAL_program_name:
	case OPT_SPECIAL_input_file:
	  goto keep;

	/* Do not handle the following yet, just remember the last one.  */
	case OPT_Wcomplain_wrong_lang:
	  gcc_checking_assert (i != 0);
	  if (Wcomplain_wrong_lang_idx == 0)
	    ++options_to_prepend;
	  Wcomplain_wrong_lang_idx = i;
	  continue;
	case OPT_fdiagnostics_color_:
	  gcc_checking_assert (i != 0);
	  if (fdiagnostics_color_idx == 0)
	    ++options_to_prepend;
	  fdiagnostics_color_idx = i;
	  continue;
	case OPT_fdiagnostics_urls_:
	  gcc_checking_assert (i != 0);
	  if (fdiagnostics_urls_idx == 0)
	    ++options_to_prepend;
	  fdiagnostics_urls_idx = i;
	  continue;

	default:
	  gcc_assert (opt_idx < cl_options_count);
	  option = &cl_options[opt_idx];
	  if (option->neg_index < 0)
	    goto keep;

	  /* Skip joined switches.  */
	  if ((option->flags & CL_JOINED)
	      && (!option->cl_reject_negative
		  || (unsigned int) option->neg_index != opt_idx))
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
	      if ((cl_options[next_opt_idx].flags & CL_JOINED)
		  && (!cl_options[next_opt_idx].cl_reject_negative
		      || ((unsigned int) cl_options[next_opt_idx].neg_index
			  != next_opt_idx)))
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

  /* For those not yet handled, put (only) the last at a front position after
     'argv[0]', so they can take effect immediately.  */
  if (options_to_prepend)
    {
      const unsigned int argv_0 = 1;
      memmove (new_decoded_options + argv_0 + options_to_prepend,
	       new_decoded_options + argv_0,
	       sizeof (struct cl_decoded_option)
	       * (new_decoded_options_count - argv_0));
      unsigned int options_prepended = 0;
      if (Wcomplain_wrong_lang_idx != 0)
	{
	  new_decoded_options[argv_0 + options_prepended++]
	    = old_decoded_options[Wcomplain_wrong_lang_idx];
	  new_decoded_options_count++;
	}
      if (fdiagnostics_color_idx != 0)
	{
	  new_decoded_options[argv_0 + options_prepended++]
	    = old_decoded_options[fdiagnostics_color_idx];
	  new_decoded_options_count++;
	}
      if (fdiagnostics_urls_idx != 0)
	{
	  new_decoded_options[argv_0 + options_prepended++]
	    = old_decoded_options[fdiagnostics_urls_idx];
	  new_decoded_options_count++;
	}
      gcc_checking_assert (options_to_prepend == options_prepended);
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
   for one explicitly passed by the user.  control_warning_option
   generated options are considered explicitly passed by the user.
   Returns false if the switch was invalid.  DC is the diagnostic
   context for options affecting diagnostics state, or NULL.  */

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
  HOST_WIDE_INT value = decoded->value;
  HOST_WIDE_INT mask = decoded->mask;
  const struct cl_option *option = &cl_options[opt_index];
  void *flag_var = option_flag_var (opt_index, opts);
  size_t i;

  if (flag_var)
    set_option (opts, (generated_p ? NULL : opts_set),
		opt_index, value, arg, kind, loc, dc, mask);

  for (i = 0; i < handlers->num_handlers; i++)
    if (option->flags & handlers->handlers[i].mask)
      {
	if (!handlers->handlers[i].handler (opts, opts_set, decoded,
					    lang_mask, kind, loc,
					    handlers, dc,
					    handlers->target_option_override_hook))
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
			 size_t opt_index, const char *arg, HOST_WIDE_INT value,
			 unsigned int lang_mask, int kind, location_t loc,
			 const struct cl_option_handlers *handlers,
			 bool generated_p, diagnostic_context *dc)
{
  struct cl_decoded_option decoded;

  generate_option (opt_index, arg, value, lang_mask, &decoded);
  return handle_option (opts, opts_set, &decoded, lang_mask, kind, loc,
			handlers, generated_p, dc);
}

/* Fill in *DECODED with an option described by OPT_INDEX, ARG and
   VALUE for a front end using LANG_MASK.  This is used when the
   compiler generates options internally.  */

void
generate_option (size_t opt_index, const char *arg, HOST_WIDE_INT value,
		 unsigned int lang_mask, struct cl_decoded_option *decoded)
{
  const struct cl_option *option = &cl_options[opt_index];

  decoded->opt_index = opt_index;
  decoded->warn_message = NULL;
  decoded->arg = arg;
  decoded->value = value;
  decoded->mask = 0;
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
  decoded->mask = 0;
  decoded->errors = 0;
}

/* Helper function for listing valid choices and hint for misspelled
   value.  CANDIDATES is a vector containing all valid strings,
   STR is set to a heap allocated string that contains all those
   strings concatenated, separated by spaces, and the return value
   is the closest string from those to ARG, or NULL if nothing is
   close enough.  Callers should XDELETEVEC (STR) after using it
   to avoid memory leaks.  */

const char *
candidates_list_and_hint (const char *arg, char *&str,
			  const auto_vec <const char *> &candidates)
{
  size_t len = 0;
  int i;
  const char *candidate;
  char *p;

  gcc_assert (!candidates.is_empty ());

  FOR_EACH_VEC_ELT (candidates, i, candidate)
    len += strlen (candidate) + 1;

  str = p = XNEWVEC (char, len);
  FOR_EACH_VEC_ELT (candidates, i, candidate)
    {
      len = strlen (candidate);
      memcpy (p, candidate, len);
      p[len] = ' ';
      p += len + 1;
    }
  p[-1] = '\0';
  return find_closest_string (arg, &candidates);
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
      error_at (loc, "command-line option %qs"
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
      if (option->cl_byte_size)
	error_at (loc, "argument to %qs should be a non-negative integer "
		  "optionally followed by a size unit",
		  option->opt_text);
      else
	error_at (loc, "argument to %qs should be a non-negative integer",
		  option->opt_text);
      return true;
    }

  if (errors & CL_ERR_INT_RANGE_ARG)
    {
      error_at (loc, "argument to %qs is not between %d and %d",
		option->opt_text, option->range_min, option->range_max);
      return true;
    }

  if (errors & CL_ERR_ENUM_SET_ARG)
    {
      const struct cl_enum *e = &cl_enums[option->var_enum];
      const char *p = arg;
      unsigned HOST_WIDE_INT used_sets = 0;
      const char *second_opt = NULL;
      size_t second_opt_len = 0;
      errors = 0;
      do
	{
	  const char *q = strchr (p, ',');
	  HOST_WIDE_INT this_value = 0;
	  if (q && q == p)
	    {
	      arg = "";
	      errors = CL_ERR_ENUM_ARG;
	      break;
	    }
	  int idx = enum_arg_to_value (e->values, p, q ? q - p : 0,
				       &this_value, lang_mask);
	  if (idx < 0)
	    {
	      if (q == NULL)
		q = strchr (p, '\0');
	      char *narg = XALLOCAVEC (char, (q - p) + 1);
	      memcpy (narg, p, q - p);
	      narg[q - p] = '\0';
	      arg = narg;
	      errors = CL_ERR_ENUM_ARG;
	      break;
	    }

	  if (option->var_value == CLEV_BITSET)
	    {
	      if (q == NULL)
		break;
	      p = q + 1;
	      continue;
	    }

	  unsigned set = e->values[idx].flags >> CL_ENUM_SET_SHIFT;
	  gcc_checking_assert (set >= 1 && set <= HOST_BITS_PER_WIDE_INT);
	  if ((used_sets & (HOST_WIDE_INT_1U << (set - 1))) != 0)
	    {
	      if (q == NULL)
		q = strchr (p, '\0');
	      if (second_opt == NULL)
		{
		  used_sets = HOST_WIDE_INT_1U << (set - 1);
		  second_opt = p;
		  second_opt_len = q - p;
		  p = arg;
		  continue;
		}
	      char *args = XALLOCAVEC (char, (q - p) + 1 + second_opt_len + 1);
	      memcpy (args, p, q - p);
	      args[q - p] = '\0';
	      memcpy (args + (q - p) + 1, second_opt, second_opt_len);
	      args[(q - p) + 1 + second_opt_len] = '\0';
	      error_at (loc, "invalid argument in option %qs", opt);
	      if (strcmp (args, args + (q - p) + 1) == 0)
		inform (loc, "%qs specified multiple times in the same option",
			args);
	      else
		inform (loc, "%qs is mutually exclusive with %qs and cannot be"
			     " specified together", args, args + (q - p) + 1);
	      return true;
	    }
	  used_sets |= HOST_WIDE_INT_1U << (set - 1);
	  if (q == NULL)
	    break;
	  p = q + 1;
	}
      while (1);
    }

  if (errors & CL_ERR_ENUM_ARG)
    {
      const struct cl_enum *e = &cl_enums[option->var_enum];
      unsigned int i;
      char *s;

      auto_diagnostic_group d;
      if (e->unknown_error)
	error_at (loc, e->unknown_error, arg);
      else
	error_at (loc, "unrecognized argument in option %qs", opt);

      auto_vec <const char *> candidates;
      for (i = 0; e->values[i].arg != NULL; i++)
	{
	  if (!enum_arg_ok_for_language (&e->values[i], lang_mask))
	    continue;
	  candidates.safe_push (e->values[i].arg);
	}
      const char *hint = candidates_list_and_hint (arg, s, candidates);
      if (hint)
	inform (loc, "valid arguments to %qs are: %s; did you mean %qs?",
		option->opt_text, s, hint);
      else
	inform (loc, "valid arguments to %qs are: %s", option->opt_text, s);
      XDELETEVEC (s);

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
	error_at (loc, "unrecognized command-line option %qs", decoded->arg);
      return;
    }

  if (decoded->opt_index == OPT_SPECIAL_ignore)
    return;

  if (decoded->opt_index == OPT_SPECIAL_warn_removed)
    {
      /* Warn only about positive ignored options.  */
      if (decoded->value)
	warning_at (loc, 0, "switch %qs is no longer supported", opt);
      return;
    }

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
    error_at (loc, "unrecognized command-line option %qs", opt);
}

/* Set any field in OPTS, and OPTS_SET if not NULL, for option
   OPT_INDEX according to VALUE and ARG, diagnostic kind KIND,
   location LOC, using diagnostic context DC if not NULL for
   diagnostic classification.  */

void
set_option (struct gcc_options *opts, struct gcc_options *opts_set,
	    int opt_index, HOST_WIDE_INT value, const char *arg, int kind,
	    location_t loc, diagnostic_context *dc,
	    HOST_WIDE_INT mask /* = 0 */)
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
    case CLVC_INTEGER:
	if (option->cl_host_wide_int)
	  {
	    *(HOST_WIDE_INT *) flag_var = value;
	    if (set_flag_var)
	      *(HOST_WIDE_INT *) set_flag_var = 1;
	  }
	else
	  {
	    if (value > INT_MAX)
	      error_at (loc, "argument to %qs is bigger than %d",
			option->opt_text, INT_MAX);
	    else
	      {
		*(int *) flag_var = value;
		if (set_flag_var)
		  *(int *) set_flag_var = 1;
	      }
	  }

	break;

    case CLVC_SIZE:
	if (option->cl_host_wide_int)
	  {
	    *(HOST_WIDE_INT *) flag_var = value;
	    if (set_flag_var)
	      *(HOST_WIDE_INT *) set_flag_var = value;
	  }
	else
	  {
	    *(int *) flag_var = value;
	    if (set_flag_var)
	      *(int *) set_flag_var = value;
	  }

	break;

    case CLVC_EQUAL:
	if (option->cl_host_wide_int)
	  {
	    *(HOST_WIDE_INT *) flag_var = (value
					   ? option->var_value
					   : !option->var_value);
	    if (set_flag_var)
	      *(HOST_WIDE_INT *) set_flag_var = 1;
	  }
	else
	  {
	    *(int *) flag_var = (value
				 ? option->var_value
				 : !option->var_value);
	    if (set_flag_var)
	      *(int *) set_flag_var = 1;
	  }
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

	if (mask)
	  e->set (flag_var, value | (e->get (flag_var) & ~mask));
	else
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
   or -1 if it isn't a simple on-off switch
   (or if the value is unknown, typically set later in target).  */

int
option_enabled (int opt_idx, unsigned lang_mask, void *opts)
{
  const struct cl_option *option = &(cl_options[opt_idx]);

  /* A language-specific option can only be considered enabled when it's
     valid for the current language.  */
  if (!(option->flags & CL_COMMON)
      && (option->flags & CL_LANG_ALL)
      && !(option->flags & lang_mask))
    return 0;

  struct gcc_options *optsg = (struct gcc_options *) opts;
  void *flag_var = option_flag_var (opt_idx, optsg);

  if (flag_var)
    switch (option->var_type)
      {
      case CLVC_INTEGER:
	if (option->cl_host_wide_int)
	  {
	    HOST_WIDE_INT v = *(HOST_WIDE_INT *) flag_var;
	    return v != 0 ? (v < 0 ? -1 : 1) : 0;
	  }
	else
	  {
	    int v = *(int *) flag_var;
	    return v != 0 ? (v < 0 ? -1 : 1) : 0;
	  }

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

      case CLVC_SIZE:
	if (option->cl_host_wide_int)
	  return *(HOST_WIDE_INT *) flag_var != -1;
	else
	  return *(int *) flag_var != -1;

      case CLVC_STRING:
      case CLVC_ENUM:
      case CLVC_DEFER:
	break;
      }
  return -1;
}

int
compiler_diagnostic_option_manager::
option_enabled_p (diagnostic_option_id opt_id) const
{
  return option_enabled (opt_id.m_idx, m_lang_mask, m_opts);
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
    case CLVC_INTEGER:
    case CLVC_EQUAL:
    case CLVC_SIZE:
      state->data = flag_var;
      state->size = (cl_options[option].cl_host_wide_int
		     ? sizeof (HOST_WIDE_INT)
		     : sizeof (int));
      break;

    case CLVC_BIT_CLEAR:
    case CLVC_BIT_SET:
      state->ch = option_enabled (option, -1, opts);
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
  if (opt_index == OPT_SPECIAL_ignore || opt_index == OPT_SPECIAL_warn_removed)
    return;
  if (dc)
    diagnostic_classify_diagnostic (dc, opt_index, (diagnostic_t) kind, loc);
  if (imply)
    {
      /* -Werror=foo implies -Wfoo.  */
      const struct cl_option *option = &cl_options[opt_index];
      HOST_WIDE_INT value = 1;

      if (option->var_type == CLVC_INTEGER
	  || option->var_type == CLVC_ENUM
	  || option->var_type == CLVC_SIZE)
	{

	  if (arg && *arg == '\0' && !option->cl_missing_ok)
	    arg = NULL;

	  if ((option->flags & CL_JOINED) && arg == NULL)
	    {
	      cmdline_handle_error (loc, option, option->opt_text, arg,
				    CL_ERR_MISSING_ARG, lang_mask);
	      return;
	    }

	  /* If the switch takes an integer argument, convert it.  */
	  if (arg && (option->cl_uinteger || option->cl_host_wide_int))
	    {
	      int error = 0;
	      value = *arg ? integral_argument (arg, &error,
						option->cl_byte_size) : 0;
	      if (error)
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

	      if (enum_arg_to_value (e->values, arg, 0, &value,
				     lang_mask) >= 0)
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
	}

      handle_generated_option (opts, opts_set,
			       opt_index, arg, value, lang_mask,
			       kind, loc, handlers, false, dc);
    }
}

/* Parse options in COLLECT_GCC_OPTIONS and push them on ARGV_OBSTACK.
   Store number of arguments into ARGC_P.  */

void
parse_options_from_collect_gcc_options (const char *collect_gcc_options,
					obstack *argv_obstack,
					int *argc_p)
{
  char *argv_storage = xstrdup (collect_gcc_options);
  int j, k;

  for (j = 0, k = 0; argv_storage[j] != '\0'; ++j)
    {
      if (argv_storage[j] == '\'')
	{
	  obstack_ptr_grow (argv_obstack, &argv_storage[k]);
	  ++j;
	  do
	    {
	      if (argv_storage[j] == '\0')
		fatal_error (input_location,
			     "malformed %<COLLECT_GCC_OPTIONS%>");
	      else if (startswith (&argv_storage[j], "'\\''"))
		{
		  argv_storage[k++] = '\'';
		  j += 4;
		}
	      else if (argv_storage[j] == '\'')
		break;
	      else
		argv_storage[k++] = argv_storage[j++];
	    }
	  while (1);
	  argv_storage[k++] = '\0';
	}
    }

  obstack_ptr_grow (argv_obstack, NULL);
  *argc_p = obstack_object_size (argv_obstack) / sizeof (void *) - 1;
}

/* Prepend -Xassembler for each option in COLLECT_AS_OPTIONS,
   and push on O.  */

void prepend_xassembler_to_collect_as_options (const char *collect_as_options,
					       obstack *o)
{
  obstack opts_obstack;
  int opts_count;

  obstack_init (&opts_obstack);
  parse_options_from_collect_gcc_options (collect_as_options,
					  &opts_obstack, &opts_count);
  const char **assembler_opts = XOBFINISH (&opts_obstack, const char **);

  for (int i = 0; i < opts_count; i++)
    {
      obstack_grow (o, " '-Xassembler' ",
		    strlen (" '-Xassembler' "));
      const char *opt = assembler_opts[i];
      obstack_1grow (o, '\'');
      obstack_grow (o, opt, strlen (opt));
      obstack_1grow (o, '\'');
    }
}

jobserver_info::jobserver_info ()
{
  /* Traditionally, GNU make uses opened pipes for jobserver-auth,
    e.g. --jobserver-auth=3,4.
    Starting with GNU make 4.4, one can use --jobserver-style=fifo
    and then named pipe is used: --jobserver-auth=fifo:/tmp/hcsparta.  */

  /* Detect jobserver and drop it if it's not working.  */
  string js_needle = "--jobserver-auth=";
  string fifo_prefix = "fifo:";

  const char *envval = getenv ("MAKEFLAGS");
  if (envval != NULL)
    {
      string makeflags = envval;
      size_t n = makeflags.rfind (js_needle);
      if (n != string::npos)
	{
	  string ending = makeflags.substr (n + js_needle.size ());
	  if (ending.find (fifo_prefix) == 0)
	    {
	      ending = ending.substr (fifo_prefix.size ());
	      pipe_path = ending.substr (0, ending.find (' '));
	      is_active = true;
	    }
	  else if (sscanf (makeflags.c_str () + n + js_needle.size (),
			   "%d,%d", &rfd, &wfd) == 2
	      && rfd > 0
	      && wfd > 0
	      && is_valid_fd (rfd)
	      && is_valid_fd (wfd))
	    is_active = true;
	  else
	    {
	      string dup = makeflags.substr (0, n);
	      size_t pos = makeflags.find (' ', n);
	      if (pos != string::npos)
		dup += makeflags.substr (pos);
	      skipped_makeflags = "MAKEFLAGS=" + dup;
	      error_msg
		= "cannot access %<" + js_needle + "%> file descriptors";
	    }
	}
      error_msg = "%<" + js_needle + "%> is not present in %<MAKEFLAGS%>";
    }
  else
    error_msg = "%<MAKEFLAGS%> environment variable is unset";

  if (!error_msg.empty ())
    error_msg = "jobserver is not available: " + error_msg;
}

void
jobserver_info::connect ()
{
  if (!pipe_path.empty ())
    {
#if HOST_HAS_O_NONBLOCK
      pipefd = open (pipe_path.c_str (), O_RDWR | O_NONBLOCK);
      is_connected = true;
#else
      is_connected = false;
#endif
    }
  else
    is_connected = true;
}

void
jobserver_info::disconnect ()
{
  if (!pipe_path.empty ())
    {
      int res = close (pipefd);
      gcc_assert (res == 0);
      pipefd = -1;
    }
}

bool
jobserver_info::get_token ()
{
  int fd = pipe_path.empty () ? rfd : pipefd;
  char c;
  unsigned n = read (fd, &c, 1);
  if (n != 1)
    {
      gcc_assert (errno == EAGAIN);
      return false;
    }
  else
    return true;
}

void
jobserver_info::return_token ()
{
  int fd = pipe_path.empty () ? wfd : pipefd;
  char c = 'G';
  int res = write (fd, &c, 1);
  gcc_assert (res == 1);
}
