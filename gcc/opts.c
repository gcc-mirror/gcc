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
#include "langhooks.h"
#include "opts.h"
#include "options.h"
#include "flags.h"
#include "toplev.h"

static size_t find_opt (const char *, int);
static int common_handle_option (size_t scode, const char *arg, int value);

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
		  if (strncmp (input, cl_options[md].opt_text, opt_len))
		    break;
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

/* Handle the switch beginning at ARGV, with ARGC remaining.  */
int
handle_option (int argc ATTRIBUTE_UNUSED, char **argv, int lang_mask)
{
  size_t opt_index;
  const char *opt, *arg = 0;
  char *dup = 0;
  bool on = true;
  int result = 0, temp;
  const struct cl_option *option;

  opt = argv[0];

  /* Interpret "-" or a non-switch as a file name.  */
  if (opt[0] != '-' || opt[1] == '\0')
    {
      opt_index = cl_options_count;
      arg = opt;
      main_input_filename = opt;
      result = (*lang_hooks.handle_option) (opt_index, arg, on);
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
	  on = false;
	}

      opt_index = find_opt (opt + 1, lang_mask | CL_COMMON);
      if (opt_index == cl_options_count)
	goto done;

      option = &cl_options[opt_index];

      /* Reject negative form of switches that don't take negatives.  */
      if (!on && (option->flags & CL_REJECT_NEGATIVE))
	goto done;

      /* We've recognized this switch.  */
      result = 1;

      /* Sort out any argument the switch takes.  */
      if (option->flags & (CL_JOINED | CL_SEPARATE))
	{
	  if (option->flags & CL_JOINED)
	    {
	      /* Have arg point to the original switch.  This is because
		 some code, such as disable_builtin_function, expects its
		 argument to be persistent until the program exits.  */
	      arg = argv[0] + cl_options[opt_index].opt_len + 1;
	      if (!on)
		arg += strlen ("no-");
	    }

	  /* If we don't have an argument, and CL_SEPARATE, try the next
	     argument in the vector.  */
	  if (!arg || (*arg == '\0' && option->flags & CL_SEPARATE))
	    {
	      arg = argv[1];
	      result = 2;
	    }

	  /* Canonicalize missing arguments as NULL for the handler.  */
	  if (*arg == '\0')
	    arg = NULL;
	}

      if (option->flags & lang_mask)
	{
	  temp = (*lang_hooks.handle_option) (opt_index, arg, on);
	  if (temp <= 0)
	    result = temp;
	}

      if (result > 0 && (option->flags & CL_COMMON))
	{
	  if (common_handle_option (opt_index, arg, on) == 0)
	    result = 0;
	}
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

    case OPT_quiet:
      quiet_flag = 1;
      break;
    }

  return 1;
}
