/* Provide option suggestion for --complete option and a misspelled
   used by a user.
   Copyright (C) 2016-2018 Free Software Foundation, Inc.

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
#include "tm.h"
#include "opts.h"
#include "params.h"
#include "spellcheck.h"
#include "opt-suggestions.h"
#include "selftest.h"

option_proposer::~option_proposer ()
{
  delete m_option_suggestions;
}

const char *
option_proposer::suggest_option (const char *bad_opt)
{
  /* Lazily populate m_option_suggestions.  */
  if (!m_option_suggestions)
    build_option_suggestions ();
  gcc_assert (m_option_suggestions);

  /* "m_option_suggestions" is now populated.  Use it.  */
  return find_closest_string
    (bad_opt,
     (auto_vec <const char *> *) m_option_suggestions);
}

void
option_proposer::build_option_suggestions (void)
{
  gcc_assert (m_option_suggestions == NULL);
  m_option_suggestions = new auto_string_vec ();

  /* We build a vec of m_option_suggestions, using add_misspelling_candidates
     to add copies of strings, without a leading dash.  */

  for (unsigned int i = 0; i < cl_options_count; i++)
    {
      const struct cl_option *option = &cl_options[i];
      const char *opt_text = option->opt_text;
      switch (i)
	{
	default:
	  if (option->var_type == CLVC_ENUM)
	    {
	      const struct cl_enum *e = &cl_enums[option->var_enum];
	      for (unsigned j = 0; e->values[j].arg != NULL; j++)
		{
		  char *with_arg = concat (opt_text, e->values[j].arg, NULL);
		  add_misspelling_candidates (m_option_suggestions, option,
					      with_arg);
		  free (with_arg);
		}
	    }
	  else
	    add_misspelling_candidates (m_option_suggestions, option,
					opt_text);
	  break;

	case OPT_fsanitize_:
	case OPT_fsanitize_recover_:
	  /* -fsanitize= and -fsanitize-recover= can take
	     a comma-separated list of arguments.  Given that combinations
	     are supported, we can't add all potential candidates to the
	     vec, but if we at least add them individually without commas,
	     we should do a better job e.g. correcting
	       "-sanitize=address"
	     to
	       "-fsanitize=address"
	     rather than to "-Wframe-address" (PR driver/69265).  */
	  {
	    for (int j = 0; sanitizer_opts[j].name != NULL; ++j)
	      {
		struct cl_option optb;
		/* -fsanitize=all is not valid, only -fno-sanitize=all.
		   So don't register the positive misspelling candidates
		   for it.  */
		if (sanitizer_opts[j].flag == ~0U && i == OPT_fsanitize_)
		  {
		    optb = *option;
		    optb.opt_text = opt_text = "-fno-sanitize=";
		    optb.cl_reject_negative = true;
		    option = &optb;
		  }
		/* Get one arg at a time e.g. "-fsanitize=address".  */
		char *with_arg = concat (opt_text,
					 sanitizer_opts[j].name,
					 NULL);
		/* Add with_arg and all of its variant spellings e.g.
		   "-fno-sanitize=address" to candidates (albeit without
		   leading dashes).  */
		add_misspelling_candidates (m_option_suggestions, option,
					    with_arg);
		free (with_arg);
	      }
	  }
	  break;
	}
    }
}
