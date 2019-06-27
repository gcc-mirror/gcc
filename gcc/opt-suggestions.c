/* Provide option suggestion for --complete option and a misspelled
   used by a user.
   Copyright (C) 2016-2019 Free Software Foundation, Inc.

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
#include "common/common-target.h"
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
    build_option_suggestions (NULL);
  gcc_assert (m_option_suggestions);

  /* "m_option_suggestions" is now populated.  Use it.  */
  return find_closest_string
    (bad_opt,
     (auto_vec <const char *> *) m_option_suggestions);
}

/* Populate RESULTS with valid completions of options that begin
   with OPTION_PREFIX.  */

void
option_proposer::get_completions (const char *option_prefix,
				  auto_string_vec &results)
{
  /* Bail out for an invalid input.  */
  if (option_prefix == NULL || option_prefix[0] == '\0')
    return;

  /* Option suggestions are built without first leading dash character.  */
  if (option_prefix[0] == '-')
    option_prefix++;

  size_t length = strlen (option_prefix);

  /* Handle OPTION_PREFIX starting with "-param".  */
  const char *prefix = "-param";
  if (length >= strlen (prefix)
      && strstr (option_prefix, prefix) == option_prefix)
    {
      /* We support both '-param-xyz=123' and '-param xyz=123' */
      option_prefix += strlen (prefix);
      char separator = option_prefix[0];
      option_prefix++;
      if (separator == ' ' || separator == '=')
	find_param_completions (separator, option_prefix, results);
    }
  else
    {
      /* Lazily populate m_option_suggestions.  */
      if (!m_option_suggestions)
	build_option_suggestions (option_prefix);
      gcc_assert (m_option_suggestions);

      for (unsigned i = 0; i < m_option_suggestions->length (); i++)
	{
	  char *candidate = (*m_option_suggestions)[i];
	  if (strlen (candidate) >= length
	      && strstr (candidate, option_prefix) == candidate)
	    results.safe_push (concat ("-", candidate, NULL));
	}
    }
}

/* Print on stdout a list of valid options that begin with OPTION_PREFIX,
   one per line, suitable for use by Bash completion.

   Implementation of the "-completion=" option.  */

void
option_proposer::suggest_completion (const char *option_prefix)
{
  auto_string_vec results;
  get_completions (option_prefix, results);
  for (unsigned i = 0; i < results.length (); i++)
    printf ("%s\n", results[i]);
}

void
option_proposer::build_option_suggestions (const char *prefix)
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

	      /* Add also variant without an option argument.  */
	      add_misspelling_candidates (m_option_suggestions, option,
					  opt_text);
	    }
	  else
	    {
	      bool option_added = false;
	      if (option->flags & CL_TARGET)
		{
		  vec<const char *> option_values
		    = targetm_common.get_valid_option_values (i, prefix);
		  if (!option_values.is_empty ())
		    {
		      option_added = true;
		      for (unsigned j = 0; j < option_values.length (); j++)
			{
			  char *with_arg = concat (opt_text, option_values[j],
						   NULL);
			  add_misspelling_candidates (m_option_suggestions, option,
						      with_arg);
			  free (with_arg);
			}
		    }
		  option_values.release ();
		}

	      if (!option_added)
		add_misspelling_candidates (m_option_suggestions, option,
					    opt_text);
	    }
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
	    /* Add also variant without an option argument.  */
	    add_misspelling_candidates (m_option_suggestions, option,
					opt_text);

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

/* Find parameter completions for --param format with SEPARATOR.
   Again, save the completions into results.  */

void
option_proposer::find_param_completions (const char separator,
					 const char *param_prefix,
					 auto_string_vec &results)
{
  char separator_str[] = {separator, '\0'};
  size_t length = strlen (param_prefix);
  for (unsigned i = 0; i < get_num_compiler_params (); ++i)
    {
      const char *candidate = compiler_params[i].option;
      if (strlen (candidate) >= length
	  && strstr (candidate, param_prefix) == candidate)
	results.safe_push (concat ("--param", separator_str, candidate, NULL));
    }
}

#if CHECKING_P

namespace selftest {

/* Verify that PROPOSER generates sane auto-completion suggestions
   for OPTION_PREFIX.  */

static void
verify_autocompletions (option_proposer &proposer, const char *option_prefix)
{
  auto_string_vec suggestions;
  proposer.get_completions (option_prefix, suggestions);

  /* There must be at least one suggestion, and every suggestion must
     indeed begin with OPTION_PREFIX.  */

  ASSERT_GT (suggestions.length (), 0);

  for (unsigned i = 0; i < suggestions.length (); i++)
    ASSERT_STR_STARTSWITH (suggestions[i], option_prefix);
}

/* Verify that valid options are auto-completed correctly.  */

static void
test_completion_valid_options (option_proposer &proposer)
{
  const char *option_prefixes[] =
  {
    "-fno-var-tracking-assignments-toggle",
    "-fpredictive-commoning",
    "--param=stack-clash-protection-guard-size",
    "--param=max-predicted-iterations",
    "-ftree-loop-distribute-patterns",
    "-fno-var-tracking",
    "-Walloc-zero",
    "--param=ipa-cp-value-list-size",
    "-Wsync-nand",
    "-Wno-attributes",
    "--param=tracer-dynamic-coverage-feedback",
    "-Wno-format-contains-nul",
    "-Wnamespaces",
    "-fisolate-erroneous-paths-attribute",
    "-Wno-underflow",
    "-Wtarget-lifetime",
    "--param=asan-globals",
    "-Wno-empty-body",
    "-Wno-odr",
    "-Wformat-zero-length",
    "-Wstringop-truncation",
    "-fno-ipa-vrp",
    "-fmath-errno",
    "-Warray-temporaries",
    "-Wno-unused-label",
    "-Wreturn-local-addr",
    "--param=sms-dfa-history",
    "--param=asan-instrument-reads",
    "-Wreturn-type",
    "-Wc++17-compat",
    "-Wno-effc++",
    "--param=max-fields-for-field-sensitive",
    "-fisolate-erroneous-paths-dereference",
    "-fno-defer-pop",
    "-Wcast-align=strict",
    "-foptimize-strlen",
    "-Wpacked-not-aligned",
    "-funroll-loops",
    "-fif-conversion2",
    "-Wdesignated-init",
    "--param=max-iterations-computation-cost",
    "-Wmultiple-inheritance",
    "-fno-sel-sched-reschedule-pipelined",
    "-Wassign-intercept",
    "-Wno-format-security",
    "-fno-sched-stalled-insns",
    "-fbtr-bb-exclusive",
    "-fno-tree-tail-merge",
    "-Wlong-long",
    "-Wno-unused-but-set-parameter",
    NULL
  };

  for (const char **ptr = option_prefixes; *ptr != NULL; ptr++)
    verify_autocompletions (proposer, *ptr);
}

/* Verify that valid parameters are auto-completed correctly,
   both with the "--param=PARAM" form and the "--param PARAM" form.  */

static void
test_completion_valid_params (option_proposer &proposer)
{
  const char *option_prefixes[] =
  {
    "--param=sched-state-edge-prob-cutoff",
    "--param=iv-consider-all-candidates-bound",
    "--param=align-threshold",
    "--param=prefetch-min-insn-to-mem-ratio",
    "--param=max-unrolled-insns",
    "--param=max-early-inliner-iterations",
    "--param=max-vartrack-reverse-op-size",
    "--param=ipa-cp-loop-hint-bonus",
    "--param=tracer-min-branch-ratio",
    "--param=graphite-max-arrays-per-scop",
    "--param=sink-frequency-threshold",
    "--param=max-cse-path-length",
    "--param=sra-max-scalarization-size-Osize",
    "--param=prefetch-latency",
    "--param=dse-max-object-size",
    "--param=asan-globals",
    "--param=max-vartrack-size",
    "--param=case-values-threshold",
    "--param=max-slsr-cand-scan",
    "--param=min-insn-to-prefetch-ratio",
    "--param=tracer-min-branch-probability",
    "--param sink-frequency-threshold",
    "--param max-cse-path-length",
    "--param sra-max-scalarization-size-Osize",
    "--param prefetch-latency",
    "--param dse-max-object-size",
    "--param asan-globals",
    "--param max-vartrack-size",
    NULL
  };

  for (const char **ptr = option_prefixes; *ptr != NULL; ptr++)
    verify_autocompletions (proposer, *ptr);
}

/* Return true when EXPECTED is one of completions for OPTION_PREFIX string.  */

static bool
in_completion_p (option_proposer &proposer, const char *option_prefix,
		 const char *expected)
{
  auto_string_vec suggestions;
  proposer.get_completions (option_prefix, suggestions);

  for (unsigned i = 0; i < suggestions.length (); i++)
    {
      char *r = suggestions[i];
      if (strcmp (r, expected) == 0)
	return true;
    }

  return false;
}

/* Return true when PROPOSER does not find any partial completion
   for OPTION_PREFIX.  */

static bool
empty_completion_p (option_proposer &proposer, const char *option_prefix)
{
  auto_string_vec suggestions;
  proposer.get_completions (option_prefix, suggestions);
  return suggestions.is_empty ();
}

/* Verify autocompletions of partially-complete options.  */

static void
test_completion_partial_match (option_proposer &proposer)
{
  ASSERT_TRUE (in_completion_p (proposer, "-fsani", "-fsanitize=address"));
  ASSERT_TRUE (in_completion_p (proposer, "-fsani",
				"-fsanitize-address-use-after-scope"));
  ASSERT_TRUE (in_completion_p (proposer, "-fipa-icf", "-fipa-icf-functions"));
  ASSERT_TRUE (in_completion_p (proposer, "-fipa-icf", "-fipa-icf"));
  ASSERT_TRUE (in_completion_p (proposer, "--param=",
				"--param=max-vartrack-reverse-op-size"));
  ASSERT_TRUE (in_completion_p (proposer, "--param ",
				"--param max-vartrack-reverse-op-size"));

  ASSERT_FALSE (in_completion_p (proposer, "-fipa-icf", "-fipa"));
  ASSERT_FALSE (in_completion_p (proposer, "-fipa-icf-functions", "-fipa-icf"));

  ASSERT_FALSE (empty_completion_p (proposer, "-"));
  ASSERT_FALSE (empty_completion_p (proposer, "-fipa"));
  ASSERT_FALSE (empty_completion_p (proposer, "--par"));
}

/* Verify that autocompletion does not return any match for garbage inputs.  */

static void
test_completion_garbage (option_proposer &proposer)
{
  ASSERT_TRUE (empty_completion_p (proposer, NULL));
  ASSERT_TRUE (empty_completion_p (proposer, ""));
  ASSERT_TRUE (empty_completion_p (proposer, "- "));
  ASSERT_TRUE (empty_completion_p (proposer, "123456789"));
  ASSERT_TRUE (empty_completion_p (proposer, "---------"));
  ASSERT_TRUE (empty_completion_p (proposer, "#########"));
  ASSERT_TRUE (empty_completion_p (proposer, "- - - - - -"));
  ASSERT_TRUE (empty_completion_p (proposer, "-fsanitize=address2"));
}

/* Run all of the selftests within this file.  */

void
opt_proposer_c_tests ()
{
  option_proposer proposer;

  test_completion_valid_options (proposer);
  test_completion_valid_params (proposer);
  test_completion_partial_match (proposer);
  test_completion_garbage (proposer);
}

} // namespace selftest

#endif /* #if CHECKING_P */
