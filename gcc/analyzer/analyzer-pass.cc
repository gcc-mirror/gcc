/* Integration of the analyzer with GCC's pass manager.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "context.h"
#include "tree-pass.h"
#include "diagnostic.h"
#include "options.h"
#include "analyzer/engine.h"

namespace {

/* Data for the analyzer pass.  */

const pass_data pass_data_analyzer =
{
  IPA_PASS, /* type */
  "analyzer", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_ANALYZER, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

/* The analyzer pass.  */

class pass_analyzer : public ipa_opt_pass_d
{
public:
   pass_analyzer(gcc::context *ctxt)
   : ipa_opt_pass_d (pass_data_analyzer, ctxt,
		     NULL, /* generate_summary */
		     NULL, /* write_summary */
		     NULL, /* read_summary */
		     NULL, /* write_optimization_summary */
		     NULL, /* read_optimization_summary */
		     NULL, /* stmt_fixup */
		     0, /* function_transform_todo_flags_start */
		     NULL, /* function_transform */
		     NULL) /* variable_transform */
  {}

  /* opt_pass methods: */
  bool gate (function *) FINAL OVERRIDE;
  unsigned int execute (function *) FINAL OVERRIDE;
}; // class pass_analyzer

/* Only run the analyzer if -fanalyzer.  */

bool
pass_analyzer::gate (function *)
{
  return flag_analyzer != 0;
}

/* Entrypoint for the analyzer pass.  */

unsigned int
pass_analyzer::execute (function *)
{
#if ENABLE_ANALYZER
  ana::run_checkers ();
#else
  sorry ("%qs was not enabled in this build of GCC"
	 " (missing configure-time option %qs)",
	 "-fanalyzer", "--enable-analyzer");
#endif

  return 0;
}

} // anon namespace

/* Make an instance of the analyzer pass.  */

ipa_opt_pass_d *
make_pass_analyzer (gcc::context *ctxt)
{
  return new pass_analyzer (ctxt);
}
