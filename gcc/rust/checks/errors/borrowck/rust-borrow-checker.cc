// Copyright (C) 2020-2025 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include "rust-borrow-checker.h"
#include "rust-borrow-checker-diagnostics.h"
#include "rust-function-collector.h"
#include "rust-bir-fact-collector.h"
#include "rust-bir-builder.h"
#include "rust-bir-dump.h"
#include "polonius/rust-polonius.h"

namespace Rust {
namespace HIR {

void
dump_function_bir (const std::string &filename, BIR::Function &func,
		   const std::string &name)
{
  std::ofstream file;
  file.open (filename);
  if (file.fail ())
    {
      rust_error_at (UNKNOWN_LOCATION, "Failed to open file %s",
		     filename.c_str ());
      return;
    }
  BIR::Dump (file, func, name).go ();
  file.close ();
}

void
BorrowChecker::go (HIR::Crate &crate)
{
  std::string crate_name;

  if (enable_dump_bir)
    {
      mkdir ("bir_dump", 0755);
      auto &mappings = Analysis::Mappings::get ();
      crate_name
	= *mappings.get_crate_name (crate.get_mappings ().get_crate_num ());
      mkdir ("nll_facts_gccrs", 0755);
    }

  FunctionCollector collector;
  collector.go (crate);

  for (auto func : collector.get_functions ())
    {
      rust_debug_loc (func->get_locus (), "\nChecking function %s\n",
		      func->get_function_name ().as_string ().c_str ());

      BIR::BuilderContext ctx;
      BIR::Builder builder (ctx);
      auto bir = builder.build (*func);

      if (enable_dump_bir)
	{
	  std::string filename = "bir_dump/" + crate_name + "."
				 + func->get_function_name ().as_string ()
				 + ".bir.dump";
	  dump_function_bir (filename, bir,
			     func->get_function_name ().as_string ());
	}

      auto facts = BIR::FactCollector::collect (bir);

      if (enable_dump_bir)
	{
	  auto dir
	    = "nll_facts_gccrs/" + func->get_function_name ().as_string ();
	  mkdir (dir.c_str (), 0755);
	  auto dump_facts_to_file
	    = [&] (const std::string &suffix,
		   void (Polonius::Facts::*fn) (std::ostream &) const) {
		std::string filename = "nll_facts_gccrs/"
				       + func->get_function_name ().as_string ()
				       + "/" + suffix + ".facts";
		std::ofstream file;
		file.open (filename);
		if (file.fail ())
		  {
		    abort ();
		  }

		// Run dump
		// BEWARE: this callback charade is a workaround because gcc48
		// won't let me return a file from a function
		(facts.*fn) (file);
	      };

	  dump_facts_to_file ("loan_issued_at",
			      &Polonius::Facts::dump_loan_issued_at);
	  dump_facts_to_file ("loan_killed_at",
			      &Polonius::Facts::dump_loan_killed_at);
	  dump_facts_to_file ("loan_invalidated_at",
			      &Polonius::Facts::dump_loan_invalidated_at);
	  dump_facts_to_file ("subset_base",
			      &Polonius::Facts::dump_subset_base);
	  dump_facts_to_file ("universal_region",
			      &Polonius::Facts::dump_universal_region);
	  dump_facts_to_file ("cfg_edge", &Polonius::Facts::dump_cfg_edge);
	  dump_facts_to_file ("var_used_at",
			      &Polonius::Facts::dump_var_used_at);
	  dump_facts_to_file ("var_defined_at",
			      &Polonius::Facts::dump_var_defined_at);
	  dump_facts_to_file ("var_dropped_at",
			      &Polonius::Facts::dump_var_dropped_at);
	  dump_facts_to_file ("use_of_var_derefs_origin",
			      &Polonius::Facts::dump_use_of_var_derefs_origin);
	  dump_facts_to_file ("drop_of_var_derefs_origin",
			      &Polonius::Facts::dump_drop_of_var_derefs_origin);
	  dump_facts_to_file ("child_path", &Polonius::Facts::dump_child_path);
	  dump_facts_to_file ("path_is_var",
			      &Polonius::Facts::dump_path_is_var);
	  dump_facts_to_file ("known_placeholder_subset",
			      &Polonius::Facts::dump_known_placeholder_subset);
	  dump_facts_to_file ("path_moved_at_base",
			      &Polonius::Facts::dump_path_moved_at_base);
	  dump_facts_to_file ("path_accessed_at_base",
			      &Polonius::Facts::dump_path_accessed_at_base);
	  dump_facts_to_file ("path_assigned_at_base",
			      &Polonius::Facts::dump_path_assigned_at_base);
	  dump_facts_to_file ("placeholder",
			      &Polonius::Facts::dump_placeholder);
	}

      auto result
	= Polonius::polonius_run (facts.freeze (), rust_be_debug_p ());

      // convert to std::vector variation for easier navigation
      auto loan_errors = make_vector (result.loan_errors);
      auto move_errors = make_vector (result.move_errors);
      auto subset_errors = make_vector (result.subset_errors);

      // free allocated data
      delete result.loan_errors;
      delete result.move_errors;
      delete result.subset_errors;

      BIR::BorrowCheckerDiagnostics (func, bir, facts, move_errors, loan_errors,
				     subset_errors)
	.report_errors ();
    }

  for (auto closure ATTRIBUTE_UNUSED : collector.get_closures ())
    rust_sorry_at (closure->get_locus (),
		   "Closure borrow checking is not implemented yet.");
}

} // namespace HIR
} // namespace Rust
