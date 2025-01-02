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
#include "rust-function-collector.h"
#include "rust-bir-builder.h"
#include "rust-bir-dump.h"
#include "rust-bir-fact-collector.h"

namespace Rust {
namespace HIR {

void
mkdir_wrapped (const std::string &dirname)
{
  int ret;
#ifdef _WIN32
  ret = _mkdir (dirname.c_str ());
#elif unix
  ret = mkdir (dirname.c_str (), 0775);
#elif __APPLE__
  ret = mkdir (dirname.c_str (), 0775);
#endif
  (void) ret;
}

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
      mkdir_wrapped ("bir_dump");
      auto mappings = Analysis::Mappings::get ();
      bool ok
	= mappings->get_crate_name (crate.get_mappings ().get_crate_num (),
				    crate_name);
      rust_assert (ok);
    }

  FunctionCollector collector;
  collector.go (crate);

  for (auto func : collector.get_functions ())
    {
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
    }

  for (auto closure ATTRIBUTE_UNUSED : collector.get_closures ())
    rust_sorry_at (closure->get_locus (),
		   "Closure borrow checking is not implemented yet.");
}

} // namespace HIR
} // namespace Rust