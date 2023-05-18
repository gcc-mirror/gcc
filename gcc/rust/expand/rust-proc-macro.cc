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

#include "rust-diagnostics.h"
#include "rust-proc-macro.h"
#ifndef _WIN32
#include <dlfcn.h>
#endif

namespace Rust {

const std::string PROC_MACRO_DECL_PREFIX = "__gccrs_proc_macro_decls_";

const ProcMacro::ProcmacroArray *
load_macros_array (std::string path)
{
#ifndef _WIN32
  void *handle = dlopen (path.c_str (), RTLD_LAZY | RTLD_LOCAL);
  // We're leaking the handle since we can't ever unload it
  if (!handle)
    {
      rust_debug ("Error whilst opening procedural macro: %s", dlerror ());
      return nullptr;
    }

  // FIXME: Add CrateStableId handling, right now all versions may be loaded,
  // even incompatible ones.
  return *reinterpret_cast<const ProcMacro::ProcmacroArray **> (
    dlsym (handle, PROC_MACRO_DECL_PREFIX.c_str ()));
#else
  rust_sorry_at (Location (),
		 "Procedural macros are not yet supported on windows host");
  gcc_unreachable ();
#endif
}

const std::vector<ProcMacro::Procmacro>
load_macros (std::string path)
{
  const ProcMacro::ProcmacroArray *array = load_macros_array (path);
  // Did not load the proc macro
  if (array == nullptr)
    gcc_unreachable ();

  rust_debug ("Found %lu procedural macros", array->length);

  return std::vector<ProcMacro::Procmacro> (array->macros,
					    array->macros + array->length);
}

void
ProcMacroExpander::import_proc_macros (std::string extern_crate)
{
  auto path = session.extern_crates.find (extern_crate);
  if (path == session.extern_crates.end ())
    {
      // Extern crate path is not available.
      // FIXME: Emit error
      rust_error_at (Location (), "Cannot find requested proc macro crate");
      gcc_unreachable ();
    }
  auto macros = load_macros (path->second);

  std::string prefix = extern_crate + "::";
  for (auto &macro : macros)
    {
      switch (macro.tag)
	{
	case ProcMacro::CUSTOM_DERIVE:
	  rust_debug ("Found one derive proc macro.");
	  mappings->insert_derive_proc_macro (
	    std::make_pair (extern_crate,
			    macro.payload.custom_derive.trait_name),
	    macro.payload.custom_derive);
	  break;
	case ProcMacro::ATTR:
	  rust_debug ("Found one attribute proc macro.");
	  mappings->insert_attribute_proc_macro (
	    std::make_pair (extern_crate, macro.payload.attribute.name),
	    macro.payload.attribute);
	  break;
	case ProcMacro::BANG:
	  rust_debug ("Found one bang proc macro.");
	  mappings->insert_bang_proc_macro (
	    std::make_pair (extern_crate, macro.payload.bang.name),
	    macro.payload.bang);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
}

} // namespace Rust
