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
  rust_sorry_at (UNDEF_LOCATION,
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

} // namespace Rust
