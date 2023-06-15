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
#include "rust-lex.h"
#include "rust-token-converter.h"
#ifndef _WIN32
#include <dlfcn.h>
#endif

namespace Rust {

const std::string PROC_MACRO_DECL_PREFIX = "__gccrs_proc_macro_decls_";

ProcMacro::TokenStream
tokenstream_from_string (std::string &data, bool &lex_error)
{
  // FIXME: Insert location pointing to call site in tokens
  Lexer lex (data);

  std::vector<const_TokenPtr> tokens;
  TokenPtr ptr;
  for (ptr = lex.build_token ();
       ptr != nullptr && ptr->get_id () != END_OF_FILE;
       ptr = lex.build_token ())
    {
      tokens.emplace_back (ptr);
    }

  if (ptr == nullptr)
    {
      lex_error = true;
      return ProcMacro::TokenStream::make_tokenstream ();
    }

  lex_error = false;
  return convert (tokens);
}

static_assert (
  std::is_same<decltype (tokenstream_from_string) *,
	       ProcMacro::from_str_function_t>::value,
  "Registration callback signature not synced, check proc macro internals.");

template <typename Symbol, typename Callback>
bool
register_callback (void *handle, Symbol, std::string symbol_name,
		   Callback callback)
{
  void *addr = dlsym (handle, symbol_name.c_str ());
  if (addr == nullptr)
    {
      rust_error_at (Location (),
		     "Callback registration symbol (%s) missing from "
		     "proc macro, wrong version?",
		     symbol_name.c_str ());
      return false;
    }

  auto storage = reinterpret_cast<Symbol *> (addr);
  *storage = callback;

  return true;
}

#define REGISTER_CALLBACK(HANDLE, SYMBOL, CALLBACK)                            \
  register_callback (HANDLE, SYMBOL, #SYMBOL, CALLBACK)

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

  if (!REGISTER_CALLBACK (handle, __gccrs_pm_callback_from_str_fn,
			  tokenstream_from_string))
    return nullptr;

  // FIXME: Add CrateStableId handling, right now all versions may be loaded,
  // even incompatible ones.
  return *reinterpret_cast<const ProcMacro::ProcmacroArray **> (
    dlsym (handle, PROC_MACRO_DECL_PREFIX.c_str ()));
#else
  rust_sorry_at (UNDEF_LOCATION,
		 "Procedural macros are not yet supported on windows host");
  rust_unreachable ();
#endif
}

#undef REGISTER_CALLBACK

const std::vector<ProcMacro::Procmacro>
load_macros (std::string path)
{
  const ProcMacro::ProcmacroArray *array = load_macros_array (path);
  // Did not load the proc macro
  if (array == nullptr)
    return {};

  rust_debug ("Found %lu procedural macros", array->length);

  return std::vector<ProcMacro::Procmacro> (array->macros,
					    array->macros + array->length);
}

} // namespace Rust
