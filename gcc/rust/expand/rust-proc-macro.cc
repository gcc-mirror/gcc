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

#include "rust-system.h"
#include "rust-diagnostics.h"
#include "rust-proc-macro.h"
#include "rust-session-manager.h"
#include "rust-lex.h"
#include "rust-token-converter.h"
#include "rust-attributes.h"

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <dlfcn.h>
#endif

namespace Rust {

BangProcMacro::BangProcMacro (ProcMacro::Bang macro)
  : name (macro.name), node_id (Analysis::Mappings::get ().get_next_node_id ()),
    macro (macro.macro)
{}

AttributeProcMacro::AttributeProcMacro (ProcMacro::Attribute macro)
  : name (macro.name), node_id (Analysis::Mappings::get ().get_next_node_id ()),
    macro (macro.macro)
{}

CustomDeriveProcMacro::CustomDeriveProcMacro (ProcMacro::CustomDerive macro)
  : trait_name (macro.trait_name),
    attributes (macro.attributes, macro.attributes + macro.attr_size),
    node_id (Analysis::Mappings::get ().get_next_node_id ()),
    macro (macro.macro)
{}

namespace {

ProcMacro::Literal
literal_from_string (const std::string &data, bool &error)
{
  Lexer lex (data, nullptr);
  const_TokenPtr output = lex.build_token ();
  if (output == nullptr || !output->is_literal ())
    {
      error = true;
      // We should probably rework this
      return ProcMacro::Literal::make_usize (0);
    }

  error = false;
  return convert_literal (output);
}

ProcMacro::TokenStream
tokenstream_from_string (std::string &data, bool &lex_error)
{
  // FIXME: Insert location pointing to call site in tokens
  Lexer lex (data, Session::get_instance ().linemap);

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
	       ProcMacro::ts_from_str_fn_t>::value,
  "Registration callback signature not synced, check proc macro internals.");

static_assert (
  std::is_same<decltype (literal_from_string) *,
	       ProcMacro::lit_from_str_fn_t>::value,
  "Registration callback signature not synced, check proc macro internals.");

} // namespace

template <typename Handle, typename Symbol, typename Callback>
bool
register_callback (Handle handle, Symbol, std::string symbol_name,
		   Callback callback)
{
#ifdef _WIN32
  FARPROC addr = GetProcAddress (handle, symbol_name.c_str ());
#else
  void *addr = dlsym (handle, symbol_name.c_str ());
#endif
  if (addr == nullptr)
    {
      rust_error_at (UNDEF_LOCATION,
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
#ifdef _WIN32
  HMODULE handle = LoadLibraryA (path.c_str ());
  // We're leaking the handle since we can't ever unload it
  if (!handle)
    {
      char msg[300];
      FormatMessageA (FORMAT_MESSAGE_FROM_SYSTEM
			| FORMAT_MESSAGE_IGNORE_INSERTS,
		      nullptr, GetLastError (), 0, msg, sizeof msg, nullptr);
      rust_debug ("Error whilst opening procedural macro: %s", msg);
      return nullptr;
    }
#else
  void *handle = dlopen (path.c_str (), RTLD_LAZY | RTLD_LOCAL);
  // We're leaking the handle since we can't ever unload it
  if (!handle)
    {
      rust_debug ("Error whilst opening procedural macro: %s", dlerror ());
      return nullptr;
    }
#endif

  if (!REGISTER_CALLBACK (handle, __gccrs_proc_macro_ts_from_str_,
			  tokenstream_from_string))
    return nullptr;
  if (!REGISTER_CALLBACK (handle, __gccrs_proc_macro_lit_from_str_,
			  literal_from_string))
    return nullptr;
  if (!REGISTER_CALLBACK (handle, __gccrs_proc_macro_is_available_,
			  ProcMacro::BridgeState::Available))
    return nullptr;

  // FIXME: Add CrateStableId handling, right now all versions may be loaded,
  // even incompatible ones.
  auto symbol_name = generate_proc_macro_decls_symbol (0 /* FIXME */);

  return *reinterpret_cast<const ProcMacro::ProcmacroArray **> (
#ifdef _WIN32
    GetProcAddress (handle, symbol_name.c_str ())
#else
    dlsym (handle, symbol_name.c_str ())
#endif
  );
}

#undef REGISTER_CALLBACK

const std::vector<ProcMacro::Procmacro>
load_macros (std::string path)
{
  const ProcMacro::ProcmacroArray *array = load_macros_array (path);
  // Did not load the proc macro
  if (array == nullptr)
    return {};

  rust_debug ("Found %lu procedural macros", (unsigned long) array->length);

  return std::vector<ProcMacro::Procmacro> (array->macros,
					    array->macros + array->length);
}

std::string
generate_proc_macro_decls_symbol (std::uint32_t stable_crate_id)
{
  std::ostringstream stream;
  stream << "__gccrs_proc_macro_decls_" << std::setfill ('0') << std::hex
	 << std::setw (8) << stable_crate_id << "__";

  return stream.str ();
}

} // namespace Rust
