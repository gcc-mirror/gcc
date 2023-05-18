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

#ifndef RUST_PROC_MACRO_H
#define RUST_PROC_MACRO_H

#include <string>
#include "rust-hir-map.h"
#include "rust-name-resolver.h"
#include "rust-session-manager.h"
#include "rust-ast.h"
#include "rust-ast-collector.h"
#include "rust-token-converter.h"
#include "libproc_macro/proc_macro.h"

namespace Rust {

/**
 * Load a procedural macro library and return a pointer to it's entrypoint.
 *
 * @param The path to the shared object file to load.
 */
const std::vector<ProcMacro::Procmacro>
load_macros (std::string path);

class ProcMacroExpander
{
public:
  ProcMacroExpander (Session &session)
    : session (session), has_changed_flag (false),
      resolver (Resolver::Resolver::get ()),
      mappings (Analysis::Mappings::get ())

  {}

  ~ProcMacroExpander () = default;

  void import_proc_macros (std::string extern_crate);

  template <typename T>
  void expand_derive_proc_macro (T &item, std::string &trait_name)
  {}

  template <typename T>
  void expand_bang_proc_macro (T &item, AST::SimplePath &path)
  {}

  template <typename T>
  void expand_attribute_proc_macro (T &item, AST::SimplePath &path)
  {
    ProcMacro::Attribute macro;

    std::string crate = path.get_segments ()[0].get_segment_name ();
    std::string name = path.get_segments ()[1].get_segment_name ();
    if (!mappings->lookup_attribute_proc_macro (std::make_pair (crate, name),
						macro))
      {
	// FIXME: Resolve this path segment instead of taking it directly.
	import_proc_macros (crate);
      }

    if (!mappings->lookup_attribute_proc_macro (std::make_pair (crate, name),
						macro))
      {
	rust_error_at (Location (), "procedural macro %s not found",
		       name.c_str ());
	rust_assert (false);
      }
    // FIXME: Attach result back to the ast
    std::vector<TokenPtr> tokens;
    AST::TokenCollector collector (tokens);

    collector.visit (item);

    std::vector<const_TokenPtr> vec;
    for (auto i : collector.collect_tokens ())
      {
	vec.push_back (std::const_pointer_cast<Token> (i));
      }

    // FIXME: Handle attributes
    macro.macro (ProcMacro::TokenStream::make_tokenstream (), convert (vec));
  }

  bool has_changed () const { return has_changed_flag; }

  void reset_changed_state () { has_changed_flag = false; }

private:
  Session &session;
  bool has_changed_flag;

public:
  Resolver::Resolver *resolver;
  Analysis::Mappings *mappings;
};

} // namespace Rust

#endif /* ! RUST_PROC_MACRO_H */
