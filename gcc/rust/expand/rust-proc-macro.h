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

#include "libproc_macro_internal/proc_macro.h"
#include "rust-mapping-common.h"

namespace Rust {

class BangProcMacro
{
private:
  std::string name;
  NodeId node_id;
  ProcMacro::BangMacro macro;

public:
  BangProcMacro (ProcMacro::Bang macro);
  BangProcMacro () = default;

  const std::string &get_name () const { return name; }

  NodeId get_node_id () const { return node_id; }

  ProcMacro::BangMacro get_handle () const { return macro; }
};

class AttributeProcMacro
{
private:
  std::string name;
  NodeId node_id;
  ProcMacro::AttributeMacro macro;

public:
  AttributeProcMacro (ProcMacro::Attribute macro);
  AttributeProcMacro () = default;

  const std::string &get_name () const { return name; }

  NodeId get_node_id () const { return node_id; }

  ProcMacro::AttributeMacro get_handle () const { return macro; }
};

class CustomDeriveProcMacro
{
private:
  std::string trait_name;
  std::vector<std::string> attributes;
  NodeId node_id;
  ProcMacro::CustomDeriveMacro macro;

public:
  CustomDeriveProcMacro (ProcMacro::CustomDerive macro);
  CustomDeriveProcMacro () = default;

  const std::string &get_name () const { return trait_name; }

  NodeId get_node_id () const { return node_id; }

  ProcMacro::CustomDeriveMacro get_handle () const { return macro; }
};

/**
 * Load a procedural macro library and collect its entrypoints.
 *
 * @param The path to the shared object file to load.
 */
const std::vector<ProcMacro::Procmacro>
load_macros (std::string path);

std::string
generate_proc_macro_decls_symbol (std::uint32_t stable_crate_id);

} // namespace Rust

#endif /* ! RUST_PROC_MACRO_H */
