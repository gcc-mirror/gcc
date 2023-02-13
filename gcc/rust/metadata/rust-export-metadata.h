// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#ifndef RUST_EXPORT_METADATA_H
#define RUST_EXPORT_METADATA_H

#include "rust-system.h"
#include "rust-hir-full-decls.h"
#include "rust-hir-map.h"

namespace Rust {
namespace Metadata {

static const char kMagicHeader[4] = {'G', 'R', 'S', 'T'};
static const char kSzDelim[1] = {'$'};

class ExportContext
{
public:
  ExportContext ();

  ~ExportContext ();

  void push_module_scope (const HIR::Module &module);

  const HIR::Module &pop_module_scope ();

  void emit_trait (const HIR::Trait &trait);

  void emit_function (const HIR::Function &fn);

  const std::string &get_interface_buffer () const;

private:
  Analysis::Mappings *mappings;

  std::vector<std::reference_wrapper<const HIR::Module>> module_stack;
  std::string public_interface_buffer;
};

class PublicInterface
{
public:
  static void Export (HIR::Crate &crate);

  static void ExportTo (HIR::Crate &crate, const std::string &output_path);

  static bool is_crate_public (const HIR::VisItem &item);

  static std::string expected_metadata_filename ();

protected:
  void gather_export_data ();

  void write_to_object_file () const;

  void write_to_path (const std::string &path) const;

private:
  PublicInterface (HIR::Crate &crate);

  HIR::Crate &crate;
  Analysis::Mappings &mappings;
  ExportContext context;
};

} // namespace Metadata
} // namespace Rust

#endif // RUST_EXPORT_METADATA_H
