// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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

#include "rust-export-metadata.h"
#include "rust-hir-visitor.h"
#include "rust-hir-full.h"
#include "rust-hir-map.h"
#include "rust-ast-dump.h"
#include "rust-abi.h"
#include "rust-item.h"
#include "rust-macro.h"
#include "rust-object-export.h"

#include "md5.h"
#include "rust-system.h"

namespace Rust {
namespace Metadata {

static const std::string extension_path = ".rox";

ExportContext::ExportContext () : mappings (Analysis::Mappings::get ()) {}

ExportContext::~ExportContext () {}

void
ExportContext::push_module_scope (const HIR::Module &module)
{
  module_stack.push_back (module);
}

const HIR::Module &
ExportContext::pop_module_scope ()
{
  rust_assert (!module_stack.empty ());
  const HIR::Module &poped = module_stack.back ();
  module_stack.pop_back ();
  return poped;
}

void
ExportContext::emit_trait (AST::Trait &trait)
{
  std::stringstream oss;
  AST::Dump dumper (oss);
  dumper.process (trait);

  public_interface_buffer += oss.str ();
}

void
ExportContext::emit_use_declaration (AST::UseDeclaration &use_decl)
{
  std::stringstream oss;
  AST::Dump dumper (oss);
  dumper.process (use_decl);

  public_interface_buffer += oss.str ();
}

void
ExportContext::emit_function (AST::Function &fn)
{
  // is this a CFG macro or not
  if (fn.is_marked_for_strip ())
    return;

  // if its a generic function we need to output the full declaration
  // otherwise we can let people link against this

  std::stringstream oss;
  AST::Dump dumper (oss);
  if (!fn.has_generics ())
    {
      std::vector<std::unique_ptr<AST::ExternalItem>> external_items;
      external_items.emplace_back (fn.clone_external_item ());

      AST::ExternBlock extern_block (get_string_from_abi (Rust::ABI::RUST),
				     std::move (external_items),
				     fn.get_visibility (), {}, {},
				     fn.get_locus ());

      dumper.go (extern_block);
    }
  else
    {
      dumper.process (fn);
    }

  // store the dump
  public_interface_buffer += oss.str ();
}

void
ExportContext::emit_extern_block (const AST::ExternBlock &block,
				  std::function<void (void)> sub_visitor)
{
  public_interface_buffer += "extern \"" + block.get_abi () + "\" {\n";
  sub_visitor ();
  public_interface_buffer += "}\n";
}

void
ExportContext::emit_module (const AST::Module &module,
			    std::function<void (void)> sub_visitor)
{
  if (module.get_visibility ().is_public ())
    {
      public_interface_buffer
	+= "pub mod " + module.get_name ().as_string () + "{\n";
      sub_visitor ();
      public_interface_buffer += "}\n";
    }
}

void
ExportContext::emit_macro (AST::MacroRulesDefinition &macro)
{
  std::stringstream oss;
  AST::Dump dumper (oss);

  dumper.go (macro);

  public_interface_buffer += oss.str ();
}

const std::string &
ExportContext::get_interface_buffer () const
{
  return public_interface_buffer;
}

// implicitly by using HIR nodes we know that these have passed CFG expansion
// and they exist in the compilation unit
class ExportVisItems : public AST::DefaultASTVisitor
{
public:
  using AST::DefaultASTVisitor::visit;
  ExportVisItems (ExportContext &context) : ctx (context) {}

  void go (AST::Crate &c) { visit (c); }

  void visit (AST::Function &function) override
  {
    ctx.emit_function (function);
  }
  void visit (AST::ExternBlock &block) override
  {
    auto sub_visitor = [&] () { AST::DefaultASTVisitor::visit (block); };
    ctx.emit_extern_block (block, sub_visitor);
  }
  void visit (AST::Trait &trait) override { ctx.emit_trait (trait); }
  void visit (AST::Module &module) override
  {
    auto sub_visitor = [&] () { AST::DefaultASTVisitor::visit (module); };
    ctx.emit_module (module, sub_visitor);
  }

  void visit (AST::UseDeclaration &use_decl) override
  {
    ctx.emit_use_declaration (use_decl);
  }

private:
  ExportContext &ctx;
};

PublicInterface::PublicInterface (HIR::Crate &crate)
  : crate (crate), mappings (Analysis::Mappings::get ()), context ()
{}

void
PublicInterface::Export (HIR::Crate &crate)
{
  PublicInterface interface (crate);
  interface.gather_export_data ();
  interface.write_to_object_file ();
}

void
PublicInterface::ExportTo (HIR::Crate &crate, const std::string &output_path)
{
  PublicInterface interface (crate);
  interface.gather_export_data ();
  interface.write_to_path (output_path);
}

void
PublicInterface::gather_export_data ()
{
  ExportVisItems visitor (context);
  auto crate_num
    = mappings.lookup_crate_num (crate.get_mappings ().get_nodeid ());
  auto &ast_crate = mappings.get_ast_crate (crate_num.value ());
  visitor.go (ast_crate);

  for (auto &macro : mappings.get_exported_macros ())
    context.emit_macro (macro);
}

void
PublicInterface::write_to_object_file () const
{
  // done
  const auto &buf = context.get_interface_buffer ();
  std::string size_buffer = std::to_string (buf.size ());

  // md5 this
  struct md5_ctx chksm;
  unsigned char checksum[16];

  md5_init_ctx (&chksm);
  md5_process_bytes (buf.c_str (), buf.size (), &chksm);
  md5_finish_ctx (&chksm, checksum);

  // MAGIC MD5 DLIM  DLIM buffer-size DELIM contents
  const std::string current_crate_name = mappings.get_current_crate_name ();

  // extern void
  rust_write_export_data (kMagicHeader, sizeof (kMagicHeader));
  rust_write_export_data ((const char *) checksum, sizeof (checksum));
  rust_write_export_data (kSzDelim, sizeof (kSzDelim));
  rust_write_export_data (current_crate_name.c_str (),
			  current_crate_name.size ());
  rust_write_export_data (kSzDelim, sizeof (kSzDelim));
  rust_write_export_data (size_buffer.c_str (), size_buffer.size ());
  rust_write_export_data (kSzDelim, sizeof (kSzDelim));
  rust_write_export_data (buf.c_str (), buf.size ());
}

void
PublicInterface::write_to_path (const std::string &path) const
{
  // validate path contains correct extension
  const std::string expected_file_name = expected_metadata_filename ();
  const char *path_base_name = lbasename (path.c_str ());
  if (strcmp (path_base_name, expected_file_name.c_str ()) != 0)
    {
      rust_error_at (UNDEF_LOCATION,
		     "expected metadata-output path to have base file name of: "
		     "%qs got %qs",
		     expected_file_name.c_str (), path_base_name);
      return;
    }

  // done
  const auto &buf = context.get_interface_buffer ();
  std::string size_buffer = std::to_string (buf.size ());

  // md5 this
  struct md5_ctx chksm;
  unsigned char checksum[16];

  md5_init_ctx (&chksm);
  md5_process_bytes (buf.c_str (), buf.size (), &chksm);
  md5_finish_ctx (&chksm, checksum);

  // MAGIC MD5 DLIM  DLIM buffer-size DELIM contents
  const std::string current_crate_name = mappings.get_current_crate_name ();

  // write to path
  FILE *nfd = fopen (path.c_str (), "wb");
  if (nfd == NULL)
    {
      rust_error_at (UNDEF_LOCATION, "failed to open file %qs for writing: %s",
		     path.c_str (), xstrerror (errno));
      return;
    }

  // write data
  if (fwrite (kMagicHeader, sizeof (kMagicHeader), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (checksum, sizeof (checksum), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (current_crate_name.c_str (), current_crate_name.size (), 1, nfd)
      < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (size_buffer.c_str (), size_buffer.size (), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (!buf.empty ())
    if (fwrite (buf.c_str (), buf.size (), 1, nfd) < 1)
      {
	rust_error_at (UNDEF_LOCATION, "failed to write to file %qs: %s",
		       path.c_str (), xstrerror (errno));
	fclose (nfd);
	return;
      }

  // done
  fclose (nfd);
}

bool
PublicInterface::is_crate_public (const HIR::VisItem &item)
{
  const HIR::Visibility &visibility = item.get_visibility ();

  bool is_public
    = visibility.get_vis_type () == HIR::Visibility::VisType::Public;
  bool has_path = !visibility.get_path ().is_error ();

  // FIXME this might be pub(crate)
  // Arthur magic required here

  return is_public && !has_path;
}

std::string
PublicInterface::expected_metadata_filename ()
{
  auto &mappings = Analysis::Mappings::get ();

  const std::string current_crate_name = mappings.get_current_crate_name ();
  return current_crate_name + extension_path;
}

} // namespace Metadata
} // namespace Rust
