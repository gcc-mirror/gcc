// Copyright (C) 2020-2024 Free Software Foundation, Inc.

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
ExportContext::emit_trait (const HIR::Trait &trait)
{
  // lookup the AST node for this
  AST::Item *item = nullptr;
  bool ok
    = mappings->lookup_ast_item (trait.get_mappings ().get_nodeid (), &item);
  rust_assert (ok);

  std::stringstream oss;
  AST::Dump dumper (oss);
  dumper.go (*item);

  public_interface_buffer += oss.str ();
}

void
ExportContext::emit_function (const HIR::Function &fn)
{
  // lookup the AST node for this
  AST::Item *item = nullptr;
  bool ok = mappings->lookup_ast_item (fn.get_mappings ().get_nodeid (), &item);
  rust_assert (ok);

  // is this a CFG macro or not
  if (item->is_marked_for_strip ())
    return;

  // FIXME add assertion that item must be a vis_item;
  AST::VisItem &vis_item = static_cast<AST::VisItem &> (*item);

  // if its a generic function we need to output the full declaration
  // otherwise we can let people link against this

  std::stringstream oss;
  AST::Dump dumper (oss);
  if (!fn.has_generics ())
    {
      // FIXME assert that this is actually an AST::Function
      AST::Function &function = static_cast<AST::Function &> (vis_item);

      // we can emit an extern block with abi of "rust"
      Identifier item_name = function.get_function_name ();

      // always empty for extern linkage
      AST::WhereClause where_clause = AST::WhereClause::create_empty ();
      std::vector<std::unique_ptr<AST::GenericParam>> generic_params;

      AST::Visibility vis = function.get_visibility ();
      std::unique_ptr<AST::Type> return_type
	= std::unique_ptr<AST::Type> (nullptr);
      if (function.has_return_type ())
	{
	  return_type = function.get_return_type ()->clone_type ();
	}

      std::vector<AST::NamedFunctionParam> function_params;
      for (auto &p : function.get_function_params ())
	{
	  if (p->is_variadic () || p->is_self ())
	    rust_unreachable ();
	  auto param = static_cast<AST::FunctionParam *> (p.get ());
	  std::string name = param->get_pattern ()->as_string ();
	  std::unique_ptr<AST::Type> param_type
	    = param->get_type ()->clone_type ();

	  AST::NamedFunctionParam np (name, std::move (param_type), {},
				      param->get_locus ());
	  function_params.push_back (std::move (np));
	}

      AST::ExternalItem *external_item
	= new AST::ExternalFunctionItem (item_name, {} /* generic_params */,
					 std::move (return_type), where_clause,
					 std::move (function_params), vis,
					 function.get_outer_attrs (),
					 function.get_locus ());

      std::vector<std::unique_ptr<AST::ExternalItem>> external_items;
      external_items.push_back (
	std::unique_ptr<AST::ExternalItem> (external_item));

      AST::ExternBlock extern_block (get_string_from_abi (Rust::ABI::RUST),
				     std::move (external_items),
				     vis_item.get_visibility (), {}, {},
				     fn.get_locus ());

      dumper.go (extern_block);
    }
  else
    {
      dumper.go (*item);
    }

  // store the dump
  public_interface_buffer += oss.str ();
}

void
ExportContext::emit_macro (NodeId macro)
{
  std::stringstream oss;
  AST::Dump dumper (oss);

  AST::Item *item;
  auto ok = mappings->lookup_ast_item (macro, &item);
  rust_assert (ok);

  dumper.go (*item);

  public_interface_buffer += oss.str ();
}

const std::string &
ExportContext::get_interface_buffer () const
{
  return public_interface_buffer;
}

// implicitly by using HIR nodes we know that these have passed CFG expansion
// and they exist in the compilation unit
class ExportVisItems : public HIR::HIRVisItemVisitor
{
public:
  ExportVisItems (ExportContext &context) : ctx (context) {}

  void visit (HIR::Module &) override {}
  void visit (HIR::ExternCrate &) override {}
  void visit (HIR::UseDeclaration &) override {}
  void visit (HIR::TypeAlias &) override {}
  void visit (HIR::StructStruct &) override {}
  void visit (HIR::TupleStruct &) override {}
  void visit (HIR::Enum &) override {}
  void visit (HIR::Union &) override {}
  void visit (HIR::ConstantItem &) override {}
  void visit (HIR::StaticItem &) override {}
  void visit (HIR::ImplBlock &) override {}
  void visit (HIR::ExternBlock &) override {}

  void visit (HIR::Trait &trait) override { ctx.emit_trait (trait); }

  void visit (HIR::Function &function) override
  {
    ctx.emit_function (function);
  }

private:
  ExportContext &ctx;
};

PublicInterface::PublicInterface (HIR::Crate &crate)
  : crate (crate), mappings (*Analysis::Mappings::get ()), context ()
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
  for (auto &item : crate.get_items ())
    {
      bool is_vis_item = item->get_hir_kind () == HIR::Node::BaseKind::VIS_ITEM;
      if (!is_vis_item)
	continue;

      HIR::VisItem &vis_item = static_cast<HIR::VisItem &> (*item.get ());
      if (is_crate_public (vis_item))
	vis_item.accept_vis (visitor);
    }

  for (const auto &macro : mappings.get_exported_macros ())
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
  const char *path_base_name = basename (path.c_str ());
  if (strcmp (path_base_name, expected_file_name.c_str ()) != 0)
    {
      rust_error_at (UNDEF_LOCATION,
		     "expected metadata-output path to have base file name of: "
		     "%<%s%> got %<%s%>",
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
      rust_error_at (UNDEF_LOCATION,
		     "failed to open file %<%s%> for writing: %s",
		     path.c_str (), xstrerror (errno));
      return;
    }

  // write data
  if (fwrite (kMagicHeader, sizeof (kMagicHeader), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (checksum, sizeof (checksum), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (current_crate_name.c_str (), current_crate_name.size (), 1, nfd)
      < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (size_buffer.c_str (), size_buffer.size (), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (fwrite (kSzDelim, sizeof (kSzDelim), 1, nfd) < 1)
    {
      rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
		     path.c_str (), xstrerror (errno));
      fclose (nfd);
      return;
    }

  if (!buf.empty ())
    if (fwrite (buf.c_str (), buf.size (), 1, nfd) < 1)
      {
	rust_error_at (UNDEF_LOCATION, "failed to write to file %<%s%>: %s",
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
    = visibility.get_vis_type () == HIR::Visibility::VisType::PUBLIC;
  bool has_path = !visibility.get_path ().is_error ();

  // FIXME this might be pub(crate)
  // Arthur magic required here

  return is_public && !has_path;
}

std::string
PublicInterface::expected_metadata_filename ()
{
  auto mappings = Analysis::Mappings::get ();

  const std::string current_crate_name = mappings->get_current_crate_name ();
  return current_crate_name + extension_path;
}

} // namespace Metadata
} // namespace Rust
