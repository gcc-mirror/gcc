// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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
#include "rust-object-export.h"

#include "md5.h"

namespace Rust {
namespace Metadata {

class ExportContext
{
public:
  ExportContext () : mappings (Analysis::Mappings::get ()) {}

  ~ExportContext () {}

  void push_module_scope (const HIR::Module &module)
  {
    module_stack.push_back (module);
  }

  const HIR::Module &pop_module_scope ()
  {
    rust_assert (!module_stack.empty ());

    const HIR::Module &poped = module_stack.back ();
    module_stack.pop_back ();
    return poped;
  }

  void emit_trait (const HIR::Trait &trait)
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

  void emit_function (const HIR::Function &fn)
  {
    // lookup the AST node for this
    AST::Item *item = nullptr;
    bool ok
      = mappings->lookup_ast_item (fn.get_mappings ().get_nodeid (), &item);
    rust_assert (ok);

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
	for (AST::FunctionParam &param : function.get_function_params ())
	  {
	    std::string name = param.get_pattern ()->as_string ();
	    std::unique_ptr<AST::Type> param_type
	      = param.get_type ()->clone_type ();

	    AST::NamedFunctionParam p (name, std::move (param_type), {},
				       param.get_locus ());
	    function_params.push_back (std::move (p));
	  }

	AST::ExternalItem *external_item = new AST::ExternalFunctionItem (
	  item_name, {} /* generic_params */, std::move (return_type),
	  where_clause, std::move (function_params), false /* has_variadics */,
	  {} /* variadic_outer_attrs */, vis, function.get_outer_attrs (),
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

  const std::string &get_interface_buffer () const
  {
    return public_interface_buffer;
  }

private:
  Analysis::Mappings *mappings;

  std::vector<std::reference_wrapper<const HIR::Module>> module_stack;
  std::string public_interface_buffer;
};

// implicitly by using HIR nodes we know that these have passed CFG expansion
// and they exist in the compilation unit
class ExportVisItems : public HIR::HIRVisItemVisitor
{
public:
  ExportVisItems (ExportContext &context) : ctx (context) {}

  void visit (HIR::Module &module) override {}
  void visit (HIR::ExternCrate &crate) override {}
  void visit (HIR::UseDeclaration &use_decl) override {}
  void visit (HIR::TypeAlias &type_alias) override {}
  void visit (HIR::StructStruct &struct_item) override {}
  void visit (HIR::TupleStruct &tuple_struct) override {}
  void visit (HIR::Enum &enum_item) override {}
  void visit (HIR::Union &union_item) override {}
  void visit (HIR::ConstantItem &const_item) override {}
  void visit (HIR::StaticItem &static_item) override {}
  void visit (HIR::ImplBlock &impl) override {}
  void visit (HIR::ExternBlock &block) override {}

  void visit (HIR::Trait &trait) override { ctx.emit_trait (trait); }

  void visit (HIR::Function &function) override
  {
    ctx.emit_function (function);
  }

private:
  ExportContext &ctx;
};

PublicInterface::PublicInterface (HIR::Crate &crate)
  : crate (crate), mappings (*Analysis::Mappings::get ())
{}

void
PublicInterface::Export (HIR::Crate &crate)
{
  PublicInterface interface (crate);
  interface.go ();
}

void
PublicInterface::go ()
{
  ExportContext context;
  ExportVisItems visitor (context);
  for (auto &item : crate.items)
    {
      bool is_vis_item = item->get_hir_kind () == HIR::Node::BaseKind::VIS_ITEM;
      if (!is_vis_item)
	continue;

      HIR::VisItem &vis_item = static_cast<HIR::VisItem &> (*item.get ());
      if (is_crate_public (vis_item))
	vis_item.accept_vis (visitor);
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

} // namespace Metadata
} // namespace Rust
