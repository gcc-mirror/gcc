// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-feature-gate.h"
#include "rust-abi.h"
#include "rust-attribute-values.h"
#include "rust-ast-visitor.h"
#include "rust-feature.h"
#include "rust-ast-full.h"

namespace Rust {

void
FeatureGate::check (AST::Crate &crate)
{
  visit (crate);
}

void
FeatureGate::visit (AST::Crate &crate)
{
  valid_lang_features.clear ();
  valid_lib_features.clear ();

  // avoid clearing defined features (?)

  for (const auto &attr : crate.inner_attrs)
    {
      if (attr.get_path ().as_string () == "feature")
	{
	  // check for empty feature, such as `#![feature], this is an error
	  if (attr.empty_input ())
	    {
	      rust_error_at (attr.get_locus (), ErrorCode::E0556,
			     "malformed %<feature%> attribute input");
	      continue;
	    }
	  const auto &attr_input = attr.get_attr_input ();
	  auto type = attr_input.get_attr_input_type ();
	  if (type == AST::AttrInput::AttrInputType::TOKEN_TREE)
	    {
	      const auto &option = static_cast<const AST::DelimTokenTree &> (
		attr.get_attr_input ());
	      std::unique_ptr<AST::AttrInputMetaItemContainer> meta_item (
		option.parse_to_meta_item ());
	      for (const auto &item : meta_item->get_items ())
		{
		  const auto &name_str = item->as_string ();

		  // TODO: detect duplicates
		  if (auto tname = Feature::as_name (name_str))
		    valid_lang_features.insert (tname.value ());
		  else
		    valid_lib_features.emplace (name_str, item->get_locus ());
		}
	    }
	}
    }

  AST::DefaultASTVisitor::visit (crate);

  for (auto &ent : valid_lib_features)
    {
      const std::string &feature = ent.first;
      location_t locus = ent.second;

      // rustc treats these as valid,
      // but apparently has special handling for them
      if (feature == "libc" || feature == "test")
	continue;

      if (defined_lib_features.find (feature) != defined_lib_features.end ())
	{
	  // TODO: emit warning if stable
	  continue;
	}

      rust_error_at (locus, ErrorCode::E0635, "unknown feature %qs",
		     feature.c_str ());
    }
}

void
FeatureGate::gate (Feature::Name name, location_t loc,
		   const std::string &error_msg)
{
  if (!valid_lang_features.count (name))
    {
      auto &feature = Feature::lookup (name);
      if (auto issue = feature.issue ())
	{
	  auto issue_number = issue.value ();
	  const char *fmt_str
	    = "%s. see issue %u "
	      "<https://github.com/rust-lang/rust/issues/%u> for more "
	      "information. add `#![feature(%s)]` to the crate attributes to "
	      "enable.";
	  rust_error_at (loc, ErrorCode::E0658, fmt_str, error_msg.c_str (),
			 issue_number, issue_number,
			 feature.as_string ().c_str ());
	}
      else
	{
	  const char *fmt_str
	    = "%s. add `#![feature(%s)]` to the crate attributes to enable.";
	  rust_error_at (loc, ErrorCode::E0658, fmt_str, error_msg.c_str (),
			 feature.as_string ().c_str ());
	}
    }
}

void
FeatureGate::visit (AST::ExternBlock &block)
{
  if (block.has_abi ())
    {
      const auto abi = block.get_abi ();

      if (get_abi_from_string (abi) == ABI::INTRINSIC)
	gate (Feature::Name::INTRINSICS, block.get_locus (),
	      "intrinsics are subject to change");
    }
  AST::DefaultASTVisitor::visit (block);
}

void
FeatureGate::check_rustc_attri (const std::vector<AST::Attribute> &attributes)
{
  for (const AST::Attribute &attr : attributes)
    {
      auto name = attr.get_path ().as_string ();
      if (name.rfind ("rustc_", 0) == 0)
	{
	  gate (Feature::Name::RUSTC_ATTRS, attr.get_locus (),
		"internal implementation detail");
	}
    }
}

void
FeatureGate::check_may_dangle_attribute (
  const std::vector<AST::Attribute> &attributes)
{
  for (const AST::Attribute &attr : attributes)
    {
      if (attr.get_path ().as_string () == Values::Attributes::MAY_DANGLE)
	gate (Feature::Name::DROPCK_EYEPATCH, attr.get_locus (),
	      "`may_dangle` has unstable semantics and may be removed in the "
	      "future");
    }
}

void
FeatureGate::check_lang_item_attribute (
  const std::vector<AST::Attribute> &attributes)
{
  for (const AST::Attribute &attr : attributes)
    {
      const auto &str_path = attr.get_path ().as_string ();
      bool is_lang_item = str_path == Values::Attributes::LANG
			  && attr.has_attr_input ()
			  && attr.get_attr_input ().get_attr_input_type ()
			       == AST::AttrInput::AttrInputType::LITERAL;

      if (is_lang_item)
	gate (Feature::Name::LANG_ITEMS, attr.get_locus (),
	      "lang items are subject to change");
    }
}

void
FeatureGate::note_stability_attribute (
  const std::vector<AST::Attribute> &attributes)
{
  for (const AST::Attribute &attr : attributes)
    {
      std::string attr_name = attr.get_path ().as_string ();

      Stability stability;

      if (attr_name == Values::Attributes::STABLE)
	stability = Stability::STABLE;
      else if (attr_name == Values::Attributes::UNSTABLE)
	stability = Stability::UNSTABLE;
      else if (attr_name == Values::Attributes::RUSTC_CONST_STABLE)
	stability = Stability::STABLE;
      else if (attr_name == Values::Attributes::RUSTC_CONST_UNSTABLE)
	stability = Stability::UNSTABLE;
      else
	continue;

      if (attr.empty_input ())
	// TODO: error?
	continue;

      auto &attr_input = attr.get_attr_input ();
      if (attr_input.get_attr_input_type ()
	  != AST::AttrInput::AttrInputType::TOKEN_TREE)
	// TODO: error?
	continue;

      std::unique_ptr<AST::AttrInputMetaItemContainer> meta_item (
	static_cast<const AST::DelimTokenTree &> (attr_input)
	  .parse_to_meta_item ());

      for (auto &item : meta_item->get_items ())
	{
	  // TODO: more thorough error checking?
	  // ~only the standard libraries should ever exercise this
	  if (item->is_key_value_pair ())
	    {
	      auto &pair = static_cast<const AST::MetaNameValueStr &> (*item);
	      if (pair.get_name ().as_string () == "feature")
		defined_lib_features.emplace (pair.get_value (), stability);
	    }
	}
    }
}

void
FeatureGate::visit (AST::MacroRulesDefinition &rules_def)
{
  check_rustc_attri (rules_def.get_outer_attrs ());
  note_stability_attribute (rules_def.get_outer_attrs ());
}

void
FeatureGate::visit (AST::Function &function)
{
  if (!function.is_external ())
    check_rustc_attri (function.get_outer_attrs ());

  check_lang_item_attribute (function.get_outer_attrs ());

  note_stability_attribute (function.get_outer_attrs ());

  AST::DefaultASTVisitor::visit (function);
}

void
FeatureGate::visit (AST::ExternalTypeItem &item)
{
  // TODO(mxlol233): The gating needs a complete visiting chain to activate
  // `AST::ExternalTypeItem`.
  gate (Feature::Name::EXTERN_TYPES, item.get_locus (),
	"extern types are experimental");
}

void
FeatureGate::visit (AST::TraitImpl &impl)
{
  if (impl.is_exclam ())
    gate (Feature::Name::NEGATIVE_IMPLS, impl.get_locus (),
	  "negative_impls are not yet implemented");

  AST::DefaultASTVisitor::visit (impl);
}

void
FeatureGate::visit (AST::Trait &trait)
{
  if (trait.is_auto ())
    gate (Feature::Name::OPTIN_BUILTIN_TRAITS, trait.get_locus (),
	  "auto traits are experimental and possibly buggy");
  check_lang_item_attribute (trait.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (trait);
}

void
FeatureGate::visit (AST::BoxExpr &expr)
{
  gate (
    Feature::Name::BOX_SYNTAX, expr.get_locus (),
    "box expression syntax is experimental; you can call `Box::new` instead");
  AST::DefaultASTVisitor::visit (expr);
}

void
FeatureGate::visit (AST::LifetimeParam &lifetime_param)
{
  check_may_dangle_attribute (lifetime_param.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (lifetime_param);
}

void
FeatureGate::visit (AST::ConstGenericParam &const_param)
{
  check_may_dangle_attribute (const_param.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (const_param);
}

void
FeatureGate::visit (AST::TypeParam &param)
{
  check_may_dangle_attribute (param.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (param);
}

void
FeatureGate::visit (AST::BorrowExpr &expr)
{
  if (expr.is_raw_borrow ())
    gate (Feature::Name::RAW_REF_OP, expr.get_locus (),
	  "raw address of syntax is experimental");
}

void
FeatureGate::visit (AST::RangePattern &pattern)
{
  if (pattern.get_range_kind () == AST::RangeKind::EXCLUDED)
    gate (Feature::Name::EXCLUSIVE_RANGE_PATTERN, pattern.get_locus (),
	  "exclusive range pattern syntax is experimental");
}

void
FeatureGate::visit (AST::UseTreeGlob &use)
{
  // At the moment, UseTrees do not have outer attributes, but they should. we
  // need to eventually gate `#[prelude_import]` on use-trees based on the
  // #[feature(prelude_import)]
}

void
FeatureGate::visit (AST::StructStruct &struct_item)
{
  check_lang_item_attribute (struct_item.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (struct_item);
}

void
FeatureGate::visit (AST::TraitItemType &trait_item_type)
{
  check_lang_item_attribute (trait_item_type.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (trait_item_type);
}

void
FeatureGate::visit (AST::Enum &enum_item)
{
  check_lang_item_attribute (enum_item.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (enum_item);
}

void
FeatureGate::visit (AST::EnumItem &enum_variant)
{
  check_lang_item_attribute (enum_variant.get_outer_attrs ());
  AST::DefaultASTVisitor::visit (enum_variant);
}

} // namespace Rust
