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

#include "rust-feature-gate.h"
#include "rust-abi.h"
#include "rust-ast-visitor.h"

namespace Rust {

void
FeatureGate::check (AST::Crate &crate)
{
  visit (crate);
}

void
FeatureGate::visit (AST::Crate &crate)
{
  valid_features.clear ();

  for (const auto &attr : crate.inner_attrs)
    {
      if (attr.get_path ().as_string () == "feature")
	{
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
		  auto tname = Feature::as_name (name_str);
		  if (tname.has_value ())
		    {
		      auto name = tname.value ();
		      valid_features.insert (name);
		    }

		  else
		    rust_error_at (item->get_locus (), ErrorCode::E0635,
				   "unknown feature %qs", name_str.c_str ());
		}
	    }
	}
    }

  AST::DefaultASTVisitor::visit (crate);
}

void
FeatureGate::gate (Feature::Name name, location_t loc,
		   const std::string &error_msg)
{
  if (!valid_features.count (name))
    {
      auto feature = Feature::create (name);
      auto issue = feature.issue ();
      if (issue > 0)
	{
	  const char *fmt_str
	    = "%s. see issue %u "
	      "<https://github.com/rust-lang/rust/issues/%u> for more "
	      "information. add `#![feature(%s)]` to the crate attributes to "
	      "enable.";
	  rust_error_at (loc, ErrorCode::E0658, fmt_str, error_msg.c_str (),
			 issue, issue, feature.as_string ().c_str ());
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
FeatureGate::visit (AST::MacroRulesDefinition &rules_def)
{
  check_rustc_attri (rules_def.get_outer_attrs ());
}

void
FeatureGate::visit (AST::Function &function)
{
  check_rustc_attri (function.get_outer_attrs ());
}

void
FeatureGate::visit (AST::ExternalTypeItem &item)
{
  // TODO(mxlol233): The gating needs a complete visiting chain to activate
  // `AST::ExternalTypeItem`.
  gate (Feature::Name::EXTERN_TYPES, item.get_locus (),
	"extern types are experimental");
}

} // namespace Rust
