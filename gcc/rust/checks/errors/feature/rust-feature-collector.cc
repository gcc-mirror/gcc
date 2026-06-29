// Copyright (C) 2026 Free Software Foundation, Inc.

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

#include "rust-feature-collector.h"
#include "rust-attribute-values.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Features {

FeatureCollector::FeatureCollector () : features (CrateFeatures{UNKNOWN_NODEID})
{}

CrateFeatures
FeatureCollector::collect (AST::Crate &crate)
{
  features.valid_lang_features.clear ();
  features.valid_lib_features.clear ();
  features.crate_id = crate.get_node_id ();

  // TODO: this is a hack, remove when possible
  if (Session::get_instance ().get_compat_version () < 50)
    features.valid_lang_features.insert (
      Feature::Name::EXTENDED_KEY_VALUE_ATTRIBUTES);

  visit (crate);

  return features;
}

namespace {
bool
is_feature_attribute (const AST::Attribute &attribute)
{
  return Values::Attributes::FEATURE == attribute.get_path ().as_string ();
}

// check for empty feature, such as `#![feature], this is an error
bool
is_valid_feature_attribute (const AST::Attribute &attribute)
{
  return !attribute.empty_input ();
}
} // namespace

void
FeatureCollector::add_features_from_token_tree (
  const AST::DelimTokenTree &delim_ttree)
{
  std::unique_ptr<AST::AttrInputMetaItemContainer> meta_item (
    delim_ttree.parse_to_meta_item ());
  add_features_from_meta_item_container (*meta_item);
}

void
FeatureCollector::add_features_from_meta_item_container (
  const AST::AttrInputMetaItemContainer &meta_item_container)
{
  for (const auto &item : meta_item_container.get_items ())
    {
      const auto &name_str = item->as_string ();

      // TODO: detect duplicates
      if (auto tname = Feature::as_name (name_str))
	features.valid_lang_features.insert (*tname);
      else
	features.valid_lib_features.emplace (name_str, item->get_locus ());
    }
}

void
FeatureCollector::identify_feature (const AST::Attribute &attribute)
{
  if (is_feature_attribute (attribute))
    {
      if (!is_valid_feature_attribute (attribute))
	{
	  rust_error_at (attribute.get_locus (), ErrorCode::E0556,
			 "malformed %<feature%> attribute input");
	  return;
	}
      const auto &attr_input = attribute.get_attr_input ();
      auto type = attr_input.get_attr_input_type ();
      if (type == AST::AttrInput::AttrInputType::TOKEN_TREE)
	{
	  const auto &delim_ttree = static_cast<const AST::DelimTokenTree &> (
	    attribute.get_attr_input ());
	  add_features_from_token_tree (delim_ttree);
	}
      else if (type == AST::AttrInput::AttrInputType::META_ITEM)
	{
	  // We can find a meta item in #[cfg(toto),feature(xxxx)]
	  const auto &meta_item_container
	    = static_cast<const AST::AttrInputMetaItemContainer &> (attr_input);
	  add_features_from_meta_item_container (meta_item_container);
	}
    }
}

void
FeatureCollector::visit (AST::Crate &crate)
{
  for (const auto &attribute : crate.inner_attrs)
    identify_feature (attribute);
}

} // namespace Features
} // namespace Rust
