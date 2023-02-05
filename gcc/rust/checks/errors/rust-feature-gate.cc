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

#include "rust-feature-gate.h"
#include "rust-abi.h"

namespace Rust {

void
FeatureGate::check (AST::Crate &crate)
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
		  if (!tname.is_none ())
		    {
		      auto name = tname.get ();
		      valid_features.insert (name);
		    }

		  else
		    rust_error_at (item->get_locus (), "unknown feature '%s'",
				   name_str.c_str ());
		}
	    }
	}
    }

  auto &items = crate.items;
  for (auto it = items.begin (); it != items.end (); it++)
    {
      auto &item = *it;
      item->accept_vis (*this);
    }
}

void
FeatureGate::gate (Feature::Name name, Location loc,
		   const std::string &error_msg)
{
  if (!valid_features.count (name))
    {
      auto feature = Feature::create (name);
      auto issue = feature.issue ();
      if (issue > 0)
	{
	  const char *fmt_str
	    = "%s. see issue %ld "
	      "<https://github.com/rust-lang/rust/issues/%ld> for more "
	      "information. add `#![feature(%s)]` to the crate attributes to "
	      "enable.";
	  rust_error_at (loc, fmt_str, error_msg.c_str (), issue, issue,
			 feature.as_string ().c_str ());
	}
      else
	{
	  const char *fmt_str
	    = "%s. add `#![feature(%s)]` to the crate attributes to enable.";
	  rust_error_at (loc, fmt_str, error_msg.c_str (),
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
}

} // namespace Rust