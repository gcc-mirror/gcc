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

#include "rust-feature-gate.h"
#include "rust-feature.h"

namespace Rust {

void
FeatureGate::check (AST::Crate &crate)
{
  std::vector<Feature> valid_features;
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
		  const auto &name = item->as_string ();
		  auto tname = Feature::as_name (name);
		  if (!tname.is_none ())
		    valid_features.push_back (Feature::create (tname.get ()));
		  else
		    rust_error_at (item->get_locus (), "unknown feature '%s'",
				   name.c_str ());
		}
	    }
	}
    }
  valid_features.shrink_to_fit ();

  // TODO (mxlol233): add the real feature gate stuff.
  auto &items = crate.items;
  for (auto it = items.begin (); it != items.end (); it++)
    {
      auto &item = *it;
      item->accept_vis (*this);
    }
}
} // namespace Rust