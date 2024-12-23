// Copyright (C) 2024 Free Software Foundation, Inc.

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

#include "rust-collect-lang-items.h"
#include "optional.h"
#include "rust-ast-collector.h"
#include "rust-ast-visitor.h"
#include "rust-ast.h"
#include "rust-attribute-values.h"
#include "rust-attributes.h"
#include "rust-hir-map.h"
#include "rust-item.h"

namespace Rust {
namespace AST {

template <typename T>
tl::optional<LangItem::Kind>
get_lang_item_attr (const T &maybe_lang_item)
{
  for (const auto &attr : maybe_lang_item.get_outer_attrs ())
    {
      const auto &str_path = attr.get_path ().as_string ();
      if (!Analysis::Attributes::is_known (str_path))
	{
	  rust_error_at (attr.get_locus (), "unknown attribute %qs",
			 str_path.c_str ());
	  continue;
	}

      bool is_lang_item = str_path == Values::Attributes::LANG
			  && attr.has_attr_input ()
			  && attr.get_attr_input ().get_attr_input_type ()
			       == AST::AttrInput::AttrInputType::LITERAL;

      if (is_lang_item)
	{
	  auto &literal
	    = static_cast<AST::AttrInputLiteral &> (attr.get_attr_input ());
	  const auto &lang_item_type_str = literal.get_literal ().as_string ();

	  return LangItem::Parse (lang_item_type_str);
	}
    }

  return tl::nullopt;
}

template <typename T>
void
CollectLangItems::maybe_add_lang_item (const T &item)
{
  if (auto lang_item = get_lang_item_attr (item))
    mappings.insert_lang_item_node (lang_item.value (), item.get_node_id ());
}

void
CollectLangItems::visit (AST::Trait &item)
{
  maybe_add_lang_item (item);

  DefaultASTVisitor::visit (item);
}

void
CollectLangItems::visit (AST::TraitItemType &item)
{
  maybe_add_lang_item (item);

  DefaultASTVisitor::visit (item);
}

void
CollectLangItems::visit (AST::Function &item)
{
  maybe_add_lang_item (item);

  DefaultASTVisitor::visit (item);
}

void
CollectLangItems::visit (AST::StructStruct &item)
{
  maybe_add_lang_item (item);

  DefaultASTVisitor::visit (item);
}

void
CollectLangItems::visit (AST::EnumItem &item)
{
  maybe_add_lang_item (item);

  DefaultASTVisitor::visit (item);
}

} // namespace AST
} // namespace Rust
