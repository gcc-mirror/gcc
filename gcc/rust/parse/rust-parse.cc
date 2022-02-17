/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#include "rust-parse.h"
#include "rust-linemap.h"
#include "rust-diagnostics.h"

#if 0
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"
/* order: config, system, coretypes, target, tree, tree-iterator, input, diagnostic, stringpool,
 * cgraph, gimplify, gimple-expr, convert, print-tree, stor-layout, fold-const  */
// probably don't need all these
#endif
// maybe put these back in if compiling no longer works

/* TODO: move non-essential stuff back here from rust-parse-impl.h after
 * confirming that it works */

namespace Rust {

std::string
extract_module_path (const AST::AttrVec &inner_attrs,
		     const AST::AttrVec &outer_attrs, const std::string &name)
{
  AST::Attribute path_attr = AST::Attribute::create_empty ();
  for (const auto &attr : inner_attrs)
    {
      if (attr.get_path ().as_string () == "path")
	{
	  path_attr = attr;
	  break;
	}
    }

  // Here, we found a path attribute, but it has no associated string. This is
  // invalid
  if (!path_attr.is_empty () && !path_attr.has_attr_input ())
    {
      rust_error_at (
	path_attr.get_locus (),
	// Split the format string so that -Wformat-diag does not complain...
	"path attributes must contain a filename: '%s'", "#![path = \"file\"]");
      return name;
    }

  for (const auto &attr : outer_attrs)
    {
      if (attr.get_path ().as_string () == "path")
	{
	  path_attr = attr;
	  break;
	}
    }

  // We didn't find a path attribute. This is not an error, there simply isn't
  // one present
  if (path_attr.is_empty ())
    return name;

  // Here, we found a path attribute, but it has no associated string. This is
  // invalid
  if (!path_attr.has_attr_input ())
    {
      rust_error_at (
	path_attr.get_locus (),
	// Split the format string so that -Wformat-diag does not complain...
	"path attributes must contain a filename: '%s'", "#[path = \"file\"]");
      return name;
    }

  auto path_value = path_attr.get_attr_input ().as_string ();

  // At this point, the 'path' is of the following format: '= "<file.rs>"'
  // We need to remove the equal sign and only keep the actual filename.
  // In order to do this, we can simply go through the string until we find
  // a character that is not an equal sign or whitespace
  auto filename_begin = path_value.find_first_not_of ("=\t ");

  auto path = path_value.substr (filename_begin);

  // On windows, the path might mix '/' and '\' separators. Replace the
  // UNIX-like separators by MSDOS separators to make sure the path will resolve
  // properly.
  //
  // Source: rustc compiler
  // (https://github.com/rust-lang/rust/blob/9863bf51a52b8e61bcad312f81b5193d53099f9f/compiler/rustc_expand/src/module.rs#L174)
#if defined(HAVE_DOS_BASED_FILE_SYSTEM)
  path.replace ('/', '\\');
#endif /* HAVE_DOS_BASED_FILE_SYSTEM */

  return path;
}
} // namespace Rust
