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

} // namespace Rust
