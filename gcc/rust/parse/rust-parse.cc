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

#include <algorithm> // for std::find

/* TODO: move non-essential stuff back here from rust-parse-impl.h after
 * confirming that it works */

namespace Rust {

} // namespace Rust
