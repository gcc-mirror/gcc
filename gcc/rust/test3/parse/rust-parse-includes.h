#ifndef RUST_PARSE_INCLUDES_H
#define RUST_PARSE_INCLUDES_H
/* basically, this is just to stop gcc includes in rust-parse.cc from getting out of order due to 
 * formatter, which I can not figure out how to modify to not do this. */
// HACK: remove for final or near-final compiler where I don't have to use auto formatter anymore

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