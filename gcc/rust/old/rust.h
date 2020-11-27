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

#ifndef __GCC_RUST_H__
#define __GCC_RUST_H__

#include "config.h"

// These must be included before the #poison declarations in system.h.
#include <algorithm>
#include <list>
#include <map>
#include <set>
#include <string>
#include <vector>

#include "ansidecl.h"
#include "basic-block.h"
#include "cgraph.h"
#include "common/common-target.h"
#include "convert.h"
#include "coretypes.h"
#include "debug.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "flags.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "input.h"
#include "langhooks-def.h"
#include "langhooks.h"
#include "options.h"
#include "opts.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "stringpool.h"
#include "system.h"
#include "target.h"
#include "toplev.h"
#include "tree-iterator.h"
#include "tree-pass.h"
#include "tree.h"
#include "varasm.h"
#include <hashtab.h>
#include <vec.h>

#include <gmp.h>
#include <mpfr.h>
/* rust include */
#include "rdot-impl.h"

#if !defined(YYLTYPE)
// Location as used in grs - line and column numbers
typedef struct grs_location {
    int line;
    int column;
} grs_location_t;
// The location of a type?
typedef grs_location_t YYLTYPE;
// The location of a type?
#define YYLTYPE YYLTYPE
#endif

extern char* GRS_current_infile;
extern char* GRS_current_infname;

/* important langhook prototypes */
extern void grs_set_prefix(const char*);
extern void grs_preserve_from_gc(tree);
extern void grs_add_search_path(const char*);
extern void grs_parse_input_files(const char**, unsigned int);
extern tree grs_type_for_size(unsigned int, int);
extern tree grs_type_for_mode(enum machine_mode, int);

extern bool grs_do_compile(const char*);

/* rdot pass manager */
extern tree cstring_type_node;

extern vec<rdot, va_gc>* dot_pass_inferTypes(vec<rdot, va_gc>*);
extern vec<rdot, va_gc>* dot_pass_PrettyPrint(vec<rdot, va_gc>*);
extern vec<tree, va_gc>* dot_pass_Genericify(vec<rdot, va_gc>*);
extern void dot_pass_pushDecl(rdot);
extern void dot_pass_WriteGlobals(void);

/* hooks */
extern void rs_fill_runtime_decls(std::map<std::string, tree>*);

#endif //__GCC_RUST_H__
