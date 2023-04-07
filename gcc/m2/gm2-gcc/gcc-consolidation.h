/* gcc-consolidation.h provides a single header for required gcc headers.

Copyright (C) 2012-2023 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "realmpfr.h"
#include "backend.h"
#include "stringpool.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "df.h"
#include "tm.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "options.h"
#include "wide-int.h"
#include "inchash.h"
#include "stor-layout.h"
#include "attribs.h"
#include "intl.h"
#include "tree-iterator.h"
#include "diagnostic.h"
#include "wide-int-print.h"
#include "real.h"
#include "float.h"
#include "spellcheck.h"
#include "opt-suggestions.h"

/* Utilize some of the C build routines.  */

#include "fold-const.h"
#include "varasm.h"
#include "hashtab.h"
#include "hard-reg-set.h"
#include "function.h"

#include "hash-map.h"
#include "langhooks.h"
#include "timevar.h"
#include "dumpfile.h"
#include "target.h"
#include "dominance.h"
#include "cfg.h"
#include "cfganal.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimple-ssa.h"
#include "gimplify.h"
#include "stringpool.h"
#include "tree-nested.h"
#include "print-tree.h"
#include "except.h"
#include "toplev.h"
#include "convert.h"
#include "tree-dump.h"
#include "plugin-api.h"
#include "hard-reg-set.h"
#include "function.h"
#include "ipa-ref.h"
#include "cgraph.h"
#include "stmt.h"
#include <stdbool.h>
