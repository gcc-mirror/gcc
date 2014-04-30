// gogo-tree.cc -- convert Go frontend Gogo IR to gcc trees.

// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

#include "go-system.h"

#include "toplev.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"
#include "gimple-expr.h"
#include "gimplify.h"
#include "tree-iterator.h"
#include "cgraph.h"
#include "langhooks.h"
#include "convert.h"
#include "output.h"
#include "diagnostic.h"
#include "go-c.h"

#include "types.h"
#include "expressions.h"
#include "statements.h"
#include "runtime.h"
#include "backend.h"
#include "gogo.h"

// Whether we have seen any errors.

bool
saw_errors()
{
  return errorcount != 0 || sorrycount != 0;
}
