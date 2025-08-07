// DR 2577 - Undefined behavior for preprocessing directives in macro arguments
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A(x, y, z) x + y + z
int a = A(
#include "dr2577-2.h"		// { dg-error "embedding a directive within macro arguments is not portable" }
,
#include "dr2577-2.h"
,
#include "dr2577-2.h"
);
// { dg-error "unterminated argument list invoking macro 'A'" "" { target *-*-* } 0 }
