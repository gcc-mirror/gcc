// DR 2577 - Undefined behavior for preprocessing directives in macro arguments
// { dg-do preprocess }
// { dg-options "-pedantic-errors" }

#define A(x) x
int a = A(
#include "dr2577-3.h"		// { dg-error "embedding a directive within macro arguments is not portable" }
