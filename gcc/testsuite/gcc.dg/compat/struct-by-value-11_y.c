#include <stdarg.h>

#include "compat-common.h"

#ifdef SKIP_VA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "fp-struct-defs.h"
#include "fp-struct-init.h"
#include "fp-struct-test-by-value-y.h"

#ifndef SKIP_COMPLEX_INT
DEFS(cc,_Complex char)
INITS(cc, _Complex char)

TEST(Scc1, _Complex char)
TEST(Scc2, _Complex char)
TEST(Scc3, _Complex char)
TEST(Scc4, _Complex char)
TEST(Scc5, _Complex char)
TEST(Scc6, _Complex char)
TEST(Scc7, _Complex char)
TEST(Scc8, _Complex char)
TEST(Scc9, _Complex char)
TEST(Scc10, _Complex char)
TEST(Scc11, _Complex char)
TEST(Scc12, _Complex char)
TEST(Scc13, _Complex char)
TEST(Scc14, _Complex char)
TEST(Scc15, _Complex char)
TEST(Scc16, _Complex char)
#endif
