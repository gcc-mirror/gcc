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
DEFS(ci,_Complex int)
INITS(ci, _Complex int)

TEST(Sci1, _Complex int)
TEST(Sci2, _Complex int)
TEST(Sci3, _Complex int)
TEST(Sci4, _Complex int)
TEST(Sci5, _Complex int)
TEST(Sci6, _Complex int)
TEST(Sci7, _Complex int)
TEST(Sci8, _Complex int)
TEST(Sci9, _Complex int)
TEST(Sci10, _Complex int)
TEST(Sci11, _Complex int)
TEST(Sci12, _Complex int)
TEST(Sci13, _Complex int)
TEST(Sci14, _Complex int)
TEST(Sci15, _Complex int)
TEST(Sci16, _Complex int)
#endif
