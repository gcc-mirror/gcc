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
DEFS(cl,_Complex long)
INITS(cl, _Complex long)

TEST(Scl1, _Complex long)
TEST(Scl2, _Complex long)
TEST(Scl3, _Complex long)
TEST(Scl4, _Complex long)
TEST(Scl5, _Complex long)
TEST(Scl6, _Complex long)
TEST(Scl7, _Complex long)
TEST(Scl8, _Complex long)
TEST(Scl9, _Complex long)
TEST(Scl10, _Complex long)
TEST(Scl11, _Complex long)
TEST(Scl12, _Complex long)
TEST(Scl13, _Complex long)
TEST(Scl14, _Complex long)
TEST(Scl15, _Complex long)
TEST(Scl16, _Complex long)
#endif
