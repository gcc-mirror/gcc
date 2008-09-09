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

#ifndef SKIP_COMPLEX
DEFS(cd,_Complex double)
INITS(cd, _Complex double)

TEST(Scd13, _Complex double)
TEST(Scd14, _Complex double)
TEST(Scd15, _Complex double)
TEST(Scd16, _Complex double)
#endif
