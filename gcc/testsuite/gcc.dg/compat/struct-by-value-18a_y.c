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
DEFS(cld,_Complex long double)
INITS(cld, _Complex long double)

TEST(Scld13, _Complex long double)
TEST(Scld14, _Complex long double)
TEST(Scld15, _Complex long double)
TEST(Scld16, _Complex long double)
#endif
