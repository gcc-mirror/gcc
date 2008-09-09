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
DEFS(cf,_Complex float)
INITS(cf, _Complex float)

TEST(Scf13, _Complex float)
TEST(Scf14, _Complex float)
TEST(Scf15, _Complex float)
TEST(Scf16, _Complex float)
#endif
