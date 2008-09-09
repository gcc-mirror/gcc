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

TEST(Scf1, _Complex float)
TEST(Scf2, _Complex float)
TEST(Scf3, _Complex float)
TEST(Scf4, _Complex float)
TEST(Scf5, _Complex float)
TEST(Scf6, _Complex float)
TEST(Scf7, _Complex float)
TEST(Scf8, _Complex float)
TEST(Scf9, _Complex float)
TEST(Scf10, _Complex float)
TEST(Scf11, _Complex float)
TEST(Scf12, _Complex float)
#endif
