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
DEFS(cs,_Complex short)
INITS(cs, _Complex short)

TEST(Scs1, _Complex short)
TEST(Scs2, _Complex short)
TEST(Scs3, _Complex short)
TEST(Scs4, _Complex short)
TEST(Scs5, _Complex short)
TEST(Scs6, _Complex short)
TEST(Scs7, _Complex short)
TEST(Scs8, _Complex short)
TEST(Scs9, _Complex short)
TEST(Scs10, _Complex short)
TEST(Scs11, _Complex short)
TEST(Scs12, _Complex short)
TEST(Scs13, _Complex short)
TEST(Scs14, _Complex short)
TEST(Scs15, _Complex short)
TEST(Scs16, _Complex short)
#endif
