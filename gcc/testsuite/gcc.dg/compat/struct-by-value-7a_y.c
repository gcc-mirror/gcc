#include <stdarg.h>

#include "compat-common.h"

/* Turn off checking for variable arguments with -DSKIPVA.  */
#ifdef SKIPVA
const int test_va = 0;
#else
const int test_va = 1;
#endif

#include "fp-struct-defs.h"
#include "fp-struct-init.h"
#include "fp-struct-test-by-value-y.h"

DEFS(ld, long double)
INITS(ld, long double)

TEST(Sld1, long double)
TEST(Sld2, long double)
TEST(Sld3, long double)
TEST(Sld4, long double)
TEST(Sld5, long double)
TEST(Sld6, long double)
TEST(Sld7, long double)
TEST(Sld8, long double)
