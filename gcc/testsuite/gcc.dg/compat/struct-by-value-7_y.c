#include <stdarg.h>

#ifdef DBG
#include <stdio.h>
#define DEBUG_FPUTS(x) fputs (x, stdout)
#define DEBUG_DOT putc ('.', stdout)
#define DEBUG_NL putc ('\n', stdout)
#else
#define DEBUG_FPUTS(x)
#define DEBUG_DOT
#define DEBUG_NL
#endif

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
TEST(Sld9, long double)
TEST(Sld10, long double)
TEST(Sld11, long double)
TEST(Sld12, long double)
TEST(Sld13, long double)
TEST(Sld14, long double)
TEST(Sld15, long double)
TEST(Sld16, long double)
