#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(cf, _Complex float)
CHECKS(cf, _Complex float)

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
TEST(Scf13, _Complex float)
TEST(Scf14, _Complex float)
TEST(Scf15, _Complex float)
TEST(Scf16, _Complex float)

#undef T

void
struct_by_value_16_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();

T(Scf1, _Complex float)
T(Scf2, _Complex float)
T(Scf3, _Complex float)
T(Scf4, _Complex float)
T(Scf5, _Complex float)
T(Scf6, _Complex float)
T(Scf7, _Complex float)
T(Scf8, _Complex float)
T(Scf9, _Complex float)
T(Scf10, _Complex float)
T(Scf11, _Complex float)
T(Scf12, _Complex float)
T(Scf13, _Complex float)
T(Scf14, _Complex float)
T(Scf15, _Complex float)
T(Scf16, _Complex float)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
