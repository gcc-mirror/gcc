#include "compat-common.h"

#include "fp-struct-defs.h"
#include "fp-struct-check.h"
#include "fp-struct-test-by-value-x.h"

DEFS(cf, _Complex float)
CHECKS(cf, _Complex float)


TEST(Scf13, _Complex float)
TEST(Scf14, _Complex float)
TEST(Scf15, _Complex float)
TEST(Scf16, _Complex float)

#undef T

void
struct_by_value_16a_x ()
{
DEBUG_INIT

#define T(TYPE, MTYPE) testit##TYPE ();


T(Scf13, _Complex float)
T(Scf14, _Complex float)
T(Scf15, _Complex float)
T(Scf16, _Complex float)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
