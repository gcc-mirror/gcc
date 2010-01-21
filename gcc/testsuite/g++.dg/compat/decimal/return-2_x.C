#include "decimal-dummy.h"

#define dec32 std::decimal::decimal32
#define dec64 std::decimal::decimal64
#define dec128 std::decimal::decimal128

#include "return_x.h"

void
return_2_x (void)
{
DEBUG_INIT

#define T(NAME) testit##NAME ();

T(d32)
T(d64)
T(d128)

DEBUG_FINI

if (fails != 0)
  abort ();

#undef T
}
