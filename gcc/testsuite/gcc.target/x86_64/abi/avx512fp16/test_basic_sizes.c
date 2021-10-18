/* This checks sizes of basic types.  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  /* Integral types.  */
  run_signed_tests2(check_size, char, TYPE_SIZE_CHAR);
  run_signed_tests2(check_size, short, TYPE_SIZE_SHORT);
  run_signed_tests2(check_size, int, TYPE_SIZE_INT);
  run_signed_tests2(check_size, long, TYPE_SIZE_LONG);
  run_signed_tests2(check_size, long long, TYPE_SIZE_LONG_LONG);
#ifdef CHECK_INT128
  run_signed_tests2(check_size, __int128, TYPE_SIZE_INT128);
#endif
  check_size(enumtype, TYPE_SIZE_ENUM);

  /* Floating point types.  */
  check_size(_Float16, TYPE_SIZE_FLOAT16);
  check_size(float, TYPE_SIZE_FLOAT);
  check_size(double, TYPE_SIZE_DOUBLE);
#ifdef CHECK_LONG_DOUBLE
  check_size(long double, TYPE_SIZE_LONG_DOUBLE);
#endif
#ifdef CHECK_FLOAT128
  check_size(__float128, TYPE_SIZE_FLOAT128);
#endif

  /* Packed types - MMX, 3DNow!, SSE and SSE2.  */
#ifdef CHECK_M64_M128
  check_size(__m64, TYPE_SIZE_M64);
  check_size(__m128, TYPE_SIZE_M128);
#endif

  /* Pointer types.  */
  check_size(void *, TYPE_SIZE_POINTER);
  check_size(void (*)(), TYPE_SIZE_POINTER);

  return 0;
}
