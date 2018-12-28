/* This checks .  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  /* Integral types.  */
  run_signed_tests3(check_array_size_and_align, char, TYPE_SIZE_CHAR, TYPE_ALIGN_CHAR);
  run_signed_tests3(check_array_size_and_align, short, TYPE_SIZE_SHORT, TYPE_ALIGN_SHORT);
  run_signed_tests3(check_array_size_and_align, int, TYPE_SIZE_INT, TYPE_ALIGN_INT);
  run_signed_tests3(check_array_size_and_align, long, TYPE_SIZE_LONG, TYPE_ALIGN_LONG);
  run_signed_tests3(check_array_size_and_align, long long, TYPE_SIZE_LONG_LONG, TYPE_ALIGN_LONG_LONG);
#ifdef CHECK_INT128
  run_signed_tests3(check_array_size_and_align, __int128, TYPE_SIZE_INT128, TYPE_ALIGN_INT128);
#endif
  check_array_size_and_align(enum dummytype, TYPE_SIZE_ENUM, TYPE_ALIGN_ENUM);

  /* Floating point types.  */
  check_array_size_and_align(float, TYPE_SIZE_FLOAT, TYPE_ALIGN_FLOAT);
  check_array_size_and_align(double, TYPE_SIZE_DOUBLE, TYPE_ALIGN_DOUBLE);
#ifdef CHECK_LONG_DOUBLE
  check_array_size_and_align(long double, TYPE_SIZE_LONG_DOUBLE, TYPE_ALIGN_LONG_DOUBLE);
#endif
#ifdef CHECK_FLOAT128
  check_array_size_and_align(__float128, TYPE_SIZE_FLOAT128, TYPE_ALIGN_FLOAT128);
#endif

  /* Packed types - MMX, 3DNow!, SSE and SSE2.  */
#ifdef CHECK_M64_M128
  check_array_size_and_align(__m64, TYPE_SIZE_M64, TYPE_ALIGN_M64);
  check_array_size_and_align(__m128, TYPE_SIZE_M128, TYPE_ALIGN_M128);
#endif

  /* Pointer types. The function pointer doesn't work with these macros.  */
  check_array_size_and_align(void *, TYPE_SIZE_POINTER, TYPE_ALIGN_POINTER);

  check_array_size_and_align(_Float16, TYPE_SIZE_FLOAT16, TYPE_ALIGN_FLOAT16);

  return 0;
}
