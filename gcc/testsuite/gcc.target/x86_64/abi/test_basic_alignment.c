/* This checks alignment of basic types.  */

#include "defines.h"
#include "macros.h"


int
main (void)
{
  /* Integral types.  */
  run_signed_tests2(check_align, char, TYPE_ALIGN_CHAR);
  run_signed_tests2(check_align, short, TYPE_ALIGN_SHORT);
  run_signed_tests2(check_align, int, TYPE_ALIGN_INT);
  run_signed_tests2(check_align, long, TYPE_ALIGN_LONG);
  run_signed_tests2(check_align, long long, TYPE_ALIGN_LONG_LONG);
#ifdef CHECK_INT128
  run_signed_tests2(check_align, __int128, TYPE_ALIGN_INT128);
#endif
  check_align(enumtype, TYPE_ALIGN_ENUM);

  /* Floating point types.  */
  check_align(float, TYPE_ALIGN_FLOAT);
  check_align(double, TYPE_ALIGN_DOUBLE);
#ifdef CHECK_LONG_DOUBLE
  check_align(long double, TYPE_ALIGN_LONG_DOUBLE);
#endif
#ifdef CHECK_FLOAT128
  check_align(__float128, TYPE_ALIGN_FLOAT128);
#endif

  /* Packed types - MMX, 3DNow!, SSE and SSE2.  */
#ifdef CHECK_M64_M128
  check_align(__m64, TYPE_ALIGN_M64);
  check_align(__m128, TYPE_ALIGN_M128);
#endif

  /* Pointer types.  */
  check_align(void *, TYPE_ALIGN_POINTER);
  check_align(void (*)(), TYPE_ALIGN_POINTER);

  return 0;
}
