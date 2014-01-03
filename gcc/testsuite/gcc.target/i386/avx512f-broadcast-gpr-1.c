/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+%r\[^\n\]+%zmm\[0-9\]\[^\{\]" 1 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+%e\[^\n\]+%zmm\[0-9\]\[^\{\]" 1 { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[ \\t\]+\[^\n\]+%zmm\[0-9\]\[^\{\]" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "vpbroadcastd\[ \\t\]+\[^\n\]+%zmm\[0-9\]\[^\{\]" 1 { target ia32 } } } */

#include <x86intrin.h>

__m512i
foo_1 (long long y)
{
  return __extension__ (__m512i)(__v8di){ y, y, y, y, y, y, y, y };
}

__m512i
foo_2 (int y)
{
  return __extension__ (__m512i)(__v16si){ y, y, y, y, y, y, y, y, y,
				      y, y, y, y, y, y, y };
}
