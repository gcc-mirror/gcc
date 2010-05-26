/* { dg-do run { target int128 } } */
/* { dg-options "-std=gnu99" { target c } } */
/* { dg-options "" { target c++ } } */

#include <stdarg.h>

#ifndef __cplusplus
extern void abort (void);
#else
extern "C" void abort (void);
#endif

#define MK_CONST128(A,B,C,D) \
        ( (((unsigned __int128) (unsigned int) A) << 96) \
         | (((unsigned __int128) (unsigned int) B) << 64) \
         | (((unsigned __int128) (unsigned int) C) << 32) \
         | ((unsigned __int128) (unsigned int) D) )

#define MK_CONST128_SIGNED(A,B,C,D) \
        ((__int128) MK_CONST128(A, B, C, D))

void foo(int i, ...)
{
  __int128 q;
  va_list va;

  va_start(va, i);
  q = va_arg(va, __int128);
  va_end(va);

  if (q != MK_CONST128_SIGNED (0xfeffffffU, 2U, 3U, 4U))
    abort();
}

int main(void)
{
  __int128 q = MK_CONST128_SIGNED (0xfeffffffU, 2U, 3U, 4U);

  foo(1, q);
  return 0;
}
