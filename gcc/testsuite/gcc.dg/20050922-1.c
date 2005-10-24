/* This revealed a bug when rotates are expanded into
   two shifts.  */

/* { dg-do run } */
/* { dg-options "-O1 -std=c99" } */

#include <stdlib.h>

#if __INT_MAX__ == 2147483647
typedef unsigned int uint32_t;
#elif __LONG_MAX__ == 2147483647
typedef unsigned long uint32_t;
#else
#error unable to find 32-bit integer type
#endif

uint32_t
f (uint32_t *S, int j)
{
  uint32_t A, B, k, L[2] = {1234, 5678};
  int i, m;

  A = B = 0;
  for (i = 0; i < j; i++)
    {
      k = (S[i] + A + B) & 0xffffffffL;
      A = S[i] =
      ((k << (3 & 0x1f)) | ((k & 0xffffffff) >> (32 - (3 & 0x1f)))); 

      m = (int) (A + B);
      k = (L[i] + A + B) & 0xffffffffL;
      B = L[i] =
	((k << (m & 0x1f)) | ((k & 0xffffffff) >> (32 - (m & 0x1f))));
    }
  return L[0] + L[1];
}

int
main ()
{
  uint32_t S[2] = {0xffff, 0xffffff};

  if (f (S,2)!= 1392607300)
    abort();

  return 0;
}
