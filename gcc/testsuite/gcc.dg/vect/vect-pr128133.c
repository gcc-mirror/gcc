/* { dg-require-effective-target bitint } */

#include "tree-vect.h"

__attribute__((noipa))
void convert(unsigned *dst, unsigned _BitInt(17) *src)
{
  for (int i = 0; i < 4; ++i)
    dst[i] = src[i];
}

int main()
{
  unsigned _BitInt(17) src[4];
  unsigned dst[4];

  check_vect ();

  __builtin_memset(src, 0xff, sizeof src);
  convert(dst, src);
  if (dst[0] != 131071)
    __builtin_abort ();
  return 0;
}
