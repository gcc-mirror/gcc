/* { dg-additional-options "-mavx2" { target avx2 } } */

#include "tree-vect.h"

typedef __INT64_TYPE__ int64_t;
unsigned char b[256];
int64_t res_x;
unsigned res_s;

__attribute__((noipa)) void
f (unsigned char *bb, int64_t init)
{
  int64_t x = init;
  unsigned s = 0;
  for (int i = 0; i < 256; i++)
    {
      s += bb[i];
      x >>= 3;
    }
  res_x = x;
  res_s = s;
}

int main ()
{
  check_vect ();

#pragma GCC novector
  for (int i = 0; i < 256; i++)
    b[i] = (unsigned char) (i + 1);

  f (b, -1);
  if (res_x != -1 || res_s != 32640)
    __builtin_abort ();

  return 0;
}
