/* { dg-additional-options "-w -Wno-psabi" } */

#include "tree-vect.h"

typedef int v4si __attribute__((vector_size(16)));
typedef int v8si __attribute__((vector_size(32)));

void __attribute__((noipa)) test_lo (v4si *dst, v8si src)
{
  (*dst)[0] = src[0];
  (*dst)[1] = src[1];
  (*dst)[2] = src[2];
  (*dst)[3] = src[3];
}

void __attribute__((noipa)) test_hi (v4si *dst, v8si src)
{
  (*dst)[0] = src[4];
  (*dst)[1] = src[5];
  (*dst)[2] = src[6];
  (*dst)[3] = src[7];
}

void __attribute__((noipa)) test_even (v4si *dst, v8si src)
{
  (*dst)[0] = src[0];
  (*dst)[1] = src[2];
  (*dst)[2] = src[4];
  (*dst)[3] = src[6];
}

void __attribute__((noipa)) test_odd (v4si *dst, v8si src)
{
  (*dst)[0] = src[1];
  (*dst)[1] = src[3];
  (*dst)[2] = src[5];
  (*dst)[3] = src[7];
}

int main()
{
  check_vect ();
  v8si v = (v8si) { 0, 1, 2, 3, 4, 5, 6, 7 };
  v4si dst;
  test_lo (&dst, v);
  if (dst[0] != 0 || dst[1] != 1 || dst[2] != 2 || dst[3] != 3)
    abort ();
  test_hi (&dst, v);
  if (dst[0] != 4 || dst[1] != 5 || dst[2] != 6 || dst[3] != 7)
    abort ();
  test_even (&dst, v);
  if (dst[0] != 0 || dst[1] != 2 || dst[2] != 4 || dst[3] != 6)
    abort ();
  test_odd (&dst, v);
  if (dst[0] != 1 || dst[1] != 3 || dst[2] != 5 || dst[3] != 7)
    abort ();
  return 0;
}
