/* { dg-do compile } */
/* { dg-options "-march=armv8-a+sve -O2 -fno-move-loop-invariants" } */

void
test__zero (int *restrict dest, int *restrict src, float *a, int count)
{
  int i;

  for (i = 0; i < count; ++i)
    dest[i] = !__builtin_isunordered (a[i], 0) ? src[i] : 0;
}
