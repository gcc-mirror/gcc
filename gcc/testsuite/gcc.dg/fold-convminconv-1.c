/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef int int32_t __attribute__((mode (__SI__)));
typedef unsigned uint32_t __attribute__((mode (__SI__)));

int32_t foo (unsigned short a[], uint32_t x)
{
  uint32_t i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (x >= 255 ? 255 : x);
    }
  return x;
}

/* { dg-final { scan-tree-dump-not " = MIN_EXPR <x_\[0-9\]*" "optimized" } } */
