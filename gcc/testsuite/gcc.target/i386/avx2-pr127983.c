/* PR rtl-optimization/127983 */
/* { dg-do run { target avx2 } } */
/* { dg-options "-O2 -mavx2" } */

#include "avx2-check.h"

typedef int v4si __attribute__((vector_size (16)));
typedef int v8si __attribute__((vector_size (32)));
volatile v4si x;
volatile v8si y;

static void
avx2_test (void)
{
  volatile int z = 1;
  int s = z;
  for (int i = 0; i < z; i++)
    {
      x = (v4si) { s, s, s, s };
      s++;
      y = (v8si) { s, s, s, s, s, s, s, s };
    }
  if (y[0] != 2)
    abort ();
}
