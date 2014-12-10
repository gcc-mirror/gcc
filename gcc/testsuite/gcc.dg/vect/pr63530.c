/* { dg-options "-O2 -ftree-vectorize -funroll-loops --param \"max-completely-peeled-insns=400\"" } */

/* PR tree-optimization/63530 */
/* On armv7 hardware, following options cause run time failure  */
/*   -march=armv7-a -mfloat-abi=hard -mfpu=neon -marm -O2 -ftree-vectorize  */
/*   -funroll-loops --param "max-completely-peeled-insns=400"  */

#include <stdlib.h>

typedef struct {
  unsigned char map[256];
  int i;
} A, *AP;

AP __attribute__ ((noinline))
foo (int n)
{
  AP b = (AP)calloc (1, sizeof (A));
  int i;
  for (i = n; i < 256; i++)
    b->map[i] = i;
  return b;
}

int
main()
{
  AP p = foo(3);
  return p->map[30] - p->map[20] - p->map[10];
}
