/* { dg-do run }  */
/* { dg-require-effective-target avx2 }  */
/* { dg-options "-O3 -mavx2" }  */

#include "avx2-check.h"

typedef unsigned int ui;
ui x[32*32];
ui y[32];
ui z[32];
void __attribute__ ((noinline, noclone)) foo (ui n, ui z)
{
  ui i, b;
  ui v;
 for (i = 0; i< n; i++)
  {
    v = y[i];
    if (v) {
      for (b = 0; b < 32; b++)
	if ((v >> b) & 1)
	  x[i*32 +b] = z;
      y[i] = 0;
    }
  } 
}

static void
avx2_test (void)
{
  int i;
  unsigned int val;
  for (i = 0; i<32; i++)
    {
      val = 1U << i;
      y[i] = (i & 1)? 0 : val;
      z[i] = i;
    }
  foo (32, 10);
  for (i=0; i<1024; i+=66)
    if (x[i] != 10)
      __builtin_abort ();
}
