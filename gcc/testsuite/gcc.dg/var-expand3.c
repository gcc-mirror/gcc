/* { dg-do run { target { powerpc*-*-* && vmx_hw } } }} */
/* { dg-options "-O2 -funroll-loops -ffast-math -fvariable-expansion-in-unroller -maltivec -fdump-rtl-loop2_unroll" } */

#include "altivec.h"
extern void abort (void);
extern void exit (int);

#define N 256

float in1[N] __attribute__ ((__aligned__ (16))) = {0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57};
float in2[N] __attribute__ ((__aligned__ (16))) = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19};

float
foo (int n)
{
  unsigned i;
  vector float vp1, vp2, vp3, vaccum, vzero, vtmp;
  float accum = 1.0;

  vzero = (vector float){0, 0, 0, 0};

  vaccum = vzero;

  for (i = 0; i < n; i++)
    {
      vp1 = vec_ld (i * 16, in1);
      vp2 = vec_ld (i * 16, in2);

      vaccum = vec_madd (vp1, vp2, vaccum);

    }
  vtmp = vec_sld (vaccum, vaccum, 8);
  vp1 = vec_add (vaccum, vtmp);
  vtmp = vec_sld (vp1, vp1, 4);
  vp2 = vec_add (vp1, vtmp);

  vec_ste (vp2, 0, &accum);
  if (accum != 1518)
    return 0;
 
  return accum;
}

int
main (void)
{
  if (!foo (3))
    abort ();

   exit (0);
}

/* { dg-final { scan-rtl-dump "Expanding Accumulator" "loop2_unroll" } } */




