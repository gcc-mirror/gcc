/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */
/* { dg-timeout-factor 2 } */

#include "tsvc.h"

real_t vdotr(struct args_t * func_args)
{
//    control loops
//    vector dot product reduction

    initialise_arrays(__func__);

    real_t dot;
    for (int nl = 0; nl < iterations*10; nl++) {
        dot = 0.;
        for (int i = 0; i < LEN_1D; i++) {
            dot += a[i] * b[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, dot);
    }

    return dot;
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&vdotr, "vdotr", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
