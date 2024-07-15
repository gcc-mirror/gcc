/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

/* This test requires +-Inf and NaN, so disable finite-math-only */
/* { dg-additional-options "-fno-finite-math-only" } */

#include "tsvc.h"

real_t s1281(struct args_t * func_args)
{
//    crossing thresholds
//    index set splitting
//    reverse data access

    initialise_arrays(__func__);

    real_t x;
    for (int nl = 0; nl < 4*iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            x = b[i]*c[i] + a[i]*d[i] + e[i];
            a[i] = x-(real_t)1.0;
            b[i] = x;
        }
        dummy(a, b, c, d, e, aa, bb, cc, 0.);
    }

    return calc_checksum(__func__);
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&s1281, "s1281", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */