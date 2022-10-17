/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s312(struct args_t * func_args)
{
//    reductions
//    product reduction

    initialise_arrays(__func__);

    real_t prod;
    for (int nl = 0; nl < 10*iterations; nl++) {
        prod = (real_t)1.;
        for (int i = 0; i < LEN_1D; i++) {
            prod *= a[i];
        }
        dummy(a, b, c, d, e, aa, bb, cc, prod);
    }

    return prod;
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&s312, "s312", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail *-*-* } } } */
