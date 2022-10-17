/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

void s151s(real_t a[LEN_1D], real_t b[LEN_1D],  int m)
{
    for (int i = 0; i < LEN_1D-1; i++) {
        a[i] = a[i + m] + b[i];
    }
}

real_t s151(struct args_t * func_args)
{
//    interprocedural data flow analysis
//    passing parameter information into a subroutine

    initialise_arrays(__func__);

    for (int nl = 0; nl < 5*iterations; nl++) {
        s151s(a, b,  1);
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

  run(&s151, "s151", NULL);

  return 0;
}


/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */