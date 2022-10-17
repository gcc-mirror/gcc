/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s122(struct args_t * func_args)
{
//    induction variable recognition
//    variable lower and upper bound, and stride
//    reverse data access and jump in data access

    struct{int a;int b;} * x = func_args->arg_info;
    int n1 = x->a;
    int n3 = x->b;

    initialise_arrays(__func__);

    int j, k;
    for (int nl = 0; nl < iterations; nl++) {
        j = 1;
        k = 0;
        for (int i = n1-1; i < LEN_1D; i += n3) {
            k += j;
            a[i] += b[LEN_1D - k];
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

  run(&s122, "s122", &(struct{int a;int b;}){n1, n3});

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail *-*-* } } } */
