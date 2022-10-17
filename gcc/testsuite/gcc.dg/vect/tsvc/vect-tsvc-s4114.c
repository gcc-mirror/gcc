/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s4114(struct args_t * func_args)
{
//    indirect addressing
//    mix indirect addressing with variable lower and upper bounds
//    gather is required

    struct{int * __restrict__ a;int b;} * x = func_args->arg_info;
    int * __restrict__ ip = x->a;
    int n1 = x->b;

    initialise_arrays(__func__);

    int k;
    for (int nl = 0; nl < iterations; nl++) {
        for (int i = n1-1; i < LEN_1D; i++) {
            k = ip[i];
            a[i] = b[i] + c[LEN_1D-k+1-2] * d[i];
            k += 5;
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

  run(&s4114, "s4114", &(struct{int*a;int b;}){ip, n1});

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */