/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t vas(struct args_t * func_args)
{
//    control loops
//    vector assignment, scatter
//    scatter is required

    int * __restrict__ ip = func_args->arg_info;

    initialise_arrays(__func__);

    for (int nl = 0; nl < 2*iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            a[ip[i]] = b[i];
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

  run(&vas, "vas", ip);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
