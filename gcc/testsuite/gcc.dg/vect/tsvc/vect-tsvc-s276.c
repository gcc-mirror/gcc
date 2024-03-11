/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */
/* { dg-timeout-factor 2 } */

#include "tsvc.h"

real_t s276(struct args_t * func_args)
{
//    control flow
//    if test using loop index

    initialise_arrays(__func__);

    int mid = (LEN_1D/2);
    for (int nl = 0; nl < 4*iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            if (i+1 < mid) {
                a[i] += b[i] * c[i];
            } else {
                a[i] += b[i] * d[i];
            }
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

  run(&s276, "s276", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail { { ! aarch64_sve } && { ! riscv_v } }  } } } */
