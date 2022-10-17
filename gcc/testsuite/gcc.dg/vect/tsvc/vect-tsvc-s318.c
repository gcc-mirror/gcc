/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s318(struct args_t * func_args)
{
//    reductions
//    isamax, max absolute value, increments not equal to 1

    int inc = *(int*)func_args->arg_info;

    initialise_arrays(__func__);

    int k, index;
    real_t max, chksum;
    for (int nl = 0; nl < iterations/2; nl++) {
        k = 0;
        index = 0;
        max = ABS(a[0]);
        k += inc;
        for (int i = 1; i < LEN_1D; i++) {
            if (ABS(a[k]) <= max) {
                goto L5;
            }
            index = i;
            max = ABS(a[k]);
L5:
            k += inc;
        }
        chksum = max + (real_t) index;
        dummy(a, b, c, d, e, aa, bb, cc, chksum);
    }

    return max + index + 1;
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&s318, "s318", &n1);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail *-*-* } } } */
