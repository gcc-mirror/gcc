/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s331(struct args_t * func_args)
{
//    search loops
//    if to last-1

    initialise_arrays(__func__);

    int j;
    real_t chksum;
    for (int nl = 0; nl < iterations; nl++) {
        j = -1;
        for (int i = 0; i < LEN_1D; i++) {
            if (a[i] < (real_t)0.) {
                j = i;
            }
        }
        chksum = (real_t) j;
        dummy(a, b, c, d, e, aa, bb, cc, chksum);
    }

    return j+1;
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&s331, "s331", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */