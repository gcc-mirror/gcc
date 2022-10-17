/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s315(struct args_t * func_args)
{
//    reductions
//    if to max with index reductio 1 dimension

    initialise_arrays(__func__);

    for (int i = 0; i < LEN_1D; i++)
        a[i] = (i * 7) % LEN_1D;

    real_t x, chksum;
    int index;
    for (int nl = 0; nl < iterations; nl++) {
        x = a[0];
        index = 0;
        for (int i = 0; i < LEN_1D; ++i) {
            if (a[i] > x) {
                x = a[i];
                index = i;
            }
        }
        chksum = x + (real_t) index;
        dummy(a, b, c, d, e, aa, bb, cc, chksum);
    }

    return index + x + 1;
}

int main (int argc, char **argv)
{
  int n1 = 1;
  int n3 = 1;
  int* ip;
  real_t s1,s2;
  init(&ip, &s1, &s2);

  run(&s315, "s315", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */