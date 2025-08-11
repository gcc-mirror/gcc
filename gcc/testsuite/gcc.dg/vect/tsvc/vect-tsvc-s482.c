/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_early_break_hw } */
/* { dg-add-options vect_early_break } */

#include "tsvc.h"

real_t s482(struct args_t * func_args)
{
//    non-local goto's
//    other loop exit with code before exit

    initialise_arrays(__func__);

    for (int nl = 0; nl < iterations; nl++) {
        for (int i = 0; i < LEN_1D; i++) {
            a[i] += b[i] * c[i];
            if (c[i] > b[i]) break;
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

  run(&s482, "s482", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail { ! vect_early_break } } } } */
