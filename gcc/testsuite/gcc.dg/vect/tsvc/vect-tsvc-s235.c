/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0 -Diterations=LEN_2D" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s235(struct args_t * func_args)
{
//    loop interchanging
//    imperfectly nested loops

    initialise_arrays(__func__);

    for (int nl = 0; nl < 200*(iterations/LEN_2D); nl++) {
        for (int i = 0; i < LEN_2D; i++) {
            a[i] += b[i] * c[i];
            for (int j = 1; j < LEN_2D; j++) {
                aa[j][i] = aa[j-1][i] + bb[j][i] * a[i];
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

  run(&s235, "s235", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
