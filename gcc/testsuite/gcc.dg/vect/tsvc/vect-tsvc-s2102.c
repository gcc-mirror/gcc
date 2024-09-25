/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0 -Diterations=LEN_2D" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s2102(struct args_t * func_args)
{
//    diagonals
//    identity matrix, best results vectorize both inner and outer loops

    initialise_arrays(__func__);

    for (int nl = 0; nl < 100*(iterations/LEN_2D); nl++) {
        for (int i = 0; i < LEN_2D; i++) {
            for (int j = 0; j < LEN_2D; j++) {
                aa[j][i] = (real_t)0.;
            }
            aa[i][i] = (real_t)1.;
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

  run(&s2102, "s2102", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
