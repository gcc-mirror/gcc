/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s343(struct args_t * func_args)
{
//    packing
//    pack 2-d array into one dimension
//    not vectorizable, value of k in unknown at each iteration

    initialise_arrays(__func__);

    int k;
    for (int nl = 0; nl < 10*(iterations/LEN_2D); nl++) {
        k = -1;
        for (int i = 0; i < LEN_2D; i++) {
            for (int j = 0; j < LEN_2D; j++) {
                if (bb[j][i] > (real_t)0.) {
                    k++;
                    flat_2d_array[k] = aa[j][i];
                }
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

  run(&s343, "s343", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail *-*-* } } } */
