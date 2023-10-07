/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0" } */
/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s1232(struct args_t * func_args)
{
//    loop interchange
//    interchanging of triangular loops

    initialise_arrays(__func__);

    for (int nl = 0; nl < 100*(iterations/LEN_2D); nl++) {
        for (int j = 0; j < LEN_2D; j++) {
            for (int i = j; i < LEN_2D; i++) {
                aa[i][j] = bb[i][j] + cc[i][j];
            }
        }
        dummy(a, b, c, d, e, aa, bb, cc, 1.);
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

  run(&s1232, "s1232", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { xfail { { ! aarch64_sve } && { ! riscv_v } }  } } } */
