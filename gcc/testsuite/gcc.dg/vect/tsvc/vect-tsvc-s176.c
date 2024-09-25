/*  This file is distributed under the University of Illinois Open Source
    License. See license.txt for details.  */

/* { dg-additional-options "--param vect-epilogues-nomask=0 -Diterations=3200" } */
/* { dg-additional-options "-DTRUNCATE_TEST" { target { ! run_expensive_tests } } } */

/* { dg-require-effective-target vect_float } */

#include "tsvc.h"

real_t s176(struct args_t * func_args)
{
//    symbolics
//    convolution

    initialise_arrays(__func__);

#ifdef TRUNCATE_TEST
/* Reduce the iteration counts without changing what is a variable and
   what is a constant expression.
   32000/25 == 640, i.e. it still has a nice power of two factor, but is
   not a power of two itself, and still somewhat large-ish, so hopefully
   this won't perturb the vectorizer decisions much.  */
#define M_CONST LEN_1D/50
#else
#define M_CONST LEN_1D/2
#endif
    int m = M_CONST;
    
    for (int nl = 0; nl < 4*(10*iterations/LEN_1D); nl++) {
        for (int j = 0; j < (M_CONST); j++) {
            for (int i = 0; i < m; i++) {
                a[i] += b[i+m-j-1] * c[j];
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

  run(&s176, "s176", NULL);

  return 0;
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
