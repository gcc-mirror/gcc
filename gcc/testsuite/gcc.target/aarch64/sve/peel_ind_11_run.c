/* Peeling for alignment with masking in VLA modes.  */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-Ofast -msve-vector-bits=scalable --param aarch64-autovec-preference=sve-only" } */

#include "peel_ind_11.c"
#include <stdio.h>
#include <stdlib.h>

#define N 512

int __attribute__ ((optimize (1)))
main (void)
{
  for (int k = 5; k < 30; k++) {
    int *a = (int *) malloc (sizeof(int) * N);

    /* Set only one non-zero element for test.  */
    for (int i = 5; i < 30; i++)
      a[i] = (i == k ? 1 : 0);

    int res = foo (a);
    asm volatile ("");
    if (res != k) {
      __builtin_abort ();
    }
  }
}
