/* Fix for PR123089 alignment peeling with vectors and VLS and overflows.  */
/* { dg-do compile } */
/* { dg-options "-Ofast --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */
/* { dg-additional-options "-msve-vector-bits=256" { target aarch64_sve256_hw } } */
/* { dg-additional-options "-msve-vector-bits=128" { target aarch64_sve128_hw } } */

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */

#define START 2

int __attribute__((noipa))
foo (unsigned char n, int *x)
{
  unsigned char i = 0;
#pragma GCC unroll 0
  for (i = START; i < n; ++i)
    {
      if (x[i] == 0)
        return i;
      x[i] += 1;
    }
  return i;
}

