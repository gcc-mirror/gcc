/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=256 --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */

#define N 512
#define START 0
#define END 505
 
int x[N] __attribute__((aligned(32)));

int __attribute__((noipa))
foo (int start)
{
  for (unsigned int i = start; i < END; ++i)
    {
      if (x[i] == 0)
        return i;
    }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump "pfa_iv_offset" "vect" } } */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */
