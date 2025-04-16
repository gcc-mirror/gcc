/* Fix for PR119351 alignment peeling with vectors and VLS.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=256 --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */

#define N 512
#define START 1
#define END 505
 
int x[N] __attribute__((aligned(32)));

int __attribute__((noipa))
foo (void)
{
  for (int *p = x + START; p < x + END; p++)
    {
      if (*p == 0)
        return START;
    }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* Peels using a scalar loop.  */
/* { dg-final { scan-tree-dump-not "pfa_iv_offset" "vect" } } */
/* { dg-final { scan-tree-dump "Alignment of access forced using peeling" "vect" } } */
