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
  for (unsigned int i = START; i < END; i*=2)
    {
      if (x[i] == 0)
        return i;
    }
  return -1;
}

/* { dg-final { scan-tree-dump-not "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "pfa_iv_offset" "vect" } } */
/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
