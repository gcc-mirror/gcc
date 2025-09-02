/* Known inbounds DR in VLA modes.  */
/* { dg-do compile } */
/* { dg-options "-Ofast -msve-vector-bits=scalable --param aarch64-autovec-preference=sve-only -fdump-tree-vect-details" } */

#define N 512
#define START 5
#define END 509

int x[N] __attribute__((aligned(32)));

int __attribute__((noipa))
foo (void)
{
  for (signed int i = START; i < END; ++i)
    {
      if (x[i] == 0)
        return i;
    }
  return -1;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "pfa_iv_offset" "vect" } } */
/* { dg-final { scan-tree-dump-not "Alignment of access forced using peeling" "vect" } } */
