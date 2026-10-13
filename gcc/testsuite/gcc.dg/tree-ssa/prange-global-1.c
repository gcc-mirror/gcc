/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-alias" } */

/* A pointer's global range used to be a single non-null bit.
   With pranges properly stored, we should pick up the mask.  */

extern void g (void *);

long *
f (long *q)
{
  if ((__UINTPTR_TYPE__) q & 15)
    __builtin_unreachable ();
  g (q);
  return q;
}

/* { dg-final { scan-tree-dump "prange.* MASK " "optimized" } } */
