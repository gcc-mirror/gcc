/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized-alias" } */

unsigned foo (void *p)
{
  unsigned i;
  __builtin_memcpy (&i, __builtin_assume_aligned (p, 4), sizeof (unsigned));
  return i;
}

/* Even when not optimizing we should have alignment info on the temporary
   feeding the memcpy.  */
/* { dg-final { scan-tree-dump "ALIGN = 4" "optimized" } } */
