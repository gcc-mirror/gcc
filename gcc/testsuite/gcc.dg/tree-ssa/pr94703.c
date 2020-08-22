/* { dg-do compile } */
/* { dg-require-effective-target non_strict_align } */
/* { dg-options "-O -fdump-tree-ssa" } */

unsigned int set_lowpart (unsigned int const *X)
{
  unsigned int r = 0;
  __builtin_memcpy(&r,X,sizeof (unsigned int) / 2);
  return r;
}

/* { dg-final { scan-tree-dump "No longer having address taken: r" "ssa" } } */
