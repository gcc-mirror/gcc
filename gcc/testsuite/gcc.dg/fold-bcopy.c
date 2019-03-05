/* PR tree-optimization/80933 - redundant bzero/bcopy calls not eliminated
   { dg-do compile }
   { dg-options "-O1 -Wall -fdump-tree-lower" } */

void f0 (void *dst, const void *src, unsigned n)
{
  /* Bcopy(src, dst, ...) corresponds to memmove(dst, src, ...),
     with the first two arguments transposed, not memcpy.  */
  __builtin_bcopy (src, dst, n);
}

void f1 (void *p, const void *q, unsigned n)
{
  /* A call with zero size should be eliminated.  */
  __builtin_bcopy (q, p, 0);
}

int f2 (const void *p, const void *q, unsigned n)
{
  return __builtin_bcmp (p, q, n);
}

int f3 (const void *p, const void *q)
{
  /* A call with zero size should be folded into 0.  */
  return __builtin_bcmp (p, q, 0);
}

int f4 (const void *p, unsigned n)
{
  /* A call with the same argument should also be folded into 0.  */
  return __builtin_bcmp (p, p, n);
}

void f5 (void *p, unsigned n)
{
  __builtin_bzero (p, n);
}

void f6 (void *p)
{
  /* A call with zero size should be eliminated.  */
  __builtin_bzero (p, 0);
}

/* Verify that calls to bcmp, bcopy, and bzero have all been removed
   and one of each replaced with memcmp, memmove, and memset, respectively.
   The remaining three should be eliminated.
  { dg-final { scan-tree-dump-not "bcmp|bcopy|bzero" "lower" } }
  { dg-final { scan-tree-dump-times "memcmp|memmove|memset" 3 "lower" } }

  Verify that the bcopy to memmove transformation correctly transposed
  the source and destination pointer arguments.
  { dg-final { scan-tree-dump-times "memmove \\(dst, src" 1 "lower" } }  */
