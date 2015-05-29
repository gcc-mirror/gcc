/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

void bar (void *);
void foo(unsigned n)
{
  void *p = __builtin_malloc (n);
  if (p)
    {
      bar (p);
      __builtin_free (p);
      p = 0;
    }
  __builtin_free (p);
}

/* We should remove the redundant call to free.  */

/* { dg-final { scan-tree-dump-times "free" 1 "optimized" } } */
