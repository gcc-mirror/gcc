/* { dg-do compile }  */
/* { dg-options "-fpermissive -O2 -fdump-tree-optimized -w" }  */

void *arf ();
int
foo(void (*q)(void))
{
  int r = q;

  if (r != 0)
    return 1;
  else
    return 2;
}

/* The cast to an int type must remain after all optimizations are complete
   so that we do not try to canonicalize a function pointer for the
   comparison when no such canonicalization is wanted.  */
/* { dg-final { scan-tree-dump-times "r_. = \\(int\\) q" 1 "optimized" } } */

