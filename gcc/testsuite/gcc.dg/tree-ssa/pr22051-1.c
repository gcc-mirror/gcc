/* { dg-do compile }  */
/* { dg-options "-O2 -fdump-tree-optimized" }  */


void *arf ();
int
foo()
{
  void *p = arf ();

  if ((void (*)(void))p != 0)
    return 1;
  else
    return 2;
}

/* The cast to a function pointer type must remain after all optimizations
   are complete so that function pointer canonicalization works on those
   targets which require it.  */
/* { dg-final { scan-tree-dump-times "= \\(void \\(\\*<.*>\\) \\(void\\)\\) p_" 1 "optimized" } } */


