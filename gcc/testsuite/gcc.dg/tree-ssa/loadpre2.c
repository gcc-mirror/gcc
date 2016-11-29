/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-loop-im -fdump-tree-pre-stats" } */
int main(int *a, int argc)
{
  int i;
  int e;

  /* Should be able to hoist this out of the loop.  */
  for (i = 0; i < argc; i++)
    {
      e = *a;
    }
  return e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
