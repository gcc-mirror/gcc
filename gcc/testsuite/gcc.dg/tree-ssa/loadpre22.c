/* { dg-do compile } */ 
/* { dg-options "-O2 -fno-tree-loop-im -fdump-tree-pre-stats" } */
typedef int type[2];
int main(type *a, int argc)
{
  int i;
  int e;

  /* Should be able to hoist this out of the loop.  */
  for (i = 0; i < argc; i++)
    {
      e = (*a)[argc];
    }
  return e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre"} } */
