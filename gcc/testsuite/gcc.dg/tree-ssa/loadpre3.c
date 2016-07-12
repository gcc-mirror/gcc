/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */

extern void spoil (void);

int foo(int **a,int argc)
{
  int b;
  int d, e;

  if (argc)
    {
      d = *(*a);
    }
  else
    {
      /* Spoil *a and *(*a) to avoid hoisting it before the "if (...)".  */
      spoil ();
    }
  /* Should be able to eliminate one of the *(*a)'s along the if path
     by pushing it into the else path. We will also eliminate
     one of the *a's.  */
  e = *(*a);
  return d + e;
}

/* { dg-final { scan-tree-dump-times "Eliminated: 2" 1 "pre"} } */
