/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-vars" } */

int &f(int *a)
{
  return *a;
}

/* There should be no cast as pointer and references are
   considered the same type. */
/* { dg-final { scan-tree-dump-times "\\(int &\\)" 0 "vars"} } */

