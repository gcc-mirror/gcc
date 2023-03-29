/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

struct { int data[16]; } x;

int foo (int n)
{
  int *p = x.data + n;
  /* Should simplify this to n * 4 != 0.  */
  if ((void *)&x != (void *)p)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump " != 0" "forwprop1" } } */
