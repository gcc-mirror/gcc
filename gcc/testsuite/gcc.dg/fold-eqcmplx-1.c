/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int foo(float x, float y)
{
  return (_Complex float)x == (_Complex float)y;
}

/* { dg-final { scan-tree-dump-times "x == y" 1 "original" } } */
