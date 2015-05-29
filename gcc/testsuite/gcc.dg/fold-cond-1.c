/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

_Bool test1(int a, int b)
{
  return a ? b : 0;
}

_Bool test2(int c, int d)
{
  return c ? d : 1;
}

_Bool test3(int e, int f)
{
  return e ? 0 : f;
}

_Bool test4(int g, int h)
{
  return g ? 1 : h;
}

/* { dg-final { scan-tree-dump-times "a != 0 \&\& b != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c == 0 \\|\\| d != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e == 0 \&\& f != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "g == 0 \\? h != 0 : 1" 0 "original" } } */
/* { dg-final { scan-tree-dump-times "g != 0 \\? 1 : h != 0" 0 "original" } } */
