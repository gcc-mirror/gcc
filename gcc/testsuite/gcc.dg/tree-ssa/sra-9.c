/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

struct S
{
  int a, b, c;
  int z[20];
};

int foo (int d)
{
  struct S s;

  s.a = d;
  return s.a + s.b;
}

/* There should be no reference to s.b.  */
/* { dg-final { scan-tree-dump-times "= s\.b" 0 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
