/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized"  } */
int p,q,r,s,*ptr=&q, *ptr2=&p;
__attribute__ ((noinline))
int
test (int *p)
{
  *p = 1;
}
int
test1()
{
  q = 123;
  test(&p);
  return q;
}
int
test2()
{
  int *ptr = p ? &q : &s;
  *ptr = 124;
  test(&p);
  return *ptr;
}
int
test3()
{
  int *ptr = p ? &p : &s;
  q = 125;
  test(ptr);
  return q;
}
int
test4()
{
  int *ptr1 = p ? &q : &s;
  int *ptr = p ? &r : &p;
  *ptr1 = 126;
  test(ptr);
  return *ptr1;
}
/* { dg-final { scan-tree-dump "return 123" "optimized"} } */
/* { dg-final { scan-tree-dump "return 124" "optimized"} } */
/* { dg-final { scan-tree-dump "return 125" "optimized"} } */
/* { dg-final { scan-tree-dump "return 126" "optimized"} } */
