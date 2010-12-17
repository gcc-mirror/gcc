/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp2" } */

struct a {int a,b;};
const static struct a a;
static int b[10];
int c;
test()
{
  return a.a+b[c];
}
/* { dg-final { scan-tree-dump "return 0;" "ccp2" } } */
/* { dg-final { cleanup-tree-dump "ccp2" } } */
