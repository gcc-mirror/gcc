/* PR c/37261 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned
foo (int x)
{
  unsigned a = ((x & 1) | 2) & 0x80000000;	/* { dg-bogus "integer overflow in expression" } */
  unsigned b = ((x & 2) | 2) & 0x80000000;	/* { dg-bogus "integer overflow in expression" } */
  unsigned c = ((x & 4) | 2) & 0x80000000;	/* { dg-bogus "integer overflow in expression" } */
  return a + b + c;
}

/* { dg-final { scan-tree-dump "return 0" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
