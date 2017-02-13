/* PR rtl-optimization/79388 */
/* { dg-additional-options "-fno-tree-coalesce-vars" } */

unsigned int a, c;

__attribute__ ((noinline, noclone)) unsigned int
foo (unsigned int p)
{
  p |= 1;
  p &= 0xfffe;
  p %= 0xffff;
  c = p;
  return a + p;
}

int
main (void)
{
  int x = foo (6);
  if (x != 6)
    __builtin_abort();
  return 0;
}
