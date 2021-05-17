/* { dg-do compile } */
/* { dg-additional-options "--param tree-reassoc-width=2" } */

unsigned int foo_a1, foo_a2;

unsigned int foo()
{
  unsigned int v0, x;
  asm goto("" : "=r"(x) : : : lab);
lab:
  v0 += x + x;
  return v0 + x + foo_a1 + foo_a2;
}
