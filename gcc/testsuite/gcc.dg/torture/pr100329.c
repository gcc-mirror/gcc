/* { dg-do compile { target asm_goto_with_outputs } } */
/* { dg-additional-options "--param tree-reassoc-width=2" } */

unsigned int a0;

unsigned int
foo (unsigned int a1, unsigned int a2)
{
  unsigned int x;

  asm goto ("" : "=r" (x) : : : lab);
  a0 = x;

 lab:
  return x + a1 + a2 + 1;
}
