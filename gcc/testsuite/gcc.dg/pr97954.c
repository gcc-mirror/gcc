/* PR rtl-optimization/97954 */
/* { dg-do compile { target asm_goto_with_outputs } } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int x;
 lab:
  asm goto ("": "=r" (x) : : : lab);
  return x;
}
