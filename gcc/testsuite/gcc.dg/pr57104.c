/* PR tree-optimization/57104 */
/* { dg-do compile { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } } */
/* { dg-options "-fsanitize=thread" } */

register int r asm ("r14");
int v;

int
foo (void)
{
  return r + v;
}
