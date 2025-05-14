/* PR tree-optimization/119483 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2" } */

[[noreturn]] int
foo (int x)
{
  if (x > 10)
    [[gnu::musttail]] return foo (x - 1);	/* { dg-warning "function declared 'noreturn' has a 'return' statement" } */
  for (;;)
    ;
}
