/* PR tree-optimization/107668 */
/* { dg-do compile } */
/* { dg-options "-ffast-math -fno-associative-math -fsanitize=float-cast-overflow -fno-guess-branch-probability -fsigned-zeros" } */

_Complex int c;
int i;

void
foo (void)
{
  c /= (_Complex) i;
}
