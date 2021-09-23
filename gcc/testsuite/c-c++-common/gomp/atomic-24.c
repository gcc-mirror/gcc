/* PR c/101297 */

int i;

void
foo (void)
{
  #pragma omp atomic update,	/* { dg-error "expected end of line before ',' token" } */
  i++;
  #pragma omp atomic update,,	/* { dg-error "expected end of line before ',' token" } */
  i++;
}
