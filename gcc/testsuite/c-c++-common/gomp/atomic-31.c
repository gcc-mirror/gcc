/* c/104531 */
/* { dg-do compile } */

int x;

void
foo (_Complex int y)
{
  #pragma omp atomic compare	/* { dg-error "invalid operands" } */
  x = x > y ? y : x;
}
