/* PR c++/106448 */

int x, expr;
  
void
foo (void)
{
  #pragma omp atomic compare
  x = (expr > x) ? expr : x;	/* { dg-error "invalid (form|operator)" } */
  #pragma omp atomic compare
  x = (x < expr) ? expr : x;	/* { dg-error "invalid (form|operator)" } */
  #pragma omp atomic compare
  x = (x == expr) ? expr : x;	/* { dg-error "invalid (form|operator)" } */
}
