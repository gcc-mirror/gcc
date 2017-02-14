/* PR c/79431 */

void
foo (void)
{
  int a;
  #pragma omp declare target (a)
}
