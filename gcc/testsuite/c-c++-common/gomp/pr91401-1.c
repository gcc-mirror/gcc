/* PR c/91401 */

void
foo (void)
{
  int i;
  #pragma omp distribute parallel for schedule (static) dist_schedule (static)
  for (i = 0; i < 64; i++)
    ;
}
