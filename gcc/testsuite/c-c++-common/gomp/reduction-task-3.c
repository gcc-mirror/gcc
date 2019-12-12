/* PR c/91149 */

int r;

void
foo (void)
{
  #pragma omp parallel reduction(task, +: r)
  r++;
  #pragma omp target parallel reduction(task, +: r)
  r++;
}
