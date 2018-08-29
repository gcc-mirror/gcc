/* PR c++/86025 */
/* { dg-do compile } */
/* { dg-additional-options "-Wduplicated-branches" } */

int i;

void
foo (int x)
{
  if (x)
    {
      #pragma omp critical (foo)
      i++;
    }
  else
    {
      #pragma omp critical
      i++;
    }
}
