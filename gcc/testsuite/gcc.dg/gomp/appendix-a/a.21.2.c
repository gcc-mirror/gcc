/* { dg-do compile } */

void
work (int i)
{
}

void
a21_wrong (int n)
{
  int i;
#pragma omp for ordered
  for (i = 0; i < n; i++)
    {
/* incorrect because an iteration may not execute more than one
    ordered region */
#pragma omp ordered
      work (i);
#pragma omp ordered
      work (i + 1);
    }
}
