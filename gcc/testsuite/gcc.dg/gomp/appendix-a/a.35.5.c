/* { dg-do compile } */

void work (int, int);

void
wrong5 (int n)
{
#pragma omp parallel
  {
#pragma omp critical
    {
      work (n, 0);
/* incorrect nesting of barrier region in a critical region */
#pragma omp barrier	/* { dg-error "may not be closely nested" } */
      work (n, 1);
    }
  }
}
