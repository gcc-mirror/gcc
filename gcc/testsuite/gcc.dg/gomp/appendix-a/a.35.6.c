/* { dg-do compile } */

void
wrong6 (int n)
{
#pragma omp parallel
  {
#pragma omp single
    {
      work (n, 0);
/* incorrect nesting of barrier region in a single region */
#pragma omp barrier	/* { dg-error "may not be closely nested" } */
      work (n, 1);
    }
  }
}
