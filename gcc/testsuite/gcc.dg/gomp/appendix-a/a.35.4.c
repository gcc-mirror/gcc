/* { dg-do compile } */

void
wrong4 (int n)
{
#pragma omp parallel default(shared)
  {
    int i;
#pragma omp for
    for (i = 0; i < n; i++)
      {
	work (i, 0);
	/* incorrect nesting of barrier region in a loop region */
#pragma omp barrier	/* { dg-error "may not be closely nested" } */
	work (i, 1);
      }
  }
}
