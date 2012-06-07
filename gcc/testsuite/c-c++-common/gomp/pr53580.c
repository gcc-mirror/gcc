/* PR middle-end/53580 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int
main ()
{
  int x, y, v = 0;

#pragma omp parallel
  #pragma omp for
    for (x = 0; x < 10; x++)
      {
      #pragma omp for reduction(+: v)	/* { dg-error "work-sharing region may not be closely nested inside of work-sharing" } */
	for (y = 0; y < 10; y++)
	  v++;
      }
  return v - 100;
}
