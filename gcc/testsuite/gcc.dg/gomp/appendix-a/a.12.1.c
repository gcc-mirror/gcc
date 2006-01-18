/* { dg-do compile } */

#include <stdio.h>
extern float average (float, float, float);
void
a12 (float *x, float *xold, int n, float tol)
{
  int c, i, toobig;
  float error, y;
  c = 0;
#pragma omp parallel
  {
    do
      {
#pragma omp for private(i)
	for (i = 1; i < n - 1; ++i)
	  {
	    xold[i] = x[i];
	  }
#pragma omp single
	{
	  toobig = 0;
	}
#pragma omp for private(i,y,error) reduction(+:toobig)
	for (i = 1; i < n - 1; ++i)
	  {
	    y = x[i];
	    x[i] = average (xold[i - 1], x[i], xold[i + 1]);
	    error = y - x[i];
	    if (error > tol || error < -tol)
	      ++toobig;
	  }
#pragma omp master
	{
	  ++c;
	  printf ("iteration %d, toobig=%d\n", c, toobig);
	}
      }
    while (toobig > 0);
  }
}
