/* { dg-do compile } */

/* This test case is expected to fail due to errors.  */

/* Test that the imperfectly-nested loops with the ordered clause gives
   an error, and that there is only one error (and not one on every
   intervening statement).  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

  /* This loop without intervening code ought to be OK. */
#pragma omp for ordered(3)
  for (i = 0; i < a1; i++)
    {
      for (j = 0; j < a2; j++)
	{
	  for (k = 0; k < a3; k++)
	    {
	      f1 (2, k);
	      f2 (2, k);
#pragma omp ordered doacross(source:omp_cur_iteration)
#pragma omp ordered doacross(sink: i - 2, j + 2, k - 1)
	    }
	}
    }

  /* Now add intervening code.  */
#pragma omp for ordered(3)
  for (i = 0; i < a1; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      f1 (0, i);
      for (j = 0; j < a2; j++)
	{
	  f1 (1, j);
	  for (k = 0; k < a3; k++)
	    {
	      f1 (2, k);
	      f2 (2, k);
#pragma omp ordered doacross(source:omp_cur_iteration)
#pragma omp ordered doacross(sink: i - 2, j + 2, k - 1)
	    }
	  f2 (1, j);
	}
      f2 (0, i);
    }
}

