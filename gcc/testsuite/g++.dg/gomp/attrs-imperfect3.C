/* { dg-do compile { target c++11 } } */

/* This test case is expected to fail due to errors.  */

/* Test that the imperfectly-nested loops with the ordered clause gives
   an error, and that there is only one error (and not one on every
   intervening statement).  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

  [[ omp :: directive (for, ordered(3)) ]]
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
	    }
	  f2 (1, j);
	}
      f2 (0, i);
    }
}

