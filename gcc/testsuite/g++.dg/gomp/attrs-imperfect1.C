/* { dg-do compile { target c++11 } } */

/* This test case is expected to fail due to errors.  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

  [[ omp :: directive (for, collapse(3)) ]]
  for (i = 0; i < a1; i++)
    {
      f1 (0, i);
      for (j = 0; j < a2; j++)
	{
	  [[ omp :: directive (barrier) ]] ;	/* { dg-error "intervening code must not contain OpenMP directives" } */
	  f1 (1, j);
	  if (i == 2)
	    continue;	/* { dg-error "invalid exit" } */
	  else
	    break;	/* { dg-error "invalid exit" } */
	  for (k = 0; k < a3; k++)
	    {
	      f1 (2, k);
	      f2 (2, k);
	    }
	  f2 (1, j);
	}
      for (k = 0; k < a3; k++)	/* { dg-error "loop not permitted in intervening code " } */
	{
	  f1 (2, k);
	  f2 (2, k);
	}
      f2 (0, i);
    }
}
