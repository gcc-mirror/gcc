/* { dg-do compile } */

/* This test case is expected to fail due to errors.  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;

#pragma omp for collapse(4)
  for (i = 0; i < a1; i++)	/* { dg-error "not enough nested loops" } */
    {
      f1 (0, i);
      for (j = 0; j < a2; j++)
	{
	  f1 (1, j);
	  for (k = 0; k < a3; k++)
	    {
	      /* According to the grammar, this is intervening code; we
		 don't know that we are also missing a nested for loop
		 until we have parsed this whole compound expression.  */
#pragma omp barrier	/* { dg-error "intervening code must not contain OpenMP directives" } */
	      f1 (2, k);
	      f2 (2, k);
	    }
	  f2 (1, j);
	}
      f2 (0, i);
    }
}

