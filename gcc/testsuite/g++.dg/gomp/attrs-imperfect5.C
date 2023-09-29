/* { dg-do compile { target c++11 } } */

/* This test case is expected to fail due to errors.  */

int f1 (int depth, int iter);
int f2 (int depth, int iter);
int ijk (int x, int y, int z);
void f3 (int sum);

/* This function isn't particularly meaningful, but it should compile without
   error.  */
int s1 (int a1, int a2, int a3)
{
  int i, j, k;
  int r = 0;

  [[ omp :: directive (simd, collapse(3), reduction (inscan, +:r)) ]]
  for (i = 0; i < a1; i++)
    {
      for (j = 0; j < a2; j++)
	{
	  for (k = 0; k < a3; k++)
	    {
	      r = r + ijk (i, j, k);
	      [[ omp :: directive (scan, exclusive (r)) ]] ;
	      f3 (r);
	    }
	}
    }
  return r;
}

/* Adding intervening code should trigger an error.  */
int s2 (int a1, int a2, int a3)
{
  int i, j, k;
  int r = 0;

  [[ omp :: directive (simd, collapse(3), reduction (inscan, +:r)) ]]
  for (i = 0; i < a1; i++)  /* { dg-error "inner loops must be perfectly nested" } */
    {
      f1 (0, i);
      for (j = 0; j < a2; j++)
	{
	  f1 (1, j);
	  for (k = 0; k < a3; k++)
	    {
	      r = r + ijk (i, j, k);
	      [[ omp :: directive (scan, exclusive (r)) ]] ;
	      f3 (r);
	    }
	  f2 (1, j);
	}
      f2 (0, i);
    }
  return r;
}
