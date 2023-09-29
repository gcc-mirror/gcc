/* { dg-do compile { target c++11 } } */

/* This test case is expected to fail due to errors.  */

/* These functions that are part of the OpenMP runtime API would ordinarily
   be declared in omp.h, but we don't have that here.  */
extern int omp_get_num_threads(void);
extern int omp_get_max_threads(void);

int f1 (int depth, int iter);
int f2 (int depth, int iter);

void s1 (int a1, int a2, int a3)
{
  int i, j, k;
  [[ omp :: directive (for, collapse(3)) ]]
  for (i = 0; i < a1; i++)
    {
      f1 (0, i);
      for (j = 0; j < omp_get_num_threads (); j++)  /* This is OK */
	{
	  f1 (1, omp_get_num_threads ());  /* { dg-error "not permitted in intervening code" } */
	  for (k = omp_get_num_threads (); k < a3; k++)  /* This is OK */
	    {
	      f1 (2, omp_get_num_threads ());
	      f2 (2, omp_get_max_threads ());
	    }
	  f2 (1, omp_get_max_threads ());  /* { dg-error "not permitted in intervening code" } */
	}
      f2 (0, i);
    }
}


