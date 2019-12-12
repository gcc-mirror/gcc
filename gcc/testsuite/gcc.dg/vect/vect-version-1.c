/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_condition } */

void foo (double *x, double *y, int m, int n, int o, int p)
{
  for (int i = 0; i < m; ++i)
    for (int j = 0; j < n; ++j)
      for (int k = 0; k < o; ++k)
	for (int l = 0; l < p; ++l)
	  {
	    double tem = x[l] + y[l];
	    if (tem != 0.)
	      y[l] = x[l];
	    else
	      y[l] = 0.;
	  }
}

/* { dg-final { scan-tree-dump "applying loop versioning to outer loop 1" "vect" } } */
