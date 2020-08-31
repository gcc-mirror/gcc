/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_condition } */

void foo (double *x, double *y, int m, int n, int o, int p)
{
  for (int i = 0; i < m; ++i)
    for (int j = 0; j < n; ++j)
      for (int k = 0; k < o; ++k)
	for (int l = 0; l < k; ++l)
	  {
	    double tem = x[l] + y[l];
	    if (tem != 0.)
	      y[l] = x[l];
	    else
	      y[l] = 0.;
	  }
}

/* Vectorization using partial vectors has zero versioning_threshold with
   either usage 1 or usage 2, the cond_expr replies on the computation in
   outer loop, so it doesn't need to reuse the loop version created by if
   conversion.  */
/* { dg-final { scan-tree-dump "reusing loop version created by if conversion" "vect" {target {! vect_partial_vectors } } } } */
