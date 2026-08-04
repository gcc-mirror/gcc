/* PR tree-optimization/126486 */
/* { dg-do run { target lp64 } } */
/* { dg-options "-O2 -fdump-tree-ivopts-details" } */

__attribute__((noipa))
int
foo (int n_dims, int *actual_indices, const long *input_array_dims,
     int max_iter)
{
  int sum = 0;
  int iter = 0;

  while (iter < max_iter)
    {
      sum = actual_indices[0];
      for (int k = n_dims - 1; k > 0; k--)
	{
	  int product = 1;
	  for (int j = k - 1; j >= 0; j--)
	    product *= input_array_dims[j];
	  sum += actual_indices[k] * product;
	}
      iter++;
    }
  return sum;
}

int
main (void)
{
  int actual_indices[] = { 1, 2, 3 };
  long input_array_dims[] = { 4, 5, 6 };

  if (foo (3, actual_indices, input_array_dims, 2) != 69)
    __builtin_abort ();
  return 0;
}

/* The common N_DIMS term should cancel.  */
/* { dg-final { scan-tree-dump-times "inv_expr \[0-9\]+:\[^\n\]*\\(signed long\\) input_array_dims_\[0-9\]+\\(D\\) \\+ 8" 1 "ivopts" } } */

/* The unsigned K - 1 term should cancel.  */
/* { dg-final { scan-tree-dump-not "inv_expr \[0-9\]+:\[^\n\]*\\(unsigned int\\) k_\[0-9\]+ \\+ 4294967295" "ivopts" } } */
