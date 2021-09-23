/* { dg-do compile { target vect_int } } */
/* { dg-additional-options "-fopt-info-vec-all -O3" } */

extern void accumulate (int x, int *a);

int test_missing_function_defn (int *arr, int n) /* { dg-note "5: vectorized 0 loops in function" } */
/* { dg-prune-output "note: " } as we're not interested in matching any further
   notes.  */
{
  int sum = 0;
  for (int i = 0; i < n; ++i) /* { dg-missed "21: couldn't vectorize loop" } */
    accumulate (arr[i], &sum); /* { dg-missed "5: statement clobbers memory: accumulate \\(.*\\);" } */
  return sum;
}
