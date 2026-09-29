/* { dg-do compile } */

/* Check that an iterator whose trip count is not known at compile time does not
   ICE.  */

void f (int n, int *arr)
{
  #pragma omp target update to(iterator(i=0:n): arr[i]) /* { dg-message "sorry, unimplemented: dynamic iterator sizes not implemented yet" } */
}

void f1 (int s, int *arr)
{
  #pragma omp target update to(iterator(i=0:9:s): arr[i]) /* { dg-message "sorry, unimplemented: dynamic iterator sizes not implemented yet" } */
}

void f2 (int n, int *arr)
{
  #pragma omp target update from(iterator(i=0:n): arr[i]) /* { dg-message "sorry, unimplemented: dynamic iterator sizes not implemented yet" } */
}
