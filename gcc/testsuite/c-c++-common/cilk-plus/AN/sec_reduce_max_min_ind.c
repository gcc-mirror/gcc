/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int array[10][10], array2[10];
  int x, y;
  x = __sec_reduce_max_ind (array[:][:]); /* { dg-error "cannot have arrays with dimension greater than" } */

  y = __sec_reduce_max_ind (array2[:]); /* this should be OK. */

  x = __sec_reduce_min_ind (array[:][:]); /* { dg-error "cannot have arrays with dimension greater than" } */

  y = __sec_reduce_min_ind (array2[:]); /* this should be OK. */

  return 0;
}
