/* { dg-do compile } */
/* { dg-options "-fcilkplus -w" } */

/* We use -w because in the first error, there will be a warning of setting an
   integer to a pointer.  Just ignore it to expose the rank mismatch error.  */

int main (void)
{
  int x = 0;
  int array[10][10], array2[10];

  array[:][:] = array[:]; /* { dg-error "rank mismatch between" } */
  /* { dg-error "invalid conversion" "" { target c++ } .-1 } */

  x = array2[:]; /* { dg-error "cannot be scalar when" } */

  return 0;
}
