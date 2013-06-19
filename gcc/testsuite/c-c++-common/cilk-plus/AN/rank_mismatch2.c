/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int function_call (int x);
int function_call (int x)
{
  return x;
}

int main (void)
{
  int array[100], array2[100][100];
  int argc = 4;
  array[:] = array[:] + array2[:][:]; /* { dg-error "rank mismatch between" } */

  if (array[:] + array2[:][:]) /* { dg-error "rank mismatch between" } */
    return argc == 5;

  argc += function_call (array[:] + array2[5:10:2][:]); /* { dg-error "rank mismatch between" } */

  argc += function_call (function_call (array[:] + array2[5:10:2][:])); /* { dg-error "rank mismatch between" } */

   argc += __sec_reduce_add (array[:], array2[:][:]); /* { dg-error "rank mismatch between" } */

   argc += __sec_reduce_add (array2[:][:]) + argc; /* This is OK.  */
  return argc;
}
