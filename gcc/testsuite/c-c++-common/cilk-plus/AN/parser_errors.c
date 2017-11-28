/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int array[10][10], array2[10];
  
  array2[:] = array2[: ;  /* { dg-error "expected ']'" } */

  return 0; /* { dg-error "expected ';' before" "" { target c } } */
}
