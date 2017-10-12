/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int array[10][10], array2[10];
  
  array2[:] = array2[1:2:] ;  /* { dg-error "expected expression before" "" { target c } } */ 
  /* { dg-error  "expected primary-expression before" "" { target c++ } .-1 } */
  /* { dg-error "expected ';' before" "" { target c } .-2 } */

  return 0;
}
