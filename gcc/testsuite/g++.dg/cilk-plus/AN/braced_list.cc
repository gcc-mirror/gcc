/* { dg-do compile { target c++11 } } */
/* { dg-options "-fcilkplus" } */

int main (void)
{
  int Array[100], Array2[100];

  Array[{1,2}:2] = 5; /* { dg-error "braced list index is not allowed" } */
  Array[1:{1,2}:2] = 5; /* { dg-error "expected primary-expression before" } */
  Array[1:10:{1,2}] = 5; /* { dg-error "expected primary-expression before" } */

  return 0;
}
