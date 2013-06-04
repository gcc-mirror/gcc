/* { dg-options "-fcilkplus" } */

typedef int (*foo)(int);

int main(void)
{
  int array[10], array2[10][10];
  foo func_array[10];
  foo func_array2[10][10];
  foo ***func_array_ptr;
  int argc = 5;

  array[:] =  func_array[:](10); /* { dg-error "array notations cannot be used with function pointer arrays" } */
  func_array[0:5](10); /* { dg-error "array notations cannot be used with function pointer arrays" } */
  func_array2[0:5][:](10); /* { dg-error "array notations cannot be used with function pointer arrays" } */
  array2[0:5][:] = func_array2[0:5][:](10); /* { dg-error "array notations cannot be used with function pointer arrays" } */
  func_array_ptr[0:5][0:4][0:argc:2](argc); /* { dg-error "array notations cannot be used with function pointer arrays" } */

  return 0;
}
