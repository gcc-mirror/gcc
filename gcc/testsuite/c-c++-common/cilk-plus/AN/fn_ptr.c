/* { dg-do compile } */
/* { dg-options "-fcilkplus" } */

typedef int (*foo)(int);

int main(void)
{
  int array[10], array2[10][10];
  foo func_array[10];
  foo func_array2[10][10];
  foo ***func_array_ptr;
  int argc = 5;

  array[:] =  func_array[:](10); 
  func_array[0:5](10); 
  func_array2[0:5][:](10);
  array2[0:5][:] = func_array2[0:5][:](10);
  func_array_ptr[0:5][0:4][0:argc:2](argc); 

  return 0;
}
