/* { dg-do run } */
/* { dg-options "-fcilkplus" } */

int add_all (int *my_array, int size)
{
  return __sec_reduce_add (my_array[0:size]);
}

int mult_all (int *my_array, int size)
{
  return __sec_reduce_mul (my_array[0:size]);
}

int main (void)
{
  int argc = 1;
  int array[10000];
  
  __asm volatile ("" : "+r" (argc));
  array[:] = argc; /* All elements should be one.  */

  if (add_all (array, 10000) != 10000)
    return 1;

  if (mult_all (array, 10000) != 1)
    return 2;

  return 0;
}
