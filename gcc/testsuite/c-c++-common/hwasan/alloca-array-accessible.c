/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

#define alloca __builtin_alloca

int __attribute__ ((noinline))
using_alloca (int num)
{
  int retval = 0;
  int *big_array = (int*)alloca (num * sizeof (int));
  for (int i = 0; i < num; ++i) {
      retval += big_array[i];
  }
  return retval;
}

int __attribute__ ((noinline))
using_vararray (int num)
{
  int retval = 0;
  int big_array[num];
  for (int i = 0; i < num; ++i) {
      retval += big_array[i];
  }
  return retval;
}

int main()
{
  using_alloca (16);
  using_vararray (12);
  return 0;
}
