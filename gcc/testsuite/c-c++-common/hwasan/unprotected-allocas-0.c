/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-additional-options "--param hwasan-instrument-allocas=0 -save-temps" } */
/* Only run this test without optimisation.  When running with optimisation we
   use the unprotected-allocas-1.c file that also checks there are no memory
   tagging calls (since when optimised the only variable on the stack should be
   the vararray/alloca).  */
/* { dg-skip-if "" { *-*-* } { "-O1" "-O2" "-O3" } { "" } } */

#define alloca __builtin_alloca
#define assert(x) if (!(x)) __builtin_abort ()

char tag_of (void * x) { return ((unsigned long long)x) >> 56; }

int __attribute__ ((noinline))
using_alloca (int num)
{
  int retval = 0;
  int *big_array = (int*)alloca (num * sizeof (int));
  char alloca_tag = tag_of (big_array);
  assert (alloca_tag == 0);
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
  char vararray_tag = tag_of (big_array);
  assert (vararray_tag == 0);
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
