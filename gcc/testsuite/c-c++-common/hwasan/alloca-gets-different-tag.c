/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */

/* Alloca is given a different tag to other variables.
   vararray should behave in the same way.  */

#define alloca __builtin_alloca
#define assert(x) if (!(x)) __builtin_abort ()

struct two_values {
    int left;
    int right;
};

/* Require default hwasan tag ABI.
   Know we're using AArch64 since that's the only architecture we run hwasan
   tests on.  */
char tag_of (void * x) { return ((unsigned long long)x) >> 56; }

int __attribute__ ((noinline))
alloca_different_tag (int num)
{
  struct two_values tmp_object = {
      .left = 100,
      .right = num,
  };
  int *big_array = (int *)alloca (num * sizeof (int));
  int other_array[100];
  
  char first_tag = tag_of (&tmp_object);
  char second_tag = tag_of (big_array);
  char other_tag = tag_of (other_array);
  assert (first_tag != second_tag);
  assert (second_tag != other_tag);
  assert (first_tag != other_tag);
  return 0;
}

int __attribute__ ((noinline))
vararray_different_tag (int num)
{
  struct two_values tmp_object = {
      .left = 100,
      .right = num,
  };
  int big_array[num];
  int other_array[100];
  
  char first_tag = tag_of (&tmp_object);
  char second_tag = tag_of (big_array);
  char other_tag = tag_of (other_array);
  assert (first_tag != second_tag);
  assert (second_tag != other_tag);
  assert (first_tag != other_tag);
  return 0;
}

int __attribute__ ((noinline))
main ()
{
  alloca_different_tag (10);
  vararray_different_tag (8);
  return 0;
}

