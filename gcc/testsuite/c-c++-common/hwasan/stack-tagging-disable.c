/* { dg-do compile } */
/* { dg-additional-options "--param hwasan-instrument-stack=0" } */


/* No stack tagging => no calls to __hwasan_tag_memory.  */
int __attribute__ ((noinline))
accessing_pointers (int *left, int *right)
{
  int x = right[2];
  left[3] = right[1];
  return right[1] + left[2];
}

int __attribute__ ((noinline))
using_stack (int num)
{
  int big_array[10];
  int other_array[20];
  accessing_pointers(other_array, big_array);
  return big_array[num];
}

#ifndef ARG
#define ARG 0
#endif
int __attribute__ ((noinline))
main ()
{
  using_stack (ARG);
  return 0;
}

/* { dg-final { scan-assembler-not "__hwasan_tag_memory" } } */
