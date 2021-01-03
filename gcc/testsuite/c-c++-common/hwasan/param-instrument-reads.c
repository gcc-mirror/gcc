/* { dg-do compile } */
/* { dg-additional-options "--param hwasan-instrument-reads=0" } */

typedef __SIZE_TYPE__ size_t;
/* Particular code doesn't really matter, the requirement is that it has both
   loads and stores in it.  */
__attribute__ ((noinline))
int reader (int *array, size_t num)
{
  return array[num];
}

int __attribute__ ((noinline))
writer (int *array, size_t num, int value)
{
  array[num] = value;
  return num + value;
}

/* { dg-final { scan-assembler-not "__hwasan_load" } } */
/* { dg-final { scan-assembler "__hwasan_store" } } */
