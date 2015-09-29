/* PR target/pr65105 */
/* { dg-do run { target { ia32 } } } */
/* { dg-options "-O2 -march=slm" } */
/* { dg-final { scan-assembler "por" } } */
/* { dg-final { scan-assembler "pand" } } */

#include "stdlib.h"

static int count = 0;

void __attribute__((noinline))
counter (long long l)
{
  count++;
  if (!l || count > 5)
    exit (1);
}

void __attribute__((noinline))
test (long long *arr)
{
  register unsigned long long tmp;

  tmp = arr[0] | arr[1] & arr[2];
  while (tmp)
    {
      counter (tmp);
      tmp = *(arr++) & tmp;
    }
}

void  __attribute__((noinline))
fill_data (long long *arr)
{
  arr[0] = 0x00ffffffL;
  arr[1] = 0xffffff00L;
  arr[2] = 0x00ffffffL;
  arr[3] = 0x0000ff00L;
  arr[4] = 0x00ff0000L;
  arr[5] = 0xff000000L;
}

int
main (int argc, const char **argv)
{
  long long arr[6];
  fill_data (arr);
  test (arr);
  return count - 5;
}
