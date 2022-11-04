/* { dg-do run } */
/* PR/107115 */
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include <stdlib.h>

void test1(long *p1)
{
  p1[0] = 1;
}
long test2(long long *p2, int index1, int index2)
{
  p2[index1] = 2;
  return p2[index2];
}
long test3(long *p3, int index2, long value)
{
  p3[index2] = 3;
  p3[index2] = value;
  return p3[0];
}
long test4(void *p4, int index1, int index2)
{
  test1(p4);
  long temp = test2(p4, index1, index2);
  return test3(p4, index2, temp);
}
long (*volatile vtest)(void *, int, int) = test4;
int main(void)
{
  void *pp = malloc(sizeof (long) + sizeof(long long));
  if (!pp) abort();
  long result = vtest(pp, 0, 0);
  if (*(long *)pp != 2 || result != 2)
    __builtin_abort ();
  return 0;
}
