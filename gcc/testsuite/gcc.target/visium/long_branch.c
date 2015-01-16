/* { dg-do assemble } */

volatile int k = 0;

#define ONE k++;
#define TEN ONE ONE ONE ONE ONE ONE ONE ONE ONE ONE
#define HUN TEN TEN TEN TEN TEN TEN TEN TEN TEN TEN
#define THO HUN HUN HUN HUN HUN HUN HUN HUN HUN HUN
#define TTH THO THO THO THO THO THO THO THO THO THO THO

void foo (void)
{
  start:
  TTH
  __asm__ __volatile__ ("" : : : "r28");
  goto start;
}

#ifndef __OPTIMIZE__
void bar (int i)
{
  if (i > 0)
    {
      TTH
    }
}
#endif
