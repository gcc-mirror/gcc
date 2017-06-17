/* Check local register variables using a register conventionally 
   used as the frame pointer aren't clobbered under high register pressure.  */
/* { dg-do run } */
/* { dg-skip-if "incompatible options" { ! { arm_thumb1_ok || arm_thumb2_ok } } } */
/* { dg-options "-Os -mthumb -fomit-frame-pointer" } */

#include <stdlib.h>

int global=5;

void __attribute__((noinline)) foo(int p1, int p2, int p3, int p4)
{
  if (global != 5 || p1 != 1 || p2 != 2 || p3 != 3 || p4 != 4)
    abort();
}

int __attribute__((noinline)) test(int a, int b, int c, int d)
{
  register unsigned long r __asm__("r7") = 0xdeadbeef;
  int e;

  /* ABCD are live after the call which should be enough
     to cause r7 to be used if it weren't for the register variable.  */
  foo(a,b,c,d);

  e = 0;
  __asm__ __volatile__ ("mov %0, %2"
			: "=r" (e)
			: "0" (e), "r" (r));

  global = a+b+c+d;

  return e;
}

int main()
{
  if (test(1, 2, 3, 4) != 0xdeadbeef)
    abort();
  if (global != 10)
    abort();
  return 0;
}
