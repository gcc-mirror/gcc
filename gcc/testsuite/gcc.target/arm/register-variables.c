/* { dg-do run } */
/* { dg-options "-O" } */

#include <stdlib.h>

void __attribute__((noinline)) 
bar(int a, int b)
{
  if (a != 43 || b != 42)
    abort();
}

int main(void)
{
    register int r0 asm("r0") = 42;
    register int r1 asm("r1") = 43;
    asm volatile("": "+r" (r0), "+r" (r1));
    bar(r1, r0);
    return 0;
}

