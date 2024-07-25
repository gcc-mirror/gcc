/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mprefetchi -O0" } */
/* { dg-final { scan-assembler-times "\[ \\t\]+prefetchit0\[ \\t\]+bar\\(%rip\\)" 1 } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+prefetchit1\[ \\t\]+bar\\(%rip\\)" 1 } } */

#include <x86intrin.h>

int
bar (int a)
{
  return a + 1;
}

int
foo1 (int b)
{
  _m_prefetchit0 (bar);
  return bar (b) + 1;
}

int
foo2 (int b)
{
  _m_prefetchit1 (bar);
  return bar (b) + 1;
}
