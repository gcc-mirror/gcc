/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mprefetchi -O2" } */
/* { dg-final { scan-assembler-times "\[ \\t\]+prefetchit0\[ \\t\]+bar\\(%rip\\)" 2 } } */
/* { dg-final { scan-assembler-times "\[ \\t\]+prefetchit1\[ \\t\]+bar\\(%rip\\)" 2 } } */

#include <x86intrin.h>

int
bar (int a)
{
  return a + 1;
}

int
foo1 (int b)
{
  _mm_prefetch (bar, _MM_HINT_IT0);
  return bar (b) + 1;
}

int
foo2 (int b)
{
  _mm_prefetch (bar, _MM_HINT_IT1);
  return bar (b) + 1;
}

int
foo3 (int b)
{
  _m_prefetchit0 (bar);
  return bar (b) + 1;
}

int
foo4 (int b)
{
  _m_prefetchit1 (bar);
  return bar (b) + 1;
}
