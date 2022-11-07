/* { dg-do compile } */
/* { dg-options "-O0" } */

#include <x86intrin.h>

void* p;

void extern
prefetch_test (void)
{
  __builtin_ia32_prefetch (p, 0, 3, 0);
  __builtin_ia32_prefetch (p, 0, 2, 0);
  __builtin_ia32_prefetch (p, 0, 1, 0);
  __builtin_ia32_prefetch (p, 0, 0, 0);
  __builtin_ia32_prefetch (p, 1, 3, 0);
  __builtin_ia32_prefetch (p, 1, 2, 0);
  __builtin_ia32_prefetch (p, 1, 1, 0);
  __builtin_ia32_prefetch (p, 1, 0, 0);
}
