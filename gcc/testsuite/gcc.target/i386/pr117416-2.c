/* PR target/117416 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

#include <x86intrin.h>

void* p;

void extern
prefetch_test (void)
{
  __builtin_ia32_prefetch (p, 0, 0, 2); /* { dg-warning "invalid forth argument to '__builtin_ia32_prefetch'; using zero" } */
}
