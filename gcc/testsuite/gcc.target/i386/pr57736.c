/* PR target/57736 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

unsigned long long
f1 (void)
{
  return __rdtsc ();
}

unsigned long long
f2 (unsigned int *x)
{
  return __rdtscp (x);
}

unsigned long long
f3 (unsigned int x)
{
  return __rdpmc (x);
}

void
f4 (void)
{
  __rdtsc ();
}

void
f5 (unsigned int *x)
{
  __rdtscp (x);
}

void
f6 (unsigned int x)
{
  __rdpmc (x);
}
