/* PR target/124366 */
/* { dg-do assemble { target { cet && masm_intel } } } */
/* { dg-options "-O2 -mshstk -masm=intel" } */

#include <x86intrin.h>

void
wrssd (unsigned int x, void *y)
{
  _wrssd (x, y);
}

void
wrussd (unsigned int x, void *y)
{
  _wrussd (x, y);
}

#ifdef __x86_64__
void
wrssq (unsigned long long x, void *y)
{
  _wrssq (x, y);
}

void
wrussq (unsigned long long x, void *y)
{
  _wrussq (x, y);
}
#endif
