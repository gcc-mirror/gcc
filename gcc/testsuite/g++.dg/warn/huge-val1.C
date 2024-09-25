// PR c++/23139: HUGE_VAL definition should be accepted with -pedantic.
// Origin: Joseph Myers <joseph@codesourcery.com>
// { dg-do link }
// { dg-options "-pedantic-errors" }
// { dg-skip-if "requires hosted libstdc++ for cmath" { ! hostedlib } }

#include <math.h>

double d = HUGE_VAL;
#ifdef HUGE_VALF
float f = HUGE_VALF;
#endif
#ifdef HUGE_VALL
long double l = HUGE_VALL;
#endif

extern void link_failure ();

int
main ()
{
#ifdef __GLIBC__
  if (HUGE_VAL != __builtin_huge_val ())
    link_failure ();
#ifdef HUGE_VALF
  if (HUGE_VALF != __builtin_huge_valf ())
    link_failure ();
#endif
#ifdef HUGE_VALL
  if (HUGE_VALL != __builtin_huge_vall ())
    link_failure ();
#endif
#endif
}
