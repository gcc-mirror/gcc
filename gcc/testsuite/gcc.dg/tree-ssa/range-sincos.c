// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp -fno-thread-jumps" }

#include <math.h>

void use (double);
void link_error ();

void
foo (double x)
{
  if (__builtin_isnan (x))
    __builtin_unreachable ();
  x = sin (x);
  if (x < -1.0 || x > 1.0)
    link_error ();
  use (x);
}

void
bar (double x)
{
  if (!__builtin_isnan (sin (x)))
    {
      if (__builtin_isnan (x))
	link_error ();
      if (__builtin_isinf (x))
	link_error ();
    }
}

void
stool (double x)
{
  double res1 = sin (x);
  double res2 = __builtin_sin (x);
  if (res1 < -1.0 || res2 < -1.0)
    link_error ();
  if (res1 > 1.0 || res2 > 1.0)
    link_error ();
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" { target { { *-*-linux* } && { glibc } } } } }
