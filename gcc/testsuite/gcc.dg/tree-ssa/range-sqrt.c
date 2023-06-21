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
  x = sqrt (x);
  if (x < -0.0)
    link_error ();
  use (x);
}

void
bar (double x)
{
  if (!__builtin_isnan (sqrt (x)))
    {
      if (__builtin_isnan (x))
	link_error ();
      if (x < -0.0)
	link_error ();
    }
}

void
stool (double x)
{
  double res1 = sqrt (x);
  double res2 = __builtin_sqrt (x);
  if (res1 < -0.0 || res2 < -0.0)
    link_error ();
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" { target { { *-*-linux* } && { glibc } } } } }
