// { dg-do compile }
// { dg-options "-O2 -fdump-tree-evrp -fno-thread-jumps" }

#include <math.h>

void use (double);
void link_error ();

void
foo (double x)
{
  if (x < 1.0 || x > 9.0)
    __builtin_unreachable ();
  x = sqrt (x);
  if (x < 0.875 || x > 3.125)
    link_error ();
  use (x);
}

void
bar (double x)
{
  if (sqrt (x) >= 2.0 && sqrt (x) <= 4.0)
    {
      if (__builtin_isnan (x))
	link_error ();
      if (x < 3.875 || x > 16.125)
	link_error ();
    }
}

void
stool (double x)
{
  if (x >= 64.0)
    {
      double res1 = sqrt (x);
      double res2 = __builtin_sqrt (x);
      if (res1 < 7.875 || res2 < 7.875)
	link_error ();
    }
}

// { dg-final { scan-tree-dump-not "link_error" "evrp" { target { { *-*-linux* } && { glibc } } } } }
