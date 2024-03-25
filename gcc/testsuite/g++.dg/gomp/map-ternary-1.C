#include <cassert>

int foo (bool yesno)
{
  int x = 5, y = 7;
#pragma omp target map(yesno ? x : y)
  /* { dg-message {sorry, unimplemented: unsupported map expression '\(yesno \?  x :  y\)'} "" { target *-*-* } .-1 } */
  {
    x += 3;
    y += 5;
  }
  return yesno ? x : y;
}

int main (int argc, char *argv[])
{
  assert (foo (true) == 8);
  assert (foo (false) == 12);
  return 0;
}
