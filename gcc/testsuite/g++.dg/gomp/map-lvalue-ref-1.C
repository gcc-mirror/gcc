#include <cassert>

int glob = 10;

int& foo ()
{
  return glob;
}

int main (int argc, char *argv[])
{
#pragma omp target map(foo())
  /* { dg-message {sorry, unimplemented: unsupported map expression 'foo\(\)'} "" { target *-*-* } .-1 } */
  {
    foo()++;
  }
  assert (glob == 11);
  return 0;
}
