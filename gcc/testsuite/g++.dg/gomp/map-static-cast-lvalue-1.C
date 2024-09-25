// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

int foo (int x)
{
#pragma omp target map(static_cast<int&>(x))
  /* { dg-message {sorry, unimplemented: unsupported map expression '& x'} "" { target *-*-* } .-1 } */
  {
    x += 3;
  }
  return x;
}

int main (int argc, char *argv[])
{
  assert (foo (5) == 8);
  return 0;
}
