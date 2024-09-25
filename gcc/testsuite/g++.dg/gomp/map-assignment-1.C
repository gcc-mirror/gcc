// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

int main (int argc, char *argv[])
{
  int a = 5, b = 2;
#pragma omp target map(a += b)
  /* { dg-message {sorry, unimplemented: unsupported map expression '\(a = \(a \+ b\)\)'} "" { target *-*-* } .-1 } */
  {
    a++;
  }
  return 0;
}
