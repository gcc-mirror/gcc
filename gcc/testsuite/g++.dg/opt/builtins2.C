// PR c++/49813
// We should handle asinh as a built-in in C++0x mode, even when strict.
// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "asinh" } }
// { dg-skip-if "requires hosted libstdc++ for math" { ! hostedlib } }

#include <math.h>

int main()
{
  double das = asinh(1.0);
}
