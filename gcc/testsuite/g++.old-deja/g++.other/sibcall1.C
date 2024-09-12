// { dg-do run  }
// { dg-options "-O2" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>

std::ostream& foo (const char *x, std::ostream &y)
{
  return y << "" << x;
}

int main ()
{
  foo ("", std::cout);
}
