// Special g++ Options: -O2

#include <iostream>

std::ostream& foo (char *x, std::ostream &y)
{
  return y << "" << x;
}

int main ()
{
  foo ("", std::cout);
}
