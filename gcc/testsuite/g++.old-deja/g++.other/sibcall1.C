// Special g++ Options: -O2

#include <iostream>

ostream& foo (char *x, ostream &y)
{
  return y << "" << x;
}

int main ()
{
  foo ("", cout);
}
