// Special g++ Options: -O2

#include <iostream.h>

ostream& foo (char *x, ostream &y)
{
  return y << "" << x;
}

int main ()
{
  foo ("", cout);
}
