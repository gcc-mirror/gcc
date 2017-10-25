// PR c++/82307
// { dg-do run { target c++11 } }

#include <cassert>

enum : unsigned long long { VAL };

bool foo (unsigned long long) { return true; }
bool foo (int) { return false; }

int main()
{
  assert (foo(VAL));
}
