// PR c++/2136
// Test that overloaded op new and delete don't prevent us from using the
// global versions with an explicit scope.

#include <stddef.h>

struct A {
  void *operator new (size_t, float);
  void operator delete (void *, float);
};

int main ()
{
  A *p = ::new A;
  ::delete p;
}
