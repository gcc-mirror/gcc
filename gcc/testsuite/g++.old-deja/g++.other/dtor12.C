// Test that we don't complain about calling a destructor on a const object.

#include <new>

struct A
{
  ~A() {}
};

const A a = {};
int main()
{
  a.~A();
  a.A::~A();			// gets bogus error - const violation
}
