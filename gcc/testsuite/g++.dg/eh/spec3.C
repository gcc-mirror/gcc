// PR c++/4381
// Test that exception-specs work properly for classes with virtual bases.

// { dg-do run }

class Base {};

struct A : virtual public Base
{
  A() {}
};

struct B {};

void func()
#if __cplusplus <= 201402L
throw (B,A)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++1z } } } }
#endif
{
  throw A();
}

int main(void)
{
  try {	func(); }
  catch (A& a) { }
}
