// PR c++/4381
// Test that exception-specs work properly for classes with virtual bases.

// { dg-do run }

class Base {};

struct A : virtual public Base
{
  A() {}
};

struct B {};

void func() throw (B,A)
{
  throw A();
}

int main(void)
{
  try {	func(); }
  catch (A& a) { }
}
