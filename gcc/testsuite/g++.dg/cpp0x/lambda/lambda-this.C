// Test that implicit 'this' capture works, but that it's still an rvalue.
// { dg-do compile { target c++11 } }

struct A
{
  int i;
  void f()
  {
    [=] { i = 0; };
    [&] { i = 0; };
    [=] { this = 0; };		// { dg-error "lvalue" }
  }
};
