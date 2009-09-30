// Test that implicit 'this' capture works, but that it's still an rvalue.
// { dg-options -std=c++0x }

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
