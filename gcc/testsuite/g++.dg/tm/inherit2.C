// Testcase from TM TS
// { dg-options "-std=c++14 -fgnu-tm" }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }

#include <iostream>

struct B {
  virtual void f() transaction_safe;
  virtual ~B() transaction_safe_dynamic;
};
// pre-existing code
struct D1 : B
{
  void f() override { } // ok
  ~D1() override { } // ok
};
struct D2 : B
{
  void f() override { std::cout << "D2::f" << std::endl; } // { dg-error "" "transaction-safe f has transaction-unsafe definition" }
  ~D2() override { std::cout << "~D2" << std::endl; } // ok
};
int main()
{
  D2 * d2 = new D2;
  B * b2 = d2;
  atomic_commit {
    B b; // ok
    D1 d1; // ok
    B& b1 = d1;
    D2 x;
    b1.f(); // ok, calls D1::f()
    delete b2; // undefined behavior: calls unsafe destructor of D2
  } // { dg-error "" "destructor of D2 is not transaction-safe" }
}
