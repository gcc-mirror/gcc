// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }

#include <experimental/contract>

using std::experimental::contract_violation;
void handle_contract_violation(const contract_violation &violation)
{
  __builtin_exit (0);
}

struct A {
  int i;
  A(): i(42) {}
  A(const A&);
};

A f()
  [[ post r: r.i == 24 ]]
{
  A a;
  return a;
}

int main()
{
  f();
  return -1;
}
