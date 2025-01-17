// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=noexcept_enforce " }
#include <experimental/contract>
#include <exception>
#include <cstdlib>

// Test that there is an active exception when we reach the terminate handler.
void my_term()
{
  try { throw; }
  catch(int) { std::exit(0); }
}
void handle_contract_violation(const std::experimental::contract_violation& violation)
{
  throw 1;
}

struct X
{

  void f(const int x) pre(x>1) post(x>3) {
     int i = 1;
  }
};

int main()
{
  std::set_terminate (my_term);
  try
  {
      X x;
      x.f(-42);
  } catch (...) {
  }
  // We should not get here
  return 1;
}
