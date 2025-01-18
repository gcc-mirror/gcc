// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=noexcept_enforce " }
#include <experimental/contract>
#include <exception>
#include <cstdlib>

// Test that there is an active exception when we reach the terminate handler.
void my_term()
{
  std::exit(0);
}

void handle_contract_violation(const std::experimental::contract_violation& violation)
{
}

void f(int x) pre(x >= 0)
{
}

int main()
{
  std::set_terminate (my_term);
  f(-42);
  // We should not get here
  return 1;
}
