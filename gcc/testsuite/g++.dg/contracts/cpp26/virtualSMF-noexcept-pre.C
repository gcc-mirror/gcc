// Throwing violation handler in a pre/post check on a noexcept function
// behaves as if the function exited via an exception.
// This tests the behaviour of a pre condition on a destructor
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe " }

#include <experimental/contract>
#include <exception>
#include <cstdlib>

struct MyException{};

// Test that there is an active exception when we reach the terminate handler.
void my_term()
{
  try { throw; }
  catch(MyException) { std::exit(0); }
}


void handle_contract_violation(const std::experimental::contract_violation& violation)
{
  throw MyException{};
}

const int x = -42;

struct X
{
    virtual ~X() pre(x>1)
    {
      try{}
      catch(...) {}
    }
};

int main()
{
  std::set_terminate (my_term);
  try
  {
      X x;
  } catch (...) {
  }
  // We should not get here
  return 1;
}
