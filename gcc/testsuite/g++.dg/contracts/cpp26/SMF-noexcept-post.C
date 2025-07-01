// Throwing violation handler in a pre/post check on a noexcept function
// behaves as if the function exited via an exception.
// This tests the behaviour of a post condition on a constructor with function
// try block
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe " }

#include <contracts>
#include <exception>
#include <cstdlib>

struct MyException{};

// Test that there is an active exception when we reach the terminate handler.
void my_term()
{
  try { throw; }
  catch(MyException) { std::exit(0); }
}


void handle_contract_violation(const std::contracts::contract_violation& violation)
{
  throw MyException{};
}

struct X
{

    X(const int x) noexcept post(x>1) try
    {
       int i = 1;
    }
    catch(...) {}
};

int main()
{
  std::set_terminate (my_term);
  try
  {
      X x(-42);
  } catch (...) {
  }
  // We should not get here
  return 1;
}
