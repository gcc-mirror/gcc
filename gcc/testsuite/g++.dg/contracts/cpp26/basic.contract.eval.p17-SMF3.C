// N5008:
// basic.contract.eval/p17
// If a contract-violation handler invoked from the evaluation of a function contract assertion (9.4.1) exits via
// an exception, the behavior is as if the function body exits via that same exception.
// [Note 12 : A function-try-block (14.1) is the function body when present and thus does not have an opportunity to
// catch the exception. If the function has a non-throwing exception specification, the function std::terminate is
// invoked (14.6.2). — end note]
//
// This tests the behaviour of a pre condition on a destructor
//
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }

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

const int x = -42;

struct X
{

    ~X() noexcept pre(x>1)
    {
      try{}
      catch(...){
      }
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
