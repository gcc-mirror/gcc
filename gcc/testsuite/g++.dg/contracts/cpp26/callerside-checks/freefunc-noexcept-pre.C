// Throwing violation handler in a pre/post check on a noexcept function
// behaves as if the function exited via an exception.
// This tests the behaviour of a post condition on a member function
// with caller side checks.
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -fcontracts-client-check=all " }

#include <contracts>
#include <iostream>
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

void f(int x) noexcept 
  pre(x >= 0)
{
  try{
   int i = 1;
  }
  catch(...) {
  }
}

int main()
{

  std::set_terminate (my_term);
  try
  {
      f(-42);
  } catch (...) {
  }
  // We should not get here
  return 1;
}
