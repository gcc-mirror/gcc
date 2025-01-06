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

struct X
{
    void f(int x) noexcept pre(x>1) {
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

}
