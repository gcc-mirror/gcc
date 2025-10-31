// Throwing violation handler in an assert check in a noexcept function
// can be caught by the function.
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -fcontract-checks-outlined" }

#include <contracts>
#include <exception>
#include <cstdlib>

struct MyException{};


void handle_contract_violation(const std::contracts::contract_violation& violation)
{
  throw MyException{};
}

void free_f(const int x) {
  try {
	  contract_assert(x>1);
	  int i = 1;
  }
  catch(...){}
}

struct X
{
    void f(const int x) {
      try {
	  contract_assert(x>1);
	  int i = 1;
      }
      catch(...){}
    }

    virtual void virt_f(const int x) {
      try {
	  contract_assert(x>1);
	  int i = 1;
      }
      catch(...){}
    }

};

int main()
{
  free_f(-42);

  X x;
  x.f(-42);
  x.virt_f(-42);


}
