// N5008:
// basic.contract.eval/p17
// If a contract-violation handler invoked from the evaluation of a function contract assertion (9.4.1) exits via
// an exception, the behavior is as if the function body exits via that same exception.
// [Note 13 : If a contract-violation handler invoked from an assertion-statement (8.8)) exits via an exception, the search
// for a handler continues from the execution of that statement. — end note]
// { dg-do run { target c++26 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe " }

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
