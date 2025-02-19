// PR c++/108205
// { dg-module-do run }
// { dg-additional-options "-fmodules -fcontracts -fcontract-continuation-mode=on" }

#include <experimental/contract>
import test;

bool saw_violation = false;
void handle_contract_violation(const std::experimental::contract_violation& v) {
  saw_violation = true;
}

int main() {
  foo(10);
  if (saw_violation)
    __builtin_abort();
  foo(0);
  if (!saw_violation)
    __builtin_abort();
}
