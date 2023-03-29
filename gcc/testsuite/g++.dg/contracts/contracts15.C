// ensure that exceptions thrown inside a custom contract violation handler
// are not catchable up the call stack when failing in a noexcept function
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <iostream>
#include <experimental/contract>

void handle_contract_violation(const std::experimental::contract_violation &violation) {
  std::cerr << "custom std::handle_contract_violation called:"
    << " " << violation.line_number()
    << " " << violation.file_name()
    << std::endl;
  throw -(int)violation.line_number();
}

int fun() noexcept {
  int x = 0;
  [[ assert: x < 0 ]];
  return 0;
}

int fun3() {
  fun();
  return 2;
}

int main(int, char**) {
  try {
    int x = 0;
    [[ assert: x < 0 ]];
  } catch(int &ex) {
    std::cerr << "synth caught direct: " << ex << std::endl;
  }

  try {
    fun();
  } catch(int &ex) {
    std::cerr << "synth caught indirect: " << ex << std::endl;
  }

  try {
    fun3();
  } catch(int &ex) {
    std::cerr << "synth caught double indirect: " << ex << std::endl;
  }

  std::cerr << "end main" << std::endl;
  return 0;
}

// { dg-output "custom std::handle_contract_violation called: 30 .*/contracts15.C(\n|\r\n|\r)" }
// { dg-output "synth caught direct: -30(\n|\r\n|\r)" }
// { dg-output "custom std::handle_contract_violation called: 18 .*/contracts15.C(\n|\r\n|\r)" }
// { dg-output "terminate called after throwing an instance of .int.(\n|\r\n|\r)" }
// { dg-shouldfail "throwing in noexcept" }

