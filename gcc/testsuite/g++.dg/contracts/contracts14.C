// ensure that exceptions thrown inside a custom contract violation handler
// are catchable up the call stack
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

int fun() {
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

// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// { dg-output "custom std::handle_contract_violation called: 30 .*/contracts14.C(\n|\r\n|\r)" }
// { dg-output "synth caught direct: -30(\n|\r\n|\r)" }
// { dg-output "custom std::handle_contract_violation called: 18 .*/contracts14.C(\n|\r\n|\r)" }
// { dg-output "synth caught indirect: -18(\n|\r\n|\r)" }
// { dg-output "custom std::handle_contract_violation called: 18 .*/contracts14.C(\n|\r\n|\r)" }
// { dg-output "synth caught double indirect: -18(\n|\r\n|\r)" }
// { dg-output "end main" }

