// ensure that exceptions thrown inside a custom contract violation handler
// are not catchable up the call stack even when continue mode is off
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts" }
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

int main(int, char**) {
  try {
    fun();
  } catch(int &ex) {
    std::cerr << "synth caught indirect: " << ex << std::endl;
  }

  return 0;
}

// { dg-output "custom std::handle_contract_violation called: 18 .*/contracts16.C(\n|\r\n|\r)" }
// { dg-output "synth caught indirect: -18(\n|\r\n|\r)" }

