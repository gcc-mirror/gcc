// ensure that exceptions thrown inside a custom contract violation handler
// are not catchable up the call stack when failing in a noexcept function
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <iostream>
#include <contract>

int
__on_contract_violation (bool continue_,
        int line_number,
        const char * file_name,
        const char * function_name,
        const char * comment,
        const char * assertion_level,
        const char * assertion_role,
        int continuation_mode);

void handle_contract_violation(const std::contract_violation &violation) {
  std::cerr << "custom std::handle_contract_violation called:"
    << " " << violation.line_number()
    << " " << violation.file_name()
    << std::endl;
  throw -violation.line_number();
}

int fun() noexcept {
  int x = 0;
  [[ assert: x < 0 ]];
  return 0;
}

int fun2() {
  __on_contract_violation(true, 1, "T1", "T2", "T3", "T4", "T5", 1);
  return 1;
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
    fun2();
  } catch(int &ex) {
    std::cerr << "caught lib direct: " << ex << std::endl;
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

// { dg-output "custom std::handle_contract_violation called: 45 .*/contracts15.C(\n|\r\n|\r)*" }
// { dg-output "synth caught direct: -45(\n|\r\n|\r)*" }
// { dg-output "custom std::handle_contract_violation called: 1 T1(\n|\r\n|\r)*" }
// { dg-output "caught lib direct: -1(\n|\r\n|\r)*" }
// { dg-output "custom std::handle_contract_violation called: 28 .*/contracts15.C(\n|\r\n|\r)*" }
// { dg-output "terminate called after throwing an instance of .int.(\n|\r\n|\r)*" }
// { dg-shouldfail "throwing in noexcept" }

