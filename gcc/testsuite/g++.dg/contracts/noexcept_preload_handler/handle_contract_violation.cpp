#include <iostream>
#include <contract>

void handle_contract_violation(const std::contract_violation &violation) {
  std::cerr << "custom handle_contract_violation: " << std::endl
    << " line_number: " << violation.line_number() << std::endl
    << " file_name: " << violation.file_name() << std::endl
    << " function_name: " << violation.function_name() << std::endl
    << " comment: " << violation.comment() << std::endl
    << " assertion_level: " << violation.assertion_level() << std::endl
    << std::endl;
  throw -1;
}

