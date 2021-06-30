#include <iostream>
#include <contract>
#include <execinfo.h>
#include <unistd.h>

static constexpr int MAX_BACKTRACE_DEPTH = 128;

void handle_contract_violation(const std::contract_violation &violation) {
  size_t _backtraceSize{0};
  void *_backtrace[MAX_BACKTRACE_DEPTH]{};

  _backtraceSize = backtrace(_backtrace, MAX_BACKTRACE_DEPTH);
  if(_backtraceSize == MAX_BACKTRACE_DEPTH)
    std::cerr << "warning: backtrace may have been truncated" << std::endl;

  std::cerr << "contract violation: " << violation.file_name()
    << ":" << violation.line_number()
    << ": " << violation.comment() << " is false"
    << " [with contract level=" << violation.assertion_level() << "]" << std::endl
    << "violation occurs here:" << std::endl;
  // skip the stack frame of handle_contract_violation and
  // on_contract_violation to get to wherever the assert was
  backtrace_symbols_fd(_backtrace + 2, _backtraceSize - 1, STDERR_FILENO);
  std::cerr << "end of violation" << std::endl;
}

