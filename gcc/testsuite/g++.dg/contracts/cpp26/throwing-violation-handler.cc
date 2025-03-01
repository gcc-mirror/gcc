#include <contracts>

void handle_contract_violation(const std::contracts::contract_violation&)
{
  throw 666;
}

int f(int);

extern bool f_result;

void g()
{
  try {
    f(-42);
  } catch (...) {
    f_result = false;
  }
}
