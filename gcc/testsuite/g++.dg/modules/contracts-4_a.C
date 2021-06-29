// Test that template contracts are not reintpreted when the reinterpret
// contracts flag is not set, regardless of the current TU's contract
// configuration.
// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-role=default:maybe,maybe,ignore" }
module;
#include <cstdio>
#include <experimental/contract>
export module foo;
// { dg-module-cmi foo }

export int violation_count{0};
extern "C++" export void handle_contract_violation(const std::experimental::contract_violation &violation)
{
  violation_count++;
  printf("violation_count: %d\n", violation_count);
}

export template<typename T>
T fn_t(T t)
  [[ pre: t > 0 ]]
  [[ pre audit %custom: t > 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, t);
  return t;
}

export int fn_int(int n);

