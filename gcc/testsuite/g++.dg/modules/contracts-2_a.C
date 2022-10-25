// Basic test to ensure that guarded function contracts are correctly
// serialized and deserialized through the CMI.
// This also tries to ensure that entities referenced in a function's
// contracts are correctly marked as a dependency of the function itself and
// serialized in the correct order.
// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-role=default:maybe,maybe,ignore" }
module;
#include <cstdio>
#include <experimental/contract>
export module foo;
// { dg-module-cmi foo }

export int violation_count{0};
export int violation_line_sum{0};
extern "C++" export void handle_contract_violation(const std::experimental::contract_violation &violation)
{
  violation_count++;
  violation_line_sum += violation.line_number () * violation_count;
  printf("violation: %d %d\n", violation_count, violation_line_sum);
}

export int fn2(int n);
export int fn_in2(int n);
export int pre_print(int n) { printf("pre_print(%d)\n", n); return n; }

export int fn_in1(int n) [[ pre: pre_print(n) > 0 ]]
{
  printf("%s blah (%d)\n", __FUNCTION__, n);
  return n;
}
export int fn_in2(int x) [[ pre: pre_print(x) > 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, x);
  return x;
}

export int fn_iso(int n);

export int fn1(int n)
  [[ pre: pre_print(n) > 0 ]];

export int fn2(int n)
  [[ pre: pre_print(n) > 0 ]];

export int pre_print2(int n);

export int fn3(int n)
  [[ pre: pre_print2(n) > 0 ]];

