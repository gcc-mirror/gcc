// Basic test to ensure that guarded templates correctly serialize and
// deserialize their contracts through the CMI.
// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-continuation-mode=on" }
// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
module;
#include <cstdio>
#include <experimental/contract>
export module foo;
// { dg-module-cmi foo }

export int violation_count{0};
export extern "C++" void handle_contract_violation(const std::experimental::contract_violation &violation)
{
  violation_count++;
  printf("violation_count: %d\n", violation_count);
}

export int nontemplate(int n)
  [[ pre: n > 0 ]]
  [[ post r: r > 0 ]]
{
  return -n;
}

export
template<typename T>
T fn(T n)
  [[ pre: n > 0 ]]
  [[ post r: r > 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, n);
  return n;
}

export
template<typename T>
void void_fn(T n)
  [[ pre: n < 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, n);
}

