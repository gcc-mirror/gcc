// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fcontracts -fcontract-continuation-mode=on" }
// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }

#include <cstdio>
import foo;

template<typename T>
bool bar_fn_pre(T n) { printf("bar fn pre(%d)\n", n); return true; }

template<typename T>
T bar_fn(T n)
  [[ pre: bar_fn_pre(n) && n > 0 ]]
{
  printf("%s(%d)\n", __FUNCTION__, n);
  return n;
}

int main(int, char**)
{
  nontemplate(5);
  nontemplate(-5);
  fn(5);
  fn(-5);
  void_fn(5);
  void_fn(-5);
  bar_fn(5);
  bar_fn(-5);
  return violation_count == 6 ? 0 : -1;
}

