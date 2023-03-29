// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

template<typename T, typename S>
struct G5
{
  template<typename R>
  void f(T t, S s, R r)
    [[ pre: t > 0 ]] [[ pre: s > 0 ]] [[ pre: r > 0 ]]
  {
    printf ("G5 gen T S, f gen R\n");
  }
};

// specializations can remove contracts
template<>
template<typename R>
void G5<double, double>::f(double a, double b, R c)
{
  printf ("G5 full double double, f gen R\n");
}

int main(int, char**) {
  G5<double, double> g5_full;
  g5_full.f(-1.0, -1.0, -2);
  g5_full.f(-1.0, -1.0, -2.1);

  G5<int, double> g5_gen;
  g5_gen.f(-1, -1.0, -2);
  g5_gen.f(-1, -1.0, -2.1);
  return 0;
}

// { dg-output {G5 full double double, f gen R(\n|\r\n|\r)} }
// { dg-output {G5 full double double, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<int> at .*:10: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<int> at .*:10: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<int> at .*:10: r > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 gen T S, f gen R(\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<double> at .*:10: t > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<double> at .*:10: s > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function G5<int, double>::f<double> at .*:10: r > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {G5 gen T S, f gen R(\n|\r\n|\r)} }
