// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

struct S
{
  template<typename T>
  S(T a) [[ pre: a > 0 ]] [[ pre: a > 10 ]];
};

template<typename T>
S::S(T a)
{
  printf ("S::S(T): %d\n", (int)a);
}

struct S1
{
  template<typename T>
  S1(T a) [[ pre: a > 0 ]] [[ pre: a > 10 ]]
  {
    printf ("S1::S1(T): %d\n", (int)a);
  }
};

int main(int, char **) {
  S s{-1};
  S s2{-2.5};

  S1 s1_1{-3};
  S1 s1_2{-4.5};
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 8 S::S<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 8 S::S<int> .*(\n|\r\n|\r)*" }
// { dg-output "S::S.T.: -1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 8 S::S<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 8 S::S<double> .*(\n|\r\n|\r)*" }
// { dg-output "S::S.T.: -2(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 S1::S1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 S1::S1<int> .*(\n|\r\n|\r)*" }
// { dg-output "S1::S1.T.: -3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 S1::S1<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 20 S1::S1<double> .*(\n|\r\n|\r)*" }
// { dg-output "S1::S1.T.: -4(\n|\r\n|\r)*" }

