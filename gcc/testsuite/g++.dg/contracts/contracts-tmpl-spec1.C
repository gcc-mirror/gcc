// basic test to ensure pre contracts work for free template specializations
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }
#include <cstdio>

template<typename T>
int body(int a)
  [[ pre: a > 0 ]]
{
  T t = a * 2.5;
  return t;
}

template<>
int body<double>(int a)
  [[ pre: a > 0 ]]
{
  double t = a * 3.3;
  return t;
}

template<typename T>
int none(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

template<>
int none<double>(int a)
  [[ pre: a > 0 ]]
{
  return a - 100;
}

template<typename T>
int arg0(T t)
  [[ pre: t > 0 ]]
{
  return -t - 10;
}

template<>
int arg0<double>(double t)
  [[ pre: t > 0 ]]
{
  return -t + 10;
}

template<typename T>
int arg1(int a, T t)
  [[ pre: a > 0 ]]
  [[ pre: t > 0 ]]
{
  return -t * a;
}

template<>
int arg1<double>(int a, double t)
  [[ pre: a > 0 ]]
  [[ pre: t > 0 ]]
{
  return -t * a + 17;
}

template<typename T>
T ret(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

template<>
double ret<double>(int a)
  [[ pre: a > 0 ]]
{
  return -a * 3.3;
}


int main(int, char**)
{
  printf("%d\n", body<int>(-1));
  printf("%d\n", body<double>(-1));
  printf("%d\n", none<int>(-1));
  printf("%d\n", none<double>(-1));
  printf("%d\n", arg0(-1));
  printf("%d\n", arg0(-1.0));
  printf("%d\n", arg1(-3, -1));
  printf("%d\n", arg1(-3, -1.0));
  printf("%d\n", (int)ret<int>(-1));
  printf("%d\n", (int)ret<double>(-1));
  printf("%f\n", ret<double>(-1));
  return 0;
}

// { dg-output "default std::handle_contract_violation called: .*.C 8 body<int> .*(\n|\r\n|\r)*" }
// { dg-output "-2(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 16 body<double> .*(\n|\r\n|\r)*" }
// { dg-output "-3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 24 none<int> .*(\n|\r\n|\r)*" }
// { dg-output "1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 31 none<double> .*(\n|\r\n|\r)*" }
// { dg-output "-101(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 38 arg0<int> .*(\n|\r\n|\r)*" }
// { dg-output "-9(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 45 arg0<double> .*(\n|\r\n|\r)*" }
// { dg-output "11(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 52 arg1<int> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 53 arg1<int> .*(\n|\r\n|\r)*" }
// { dg-output "-3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 60 arg1<double> .*(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 61 arg1<double> .*(\n|\r\n|\r)*" }
// { dg-output "14(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 68 ret<int> .*(\n|\r\n|\r)*" }
// { dg-output "1(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 75 ret<double> .*(\n|\r\n|\r)*" }
// { dg-output "3(\n|\r\n|\r)*" }
// { dg-output "default std::handle_contract_violation called: .*.C 75 ret<double> .*(\n|\r\n|\r)*" }
// { dg-output "3.300000(\n|\r\n|\r)*" }

