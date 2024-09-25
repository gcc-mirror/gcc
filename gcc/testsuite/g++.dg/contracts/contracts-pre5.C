// basic test to ensure pre contracts work for free templates
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

template<typename T>
int none(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

template<typename T>
int arg0(T t)
  [[ pre: t > 0 ]]
{
  return -t - 10;
}

template<typename T>
int arg1(int a, T t)
  [[ pre: a > 0 ]]
  [[ pre: t > 0 ]]
{
  return -t * a;
}

template<typename T>
T ret(int a)
  [[ pre: a > 0 ]]
{
  return -a;
}

int main(int, char**)
{
  printf("%d\n", body<int>(-1));
  printf("%d\n", body<double>(-2));
  printf("%d\n", none<int>(-1));
  printf("%d\n", none<double>(-2));
  printf("%d\n", arg0(-1));
  printf("%d\n", arg0(-2.9));
  printf("%d\n", arg1(-3, -1));
  printf("%d\n", arg1(-4, -2.9));
  printf("%d\n", (int)ret<int>(-3));
  printf("%d\n", (int)ret<double>(-4.9));

  return 0;
}

// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// { dg-output "contract violation in function body<int> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "-2(\n|\r\n|\r)" }
// { dg-output "contract violation in function body<double> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "-5(\n|\r\n|\r)" }
// { dg-output "contract violation in function none<int> at .*.C:16: .*(\n|\r\n|\r)" }
// { dg-output "1(\n|\r\n|\r)" }
// { dg-output "contract violation in function none<double> at .*.C:16: .*(\n|\r\n|\r)" }
// { dg-output "2(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg0<int> at .*.C:23: .*(\n|\r\n|\r)" }
// { dg-output "-9(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg0<double> at .*.C:23: .*(\n|\r\n|\r)" }
// { dg-output "-7(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<int> at .*.C:30: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<int> at .*.C:31: .*(\n|\r\n|\r)" }
// { dg-output "-3(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<double> at .*.C:30: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<double> at .*.C:31: .*(\n|\r\n|\r)" }
// { dg-output "-11(\n|\r\n|\r)" }
// { dg-output "contract violation in function ret<int> at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "3(\n|\r\n|\r)" }
// { dg-output "contract violation in function ret<double> at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "4(\n|\r\n|\r)" }

