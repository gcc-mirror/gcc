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

// { dg-output "contract violation in function body<int> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "-2(\n|\r\n|\r)" }
// { dg-output "contract violation in function body<double> at .*.C:16: .*(\n|\r\n|\r)" }
// { dg-output "-3(\n|\r\n|\r)" }
// { dg-output "contract violation in function none<int> at .*.C:24: .*(\n|\r\n|\r)" }
// { dg-output "1(\n|\r\n|\r)" }
// { dg-output "contract violation in function none<double> at .*.C:31: .*(\n|\r\n|\r)" }
// { dg-output "-101(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg0<int> at .*.C:38: .*(\n|\r\n|\r)" }
// { dg-output "-9(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg0<double> at .*.C:45: .*(\n|\r\n|\r)" }
// { dg-output "11(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<int> at .*.C:52: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<int> at .*.C:53: .*(\n|\r\n|\r)" }
// { dg-output "-3(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<double> at .*.C:60: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function arg1<double> at .*.C:61: .*(\n|\r\n|\r)" }
// { dg-output "14(\n|\r\n|\r)" }
// { dg-output "contract violation in function ret<int> at .*.C:68: .*(\n|\r\n|\r)" }
// { dg-output "1(\n|\r\n|\r)" }
// { dg-output "contract violation in function ret<double> at .*.C:75: .*(\n|\r\n|\r)" }
// { dg-output "3(\n|\r\n|\r)" }
// { dg-output "contract violation in function ret<double> at .*.C:75: .*(\n|\r\n|\r)" }
// { dg-output "3.300000(\n|\r\n|\r)" }

