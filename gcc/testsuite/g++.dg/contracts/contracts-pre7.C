// ensure no errors are thrown when we have to insert a decl for the internal
// unchecked function after leaving a (possibly nested) namespace
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

namespace ns0
{
  template<typename T>
  int f(T a) [[ pre: a > 0 ]];
}

template<typename T>
int ns0::f(T a) [[ pre: a > 0 ]]
{
  return (int)-a;
}

namespace ns0
{
  namespace ns1
  {
    template<typename T>
    int f(T a) [[ pre: a > 0 ]];
  }
}

template<typename T>
int ns0::ns1::f(T a) [[ pre: a > 0 ]]
{
  return -a;
}

namespace ns0
{
  namespace ns1
  {
    template<typename T>
    int f2(T a) [[ pre: a > 0 ]];
    namespace ns2
    {
      template<typename T>
      int f(T a) [[ pre: a > 0 ]];
    }
  }
  template<typename T>
  int ns1::f2(T a) [[ pre: a > 0 ]]
  {
    return -a;
  }
}

template<typename T>
int ns0::ns1::ns2::f(T a) [[ pre: a > 0 ]]
{
  return -a;
}

namespace ns0
{
  template<typename T>
  struct S
  {
    int f(T a) [[ pre: a > 0 ]];
  };
  namespace ns1
  {
    template<typename T>
    struct S2
    {
      int f(T a) [[ pre: a > 0 ]];
    };
  }
}

template<typename T>
int ns0::S<T>::f(T a) [[ pre: a > 0 ]]
{
  return -a;
}

template<typename T>
int ns0::ns1::S2<T>::f(T a) [[ pre: a > 0 ]]
{
  return -a;
}

#include <cstdio>
int main(int, char**)
{
  printf ("%d\n", ns0::f(-1));
  printf ("%d\n", ns0::ns1::f(-2));
  printf ("%d\n", ns0::ns1::f2(-3));
  printf ("%d\n", ns0::ns1::ns2::f(-4));
  ns0::S<int> ns0_s;
  printf ("%d\n", ns0_s.f(-5));
  ns0::ns1::S2<int> ns0_ns1_s2;
  printf ("%d\n", ns0_ns1_s2.f(-6));

  printf ("%d\n", ns0::f(-7.5));
  printf ("%d\n", ns0::ns1::f(-8.5));
  printf ("%d\n", ns0::ns1::f2(-9.5));
  printf ("%d\n", ns0::ns1::ns2::f(-10.5));
  ns0::S<double> ns0_sd;
  printf ("%d\n", ns0_sd.f(-11.5));
  ns0::ns1::S2<double> ns0_ns1_s2d;
  printf ("%d\n", ns0_ns1_s2d.f(-12.5));

  return 0;
}

// { dg-output "contract violation in function ns0::f<int> at .*.C:13: .*(\n|\r\n|\r)" }
// { dg-output "1(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::f<int> at .*.C:28: .*(\n|\r\n|\r)" }
// { dg-output "2(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::f2<int> at .*.C:46: .*(\n|\r\n|\r)" }
// { dg-output "3(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::ns2::f<int> at .*.C:53: .*(\n|\r\n|\r)" }
// { dg-output "4(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::S<int>::f at .*.C:76: .*(\n|\r\n|\r)" }
// { dg-output "5(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::S2<int>::f at .*.C:82: .*(\n|\r\n|\r)" }
// { dg-output "6(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::f<double> at .*.C:13: .*(\n|\r\n|\r)" }
// { dg-output "7(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::f<double> at .*.C:28: .*(\n|\r\n|\r)" }
// { dg-output "8(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::f2<double> at .*.C:46: .*(\n|\r\n|\r)" }
// { dg-output "9(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::ns2::f<double> at .*.C:53: .*(\n|\r\n|\r)" }
// { dg-output "10(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::S<double>::f at .*.C:76: .*(\n|\r\n|\r)" }
// { dg-output "11(\n|\r\n|\r)" }
// { dg-output "contract violation in function ns0::ns1::S2<double>::f at .*.C:82: .*(\n|\r\n|\r)" }
// { dg-output "12(\n|\r\n|\r)" }
