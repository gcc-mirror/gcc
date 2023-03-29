// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

struct M
{
  template<typename T>
  int f(int a) [[ pre: a > 0 ]];

  template<typename T>
  int g(int a) [[ pre: a > 0 ]]
  {
    return -a;
  }

  template<typename T>
  int f_arg(T a) [[ pre: a > 0 ]];

  template<typename T>
  int g_arg(T a) [[ pre: a > 0 ]]
  {
    return (int)-a;
  }

  template<typename T>
  T f_ret(int a) [[ pre: a > 0 ]];

  template<typename T>
  T g_ret(int a) [[ pre: a > 0 ]]
  {
    return -a * 1.5;
  }
};

template<typename T>
int M::f(int a)
{
  return -a;
}

template<typename T>
int M::f_arg(T a)
{
  return (int)-a;
}

template<typename T>
T M::f_ret(int a)
{
  return -a * (T)1.5;
}

template<typename T>
struct S
{
  template<typename Q>
  int f(int a) [[ pre: a > 0 ]];

  template<typename Q>
  int g(int a) [[ pre: a > 0 ]]
  {
    return -a;
  }
};

template<typename T>
template<typename Q>
int S<T>::f(int a)
{
  return -a;
}

#include <cstdio>
int main(int, char**)
{
  {
    M m;
    printf ("=================================\n");
    printf ("m.f<int>(-10): %d\n", m.f<int>(-10));
    printf ("m.f<double>(-11.5): %d\n", m.f<double>(-11.5));
    printf ("m.f<int>(10): %d\n", m.f<int>(10));
    printf ("m.f<double>(11.5): %d\n", m.f<double>(11.5));

    printf ("=================================\n");
    printf ("m.g<int>(-10): %d\n", m.g<int>(-10));
    printf ("m.g<double>(-11.5): %d\n", m.g<double>(-11.5));
    printf ("m.g<int>(10): %d\n", m.g<int>(10));
    printf ("m.g<double>(11.5): %d\n", m.g<double>(11.5));

    printf ("=================================\n");
    printf ("m.f_arg(-10): %d\n", m.f_arg(-10));
    printf ("m.f_arg(-11.5): %d\n", m.f_arg(-11.5));
    printf ("m.f_arg(10): %d\n", m.f_arg(10));
    printf ("m.f_arg(11.5): %d\n", m.f_arg(11.5));

    printf ("=================================\n");
    printf ("m.g_arg(-10): %d\n", m.g_arg(-10));
    printf ("m.g_arg(-11.5): %d\n", m.g_arg(-11.5));
    printf ("m.g_arg(10): %d\n", m.g_arg(10));
    printf ("m.g_arg(11.5): %d\n", m.g_arg(11.5));

    printf ("=================================\n");
    printf ("m.f_ret<int>(-10): %d\n", m.f_ret<int>(-10));
    printf ("m.f_ret<double>(-11.5): %f\n", m.f_ret<double>(-11.5));
    printf ("m.f_ret<int>(10): %d\n", m.f_ret<int>(10));
    printf ("m.f_ret<double>(11.5): %f\n", m.f_ret<double>(11.5));

    printf ("=================================\n");
    printf ("m.g_ret<int>(-10): %d\n", m.g_ret<int>(-10));
    printf ("m.g_ret<double>(-11.5): %f\n", m.g_ret<double>(-11.5));
    printf ("m.g_ret<int>(10): %d\n", m.g_ret<int>(10));
    printf ("m.g_ret<double>(11.5): %f\n", m.g_ret<double>(11.5));
  }

  {
    S<int> s;
    printf ("=================================\n");
    s.f<int>(-10);

    s.f<double>(-10);

    printf ("=================================\n");
    s.g<int>(-10);

    s.g<double>(-10);
  }

  {
    S<double> s;
    printf ("=================================\n");
    s.f<int>(-10);

    s.f<double>(-10);

    printf ("=================================\n");
    s.g<int>(-10);

    s.g<double>(-10);
  }

  return 0;
}

// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f<int> at .*:7: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f<int>\(-10\): 10(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f<double> at .*:7: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f<double>\(-11.5\): 11(\n|\r\n|\r)} }
// { dg-output {m.f<int>\(10\): -10(\n|\r\n|\r)} }
// { dg-output {m.f<double>\(11.5\): -11(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g<int> at .*:10: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g<int>\(-10\): 10(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g<double> at .*:10: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g<double>\(-11.5\): 11(\n|\r\n|\r)} }
// { dg-output {m.g<int>\(10\): -10(\n|\r\n|\r)} }
// { dg-output {m.g<double>\(11.5\): -11(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f_arg<int> at .*:16: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f_arg\(-10\): 10(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f_arg<double> at .*:16: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f_arg\(-11.5\): 11(\n|\r\n|\r)} }
// { dg-output {m.f_arg\(10\): -10(\n|\r\n|\r)} }
// { dg-output {m.f_arg\(11.5\): -11(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g_arg<int> at .*:19: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g_arg\(-10\): 10(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g_arg<double> at .*:19: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g_arg\(-11.5\): 11(\n|\r\n|\r)} }
// { dg-output {m.g_arg\(10\): -10(\n|\r\n|\r)} }
// { dg-output {m.g_arg\(11.5\): -11(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f_ret<int> at .*:25: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f_ret<int>\(-10\): 10(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::f_ret<double> at .*:25: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.f_ret<double>\(-11.5\): 16.500000(\n|\r\n|\r)} }
// { dg-output {m.f_ret<int>\(10\): -10(\n|\r\n|\r)} }
// { dg-output {m.f_ret<double>\(11.5\): -16.500000(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g_ret<int> at .*:28: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g_ret<int>\(-10\): 15(\n|\r\n|\r)} }
// { dg-output {contract violation in function M::g_ret<double> at .*:28: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {m.g_ret<double>\(-11.5\): 16.500000(\n|\r\n|\r)} }
// { dg-output {m.g_ret<int>\(10\): -15(\n|\r\n|\r)} }
// { dg-output {m.g_ret<double>\(11.5\): -16.500000(\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function S<int>::f<int> at .*:56: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function S<int>::f<double> at .*:56: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function S<int>::g<int> at .*:59: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function S<int>::g<double> at .*:59: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function S<double>::f<int> at .*:56: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function S<double>::f<double> at .*:56: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {=================================(\n|\r\n|\r)} }
// { dg-output {contract violation in function S<double>::g<int> at .*:59: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
// { dg-output {contract violation in function S<double>::g<double> at .*:59: a > 0(\n|\r\n|\r)} }
// { dg-output {\[continue:on\](\n|\r\n|\r)} }
