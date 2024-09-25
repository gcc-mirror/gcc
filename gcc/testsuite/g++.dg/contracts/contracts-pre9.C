// ensure no errors are thrown for various combinations of class templates
// with guarded members
// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontract-continuation-mode=on" }

template<typename T>
struct S
{
  int f(int a) [[ pre: a > 0 ]];
  int g(int a) [[ pre: a > 0 ]];
};

template<typename T>
int S<T>::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

template<typename T>
int S<T>::g(int a) // Contract is inherited (error from line 10).
{
  return -a;
}

template<typename T>
struct S_arg
{
  int f(T a) [[ pre: a > 0 ]];
  int g(T a) [[ pre: a > 0 ]];
};

template<typename T>
int S_arg<T>::f(T a) [[ pre: a > 0 ]]
{
  return -a;
}

template<typename T>
int S_arg<T>::g(T a) // Contract is inherited (error from line 29).
{
  return -a;
}

template<typename T>
struct S_ret
{
  T f(int a) [[ pre: a > 0 ]];
  T g(int a) [[ pre: a > 0 ]];
};

template<typename T>
T S_ret<T>::f(int a) [[ pre: a > 0 ]]
{
  return -a;
}

template<typename T>
T S_ret<T>::g(int a) // Contract is inherited (error from line 48).
{
  return -a;
}

#include <cstdio>
int main(int, char**)
{
  {
    S<int> s_int;
    printf ("s_int.f(-10): %d\n", s_int.f(-10));
    printf ("s_int.g(-10): %d\n", s_int.g(-10));
    printf ("s_int.f(10): %d\n", s_int.f(10));
    printf ("s_int.g(10): %d\n", s_int.g(10));

    S<double> s_double;
    printf ("s_double.f(-10.5): %d\n", s_double.f(-10.5));
    printf ("s_double.g(-10.5): %d\n", s_double.g(-10.5));
    printf ("s_double.f(10.5): %d\n", s_double.f(10.5));
    printf ("s_double.g(10.5): %d\n", s_double.g(10.5));
  }

  {
    S_arg<int> s_arg_int;
    printf ("s_arg_int.f(-10): %d\n", s_arg_int.f(-10));
    printf ("s_arg_int.g(-10): %d\n", s_arg_int.g(-10));
    printf ("s_arg_int.f(10): %d\n", s_arg_int.f(10));
    printf ("s_arg_int.g(10): %d\n", s_arg_int.g(10));

    S_arg<double> s_arg_double;
    printf ("s_arg_double.f(-10): %d\n", s_arg_double.f(-10));
    printf ("s_arg_double.g(-10): %d\n", s_arg_double.g(-10));
    printf ("s_arg_double.f(10): %d\n", s_arg_double.f(10));
    printf ("s_arg_double.g(10): %d\n", s_arg_double.g(10));
  }

  {
    S_ret<int> s_ret_int;
    printf ("s_ret_int.f(-10): %d\n", s_ret_int.f(-10));
    printf ("s_ret_int.g(-10): %d\n", s_ret_int.g(-10));
    printf ("s_ret_int.f(10): %d\n", s_ret_int.f(10));
    printf ("s_ret_int.g(10): %d\n", s_ret_int.g(10));

    S_ret<double> s_ret_double;
    printf ("s_ret_double.f(-10): %f\n", s_ret_double.f(-10));
    printf ("s_ret_double.g(-10): %f\n", s_ret_double.g(-10));
    printf ("s_ret_double.f(10): %f\n", s_ret_double.f(10));
    printf ("s_ret_double.g(10): %f\n", s_ret_double.g(10));
  }

  return 0;
}

// { dg-skip-if "requires hosted libstdc++ for cstdio" { ! hostedlib } }
// { dg-output "contract violation in function S<int>::f at .*.C:14: .*(\n|\r\n|\r)" }
// { dg-output "s_int.f.-10.: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S<int>::g at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "s_int.g.-10.: 10(\n|\r\n|\r)" }
// { dg-output "s_int.f.10.: -10(\n|\r\n|\r)" }
// { dg-output "s_int.g.10.: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S<double>::f at .*.C:14: .*(\n|\r\n|\r)" }
// { dg-output "s_double.f.-10.5.: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S<double>::g at .*.C:10: .*(\n|\r\n|\r)" }
// { dg-output "s_double.g.-10.5.: 10(\n|\r\n|\r)" }
// { dg-output "s_double.f.10.5.: -10(\n|\r\n|\r)" }
// { dg-output "s_double.g.10.5.: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_arg<int>::f at .*.C:33: .*(\n|\r\n|\r)" }
// { dg-output "s_arg_int.f.-10.: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_arg<int>::g at .*.C:29: .*(\n|\r\n|\r)" }
// { dg-output "s_arg_int.g.-10.: 10(\n|\r\n|\r)" }
// { dg-output "s_arg_int.f.10.: -10(\n|\r\n|\r)" }
// { dg-output "s_arg_int.g.10.: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_arg<double>::f at .*.C:33: .*(\n|\r\n|\r)" }
// { dg-output "s_arg_double.f.-10.: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_arg<double>::g at .*.C:29: .*(\n|\r\n|\r)" }
// { dg-output "s_arg_double.g.-10.: 10(\n|\r\n|\r)" }
// { dg-output "s_arg_double.f.10.: -10(\n|\r\n|\r)" }
// { dg-output "s_arg_double.g.10.: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_ret<int>::f at .*.C:52: .*(\n|\r\n|\r)" }
// { dg-output "s_ret_int.f.-10.: 10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_ret<int>::g at .*.C:48: .*(\n|\r\n|\r)" }
// { dg-output "s_ret_int.g.-10.: 10(\n|\r\n|\r)" }
// { dg-output "s_ret_int.f.10.: -10(\n|\r\n|\r)" }
// { dg-output "s_ret_int.g.10.: -10(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_ret<double>::f at .*.C:52: .*(\n|\r\n|\r)" }
// { dg-output "s_ret_double.f.-10.: 10.000000(\n|\r\n|\r)" }
// { dg-output "contract violation in function S_ret<double>::g at .*.C:48: .*(\n|\r\n|\r)" }
// { dg-output "s_ret_double.g.-10.: 10.000000(\n|\r\n|\r)" }
// { dg-output "s_ret_double.f.10.: -10.000000(\n|\r\n|\r)" }
// { dg-output "s_ret_double.g.10.: -10.000000(\n|\r\n|\r)" }
