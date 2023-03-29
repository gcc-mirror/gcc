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

// { dg-output "contract violation in function S::S<int> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::S<int> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "S::S.T.: -1(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::S<double> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::S<double> at .*.C:8: .*(\n|\r\n|\r)" }
// { dg-output "S::S.T.: -2(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1::S1<int> at .*.C:20: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1::S1<int> at .*.C:20: .*(\n|\r\n|\r)" }
// { dg-output "S1::S1.T.: -3(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1::S1<double> at .*.C:20: .*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S1::S1<double> at .*.C:20: .*(\n|\r\n|\r)" }
// { dg-output "S1::S1.T.: -4(\n|\r\n|\r)" }

