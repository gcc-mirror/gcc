// { dg-do run }
// { dg-options "-std=c++2a -fcontracts -fcontracts-nonattr -fcontract-evaluation-semantic=observe -fcontracts-nonattr-client-check=all" }



int f(const int a, const int b) pre (a > 2) post(r : r > 2){ return b;  }


struct S
{
  int f(const int a, const int b) pre (a > 3) post(r : r > 3){ return b;  }
};

template<typename T>
struct TS
{
  int f(const int a, const T b) pre (a > 4) post(r : r > 4){ return b;  }

  template <typename U>
  int tf(const int a, const U b) pre (a > 5) post(r : r > 5){ return b;  }
};

int main(int, char**)
{
  f(1,1);

  S s;
  s.f(1,1);

  TS<int> ts;
  ts.f(1,1);

  ts.tf(1,1);
  return 0;
}

// { dg-output "contract violation in function .*contract_wrapper at .*: a > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*: a > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function f at .*: r > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: r > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: a > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::f at .*: a > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function S::f at .*: r > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: r > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: a > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TS<int>::f at .*: a > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TS<int>::f at .*: r > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: r > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TS<int>::tf<int> at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function TS<int>::tf<int> at .*: r > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function .*contract_wrapper at .*: r > 5.*(\n|\r\n|\r)" }
