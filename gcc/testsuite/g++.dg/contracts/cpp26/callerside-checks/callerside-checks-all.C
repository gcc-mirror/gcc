// { dg-do run { target c++20 } }
// { dg-additional-options "-fcontracts -fcontract-evaluation-semantic=observe -fcontracts-client-check=all" }



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

// { dg-output "contract violation in function int f.int, int. at .*: a > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int f.int, int. at .*: a > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int f.int, int. at .*: r > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int f.int, int. at .*: r > 2.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int S::f.int, int. at .*: a > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int S::f.int, int. at .*: a > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int S::f.int, int. at .*: r > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int S::f.int, int. at .*: r > 3.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::f.int, T. .with T = int. at .*: a > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::f.int, T. .with T = int. at .*: a > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::f.int, T. .with T = int. at .*: r > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::f.int, T. .with T = int. at .*: r > 4.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::tf.int, U. .with U = int; T = int. at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::tf.int, U. .with U = int; T = int. at .*: a > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::tf.int, U. .with U = int; T = int. at .*: r > 5.*(\n|\r\n|\r)" }
// { dg-output "contract violation in function int TS<T>::tf.int, U. .with U = int; T = int. at .*: r > 5.*(\n|\r\n|\r)" }
