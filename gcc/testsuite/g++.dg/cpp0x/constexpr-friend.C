// PR c++/48948
// { dg-do compile { target c++11 } }

struct A { A(); };

struct B {
  friend constexpr int f(B) { return 0; } // OK
  friend constexpr int f(A) { return 0; } // { dg-error "constexpr" }
};

template <class T>
struct C
{
  friend constexpr int f(C) { return 0; }
  friend constexpr int g(C, A) { return 0; }
  constexpr int m(C) { return 0; }
  constexpr int m(A) { return 0; }
};

constexpr int i = f(C<int>());
constexpr int j = C<int>().m(C<int>());
constexpr int k = C<double>().m(A()); // { dg-error "" }
constexpr int l = g(C<double>(),A()); // { dg-error "" }

// { dg-prune-output "parameter" }
