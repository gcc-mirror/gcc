// PR c++/99700
// { dg-do compile { target c++20 } }

template <class T>
struct A {
  T c[5];
  constexpr A(int skip = -1) {
    for (int i = 0; i < 5; i++)
      if (i != skip)
        c[i] = {};
  }
};

constexpr A<int> a;
constexpr A<int> a0(0); // { dg-error "not a constant expression|incompletely initialized" }
constexpr A<int> a1(1); // { dg-error "not a constant expression|incompletely initialized" }
constexpr A<int> a2(2); // { dg-error "not a constant expression|incompletely initialized" }
constexpr A<int> a3(3); // { dg-error "not a constant expression|incompletely initialized" }
constexpr A<int> a4(4); // { dg-error "not a constant expression|incompletely initialized" }

struct s { int n; };
constexpr A<s> b;
constexpr A<s> b0(0); // {  dg-error "not a constant expression|incompletely initialized" }

struct empty {};
constexpr A<empty> c;
constexpr A<empty> c0(0);
