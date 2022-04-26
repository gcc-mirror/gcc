// PR c++/100252
// { dg-do compile { target c++14 } }

struct B { };

struct A {
  int x;
  int y = x;
  constexpr operator B() { return B{}; }
};

struct C {
  int x = 42;
  B b = A{x};
};

C c1 = {};
C c2 = { 42 };
constexpr C c3 = {};
constexpr C c4 = { 42 };

struct D {
  int x = 42;
  B b = (true, A{x});
};

D d1 = {};
D d2 = { 42 };
constexpr D d3 = {};
constexpr D d4 = { 42 };

struct E {
  int x = 42;
  B b = (A{x});
};

E e1 = {};
E e2 = { 42 };
constexpr E e3 = {};
constexpr E e4 = { 42 };

struct F {
  int x = 42;
  B b = (A{x});
};

F f1 = {};
F f2 = { 42 };
constexpr F f3 = {};
constexpr F f4 = { 42 };

void
g (C c5 = C{}, C c6 = C{ 42 }, D d5 = D{}, D d6 = D{ 42 },
   E e5 = E{}, E e6 = E{ 42 }, F f5 = F{}, F f6 = F{ 42 })
{
}
