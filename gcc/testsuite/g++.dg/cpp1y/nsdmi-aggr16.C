// PR c++/100252
// { dg-do compile { target c++14 } }

#define SA(X) static_assert ((X),#X)

struct A {
  const A* p = this;
};

struct B {
  A a = A{};
};

constexpr B b;
SA(b.a.p == &b.a);
B b1 = { };

struct C {
  A a = (true, A{});
};

constexpr C c;
SA(c.a.p == &c.a);
C c1 = { };

struct D {
  A a = (A{});
};

constexpr D d;
SA(d.a.p == &d.a);
D d1 = { };

static constexpr A global_a;

struct E {
  A a = true ? A{} : A{};
  A b = true ? global_a : (false ? A{} : A{});
  A c = true ? (false ? A{} : A{}) : global_a;
  A d = true ? (false ? A{} : A{}) : (false ? A{} : A{});
};

constexpr E e;
SA (e.a.p == &e.a);

E e1 = { };

struct F {
  bool b = (A{}, true);
};

constexpr F f;

void
g (B b2 = B{}, C c2 = C{}, D d2 = D{}, E e2 = E{})
{
}
