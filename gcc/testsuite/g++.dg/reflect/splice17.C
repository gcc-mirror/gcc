// PR c++/124794
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct C {
  template <class T> void f(T);
  void g (int);

  static constexpr int val = 42;
};

constexpr auto ac = std::meta::access_context::current();
constexpr auto f1 = members_of(^^C, ac)[0];
constexpr auto f2 = ^^C::f;
void (C::*p1)(int) = &template [:f1:];
void (C::*p2)(int) = &template [:f2:];

constexpr auto g1 = members_of(^^C, ac)[1];
constexpr auto g2 = ^^C::g;
void (C::*p3)(int) = &[:g1:];
void (C::*p4)(int) = &[:g2:];

void
g (C *pc)
{
  auto p = &pc->[: ^^C::val :];
  auto q = &pc->C::val;

  pc->f (42);
  pc->template [:f1:](42);
  pc->template [:f2:](42);
  pc->g (42);
  pc->[:g1:] (42);
  pc->[:g2:] (42);
}

struct D1 : C {
  void mfn (D1 *pd)
  {
    auto p = &pd->[: ^^C::val :];
    auto q = &pd->C::val;

    pd->f (42);
    pd->template [:f1:](42);
    pd->template [:f2:](42);
    pd->g (42);
    pd->[:g1:] (42);
    pd->[:g2:] (42);
  }
};

struct D2 : C {
  void mfn (D1 *pd)
  {
    auto p = &pd->[: ^^C::val :];
    auto q = &pd->C::val;

    constexpr auto rg = ^^C::g;
    pd->[:rg:] (42);
    constexpr auto rf = ^^C::f;
    pd->template [:rf:] (42);

    pd->f (42);
    pd->template [:f1:](42);
    pd->template [:f2:](42);
    pd->g (42);
    pd->[:g1:] (42);
    pd->[:g2:] (42);
  }
};
