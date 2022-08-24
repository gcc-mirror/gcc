// PR c++/105550
// { dg-do compile { target c++14 } }

struct A {
  const A *p = this;
};

struct B {
  const B *p = this;
  constexpr operator A() const { return {}; }
};

constexpr A
bar (A)
{
  return {};
}

constexpr A baz() { return {}; }

struct E {
  A a1 = true ? A{} : A{};
  A a2 = true ? A{} : B{};
  A a3 = false ? A{} : B{};
  A a4 = false ? B{} : B{};
  A a5 = A{};
  A a6 = B{};
  A a7 = false ? B{} : (true ? A{} : A{});
  A a8 = false ? (true ? A{} : B{}) : (true ? A{} : A{});
  A a9 = (A{});
  A a10 = (true, A{});
  A a11 = bar (A{});
  A a12 = baz ();
  A a13 = (A{}, A{});
};

constexpr E e{};

constexpr A a1 = true ? A{} : A{};
constexpr A a2 = true ? A{} : B{};
constexpr A a3 = false ? A{} : B{};
constexpr A a4 = false ? B{} : B{};
constexpr A a5 = A{};
constexpr A a6 = B{};
constexpr A a7 = false ? B{} : (true ? A{} : A{});
constexpr A a8 = false ? (true ? A{} : B{}) : (true ? A{} : A{});
constexpr A a9 = (A{});
constexpr A a10 = (true, A{});
constexpr A a11 = bar (A{});
//static_assert(a10.p == &a10, ""); // bug, 105619
constexpr A a12 = baz ();
//static_assert(a11.p == &a11, ""); // bug, 105619
constexpr A a13 = (A{}, A{});
