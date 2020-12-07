// PR c++/97093
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fconcepts-diagnostics-depth=3 --param=hash-table-verification-limit=10000" }

template <typename T>
concept C =  requires (T t)
{
  requires t.some_const < 2 || requires { t.some_fn (); };
};

template <unsigned, unsigned>
struct c
{};

template <typename T>
concept P = requires (T t, c <0, 1> v) { { t (v) }; }; // { dg-error "no match" }

template <P auto, P auto ...>
struct m
{
  constexpr auto operator () (C auto) const
  {};
};

struct pc
{
  constexpr auto operator () (C auto) const
  {};
};

constexpr auto cc = pc {};
constexpr auto mmcc = m <cc> {}; // { dg-error "not satisfied" }
