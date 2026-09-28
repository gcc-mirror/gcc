// PR c++/127643
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

struct X {
  template<class T1, class T2> X(T1, T2);
  template<class T1, class T2> void f(T1, T2);
};

template<typename>
struct Y {
  template<class T> Y(T);
};

static constexpr auto ctx = std::meta::access_context::unchecked();

/* 7 because we expect:
  template<class T1, class T2> X::X(T1, T2)
  template<class T1, class T2> void X::f(T1, T2)
  constexpr X::X(const X&)
  constexpr X& X::operator=(const X&)
  constexpr X::X(X&&)
  constexpr X& X::operator=(X&&)
  constexpr X::~X()  */
static_assert (members_of (^^X, ctx).size () == 7);

consteval auto
count_ctor_templates (std::meta::info r)
{
  int n = 0;
  for (auto m : members_of (r, ctx))
    if (is_constructor_template (m))
      ++n;
  return n;
}

static_assert (count_ctor_templates (^^X) == 1);
static_assert (count_ctor_templates (^^Y<int>) == 1);
