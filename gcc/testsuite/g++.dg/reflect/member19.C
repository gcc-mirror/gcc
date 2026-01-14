// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct S {
  template <int N>
  static constexpr int a = N;
};
struct T : public S {
  template <int N>
  static constexpr int a = N + 100;
};

constexpr access_context uctx = access_context::unchecked ();

constexpr S s;
static_assert (s.a <42> == 42);
static_assert (s.a <43> == 43);
static_assert (s.template [:members_of (^^S, uctx)[0]:] <44> == 44);
static_assert (s.template [:members_of (^^S, uctx)[0]:] <45> == 45);
static_assert (s.template [:^^S::a:] <44> == 44);
static_assert (s.template [:^^S::a:] <45> == 45);
constexpr T t;
static_assert (t.a <42> == 142);
static_assert (t.a <43> == 143);
static_assert (t.template [:members_of (^^S, uctx)[0]:] <44> == 44);
static_assert (t.template [:members_of (^^S, uctx)[0]:] <45> == 45);
static_assert (t.template [:members_of (^^T, uctx)[0]:] <44> == 144);
static_assert (t.template [:members_of (^^T, uctx)[0]:] <45> == 145);
static_assert (t.template [:^^S::a:] <44> == 44);
static_assert (t.template [:^^S::a:] <45> == 45);
static_assert (t.template [:^^T::a:] <44> == 144);
static_assert (t.template [:^^T::a:] <45> == 145);

template <int N>
void
foo ()
{
  constexpr S s;
  static_assert (s.a <42> == 42);
  static_assert (s.a <43> == 43);
  static_assert (s.template [:members_of (^^S, uctx)[0]:] <44> == 44);
  static_assert (s.template [:members_of (^^S, uctx)[0]:] <45> == 45);
  static_assert (s.template [:^^S::a:] <44> == 44);
  static_assert (s.template [:^^S::a:] <45> == 45);
  constexpr T t;
  static_assert (t.a <42> == 142);
  static_assert (t.a <43> == 143);
  static_assert (t.template [:members_of (^^S, uctx)[0]:] <44> == 44);
  static_assert (t.template [:members_of (^^S, uctx)[0]:] <45> == 45);
  static_assert (t.template [:^^S::a:] <44> == 44);
  static_assert (t.template [:^^S::a:] <45> == 45);
  static_assert (t.template [:members_of (^^T, uctx)[0]:] <44> == 144);
  static_assert (t.template [:members_of (^^T, uctx)[0]:] <45> == 145);
  static_assert (t.template [:^^T::a:] <44> == 144);
  static_assert (t.template [:^^T::a:] <45> == 145);
}

template <typename A, typename B>
void
bar ()
{
  constexpr A s;
  static_assert (s.template a <42> == 42);
  static_assert (s.template a <43> == 43);
  static_assert (s.template [:members_of (^^S, uctx)[0]:] <44> == 44);
  static_assert (s.template [:members_of (^^S, uctx)[0]:] <45> == 45);
  static_assert (s.template [:^^S::a:] <44> == 44);
  static_assert (s.template [:^^S::a:] <45> == 45);
  constexpr B t;
  static_assert (t.template a <42> == 142);
  static_assert (t.template a <43> == 143);
  static_assert (t.template [:members_of (^^S, uctx)[0]:] <44> == 44);
  static_assert (t.template [:members_of (^^S, uctx)[0]:] <45> == 45);
  static_assert (t.template [:members_of (^^T, uctx)[0]:] <44> == 144);
  static_assert (t.template [:members_of (^^T, uctx)[0]:] <45> == 145);
  static_assert (t.template [:^^S::a:] <44> == 44);
  static_assert (t.template [:^^S::a:] <45> == 45);
  static_assert (t.template [:^^T::a:] <44> == 144);
  static_assert (t.template [:^^T::a:] <45> == 145);
}

template <typename A, typename B>
void
baz ()
{
  constexpr S s;
  static_assert (s.a <42> == 42);
  static_assert (s.a <43> == 43);
  static_assert (s.template [:members_of (^^A, uctx)[0]:] <44> == 44);
  static_assert (s.template [:members_of (^^A, uctx)[0]:] <45> == 45);
#if 0
  // TODO: This doesn't work yet.
  static_assert (s.template [:^^A::a:] <44> == 44);
  static_assert (s.template [:^^A::a:] <45> == 45);
#endif
  constexpr T t;
  static_assert (t.a <42> == 142);
  static_assert (t.a <43> == 143);
  static_assert (t.template [:members_of (^^A, uctx)[0]:] <44> == 44);
  static_assert (t.template [:members_of (^^A, uctx)[0]:] <45> == 45);
#if 0
  // TODO: This doesn't work yet.
  static_assert (t.template [:^^A::a:] <44> == 44);
  static_assert (t.template [:^^A::a:] <45> == 45);
#endif
  static_assert (t.template [:members_of (^^B, uctx)[0]:] <44> == 144);
  static_assert (t.template [:members_of (^^B, uctx)[0]:] <45> == 145);
#if 0
  // TODO: This doesn't work yet.
  static_assert (t.template [:^^B::a:] <44> == 144);
  static_assert (t.template [:^^B::a:] <45> == 145);
#endif
}

template <typename A, typename B>
void
qux ()
{
  constexpr A s;
  static_assert (s.template a <42> == 42);
  static_assert (s.template a <43> == 43);
  static_assert (s.template [:members_of (^^A, uctx)[0]:] <44> == 44);
  static_assert (s.template [:members_of (^^A, uctx)[0]:] <45> == 45);
#if 0
  // TODO: This doesn't work yet.
  static_assert (s.template [:^^A::a:] <44> == 44);
  static_assert (s.template [:^^A::a:] <45> == 45);
#endif
  constexpr B t;
  static_assert (t.template a <42> == 142);
  static_assert (t.template a <43> == 143);
  static_assert (t.template [:members_of (^^A, uctx)[0]:] <44> == 44);
  static_assert (t.template [:members_of (^^A, uctx)[0]:] <45> == 45);
#if 0
  // TODO: This doesn't work yet.
  static_assert (t.template [:^^A::a:] <44> == 44);
  static_assert (t.template [:^^A::a:] <45> == 45);
#endif
  static_assert (t.template [:members_of (^^B, uctx)[0]:] <44> == 144);
  static_assert (t.template [:members_of (^^B, uctx)[0]:] <45> == 145);
#if 0
  // TODO: This doesn't work yet.
  static_assert (t.template [:^^B::a:] <44> == 144);
  static_assert (t.template [:^^B::a:] <45> == 145);
#endif
}

void
fred ()
{
  foo <42> ();
  bar <S, T> ();
  baz <S, T> ();
  qux <S, T> ();
}
