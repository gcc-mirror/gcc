// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct S { int _; long _; short _; };
struct T { int _; int _; int _; };
struct U { enum E { F, G, H, I } j, k; };
struct V : S, T { int _; int _; };

constexpr access_context uctx = access_context::unchecked ();

void
foo ()
{
  S s = { 1, 2, 3 };
  s.[:members_of (^^S, uctx)[1]:] += 10;
  s.[:members_of (^^S, uctx)[0]:] += 20;
  s.[:members_of (^^S, uctx)[2]:] += 30;
  auto [ sa, sb, sc ] = s;
  if (sa != 21 || sb != 12 || sc != 33)
    __builtin_abort ();
  T t = { 4, 5, 6 };
  t.[:members_of (^^T, uctx)[1]:] += 40;
  t.[:members_of (^^T, uctx)[2]:] += 50;
  t.[:members_of (^^T, uctx)[0]:] += 60;
  auto [ ta, tb, tc ] = t;
  if (ta != 64 || tb != 45 || tc != 56)
    __builtin_abort ();
  U u = { U::F, U::I };
  u.[:nonstatic_data_members_of (^^U, uctx)[0]:] = u.[:^^U::E::G:];
  u.[:nonstatic_data_members_of (^^U, uctx)[1]:] = u.[:^^U::H:];
  if (u.j != U::G || u.k != U::E::H)
    __builtin_abort ();
  V v = {};
  v.[:members_of (^^S, uctx)[1]:] = 1;
  v.[:members_of (^^T, uctx)[2]:] = 2;
  v.[:members_of (^^V, uctx)[0]:] = 3;
}

template <int N>
void
bar ()
{
  S s = { 1, 2, 3 };
  s.[:members_of (^^S, uctx)[1]:] += 10;
  s.[:members_of (^^S, uctx)[0]:] += 20;
  s.[:members_of (^^S, uctx)[2]:] += 30;
  auto [ sa, sb, sc ] = s;
  if (sa != 21 || sb != 12 || sc != 33)
    __builtin_abort ();
  T t = { 4, 5, 6 };
  t.[:members_of (^^T, uctx)[1]:] += 40;
  t.[:members_of (^^T, uctx)[2]:] += 50;
  t.[:members_of (^^T, uctx)[0]:] += 60;
  auto [ ta, tb, tc ] = t;
  if (ta != 64 || tb != 45 || tc != 56)
    __builtin_abort ();
  U u = { U::F, U::I };
  u.[:nonstatic_data_members_of (^^U, uctx)[0]:] = u.[:^^U::E::G:];
  u.[:nonstatic_data_members_of (^^U, uctx)[1]:] = u.[:^^U::H:];
  if (u.j != U::G || u.k != U::E::H)
    __builtin_abort ();
  V v = {};
  v.[:members_of (^^S, uctx)[1]:] = 1;
  v.[:members_of (^^T, uctx)[2]:] = 2;
  v.[:members_of (^^V, uctx)[0]:] = 3;
}

template <typename A, typename B, typename C, typename D>
void
baz ()
{
  A s = { 1, 2, 3 };
  s.[:members_of (^^S, uctx)[1]:] += 10;
  s.[:members_of (^^S, uctx)[0]:] += 20;
  s.[:members_of (^^S, uctx)[2]:] += 30;
  auto [ sa, sb, sc ] = s;
  if (sa != 21 || sb != 12 || sc != 33)
    __builtin_abort ();
  B t = { 4, 5, 6 };
  t.[:members_of (^^T, uctx)[1]:] += 40;
  t.[:members_of (^^T, uctx)[2]:] += 50;
  t.[:members_of (^^T, uctx)[0]:] += 60;
  auto [ ta, tb, tc ] = t;
  if (ta != 64 || tb != 45 || tc != 56)
    __builtin_abort ();
  C u = { U::F, U::I };
  u.[:nonstatic_data_members_of (^^U, uctx)[0]:] = u.[:^^U::E::G:];
  u.[:nonstatic_data_members_of (^^U, uctx)[1]:] = u.[:^^U::H:];
  if (u.j != U::G || u.k != U::E::H)
    __builtin_abort ();
  D v = {};
  v.[:members_of (^^S, uctx)[1]:] = 1;
  v.[:members_of (^^T, uctx)[2]:] = 2;
  v.[:members_of (^^V, uctx)[0]:] = 3;
}

template <typename A, typename B, typename C, typename D>
void
qux ()
{
  S s = { 1, 2, 3 };
  s.[:members_of (^^A, uctx)[1]:] += 10;
  s.[:members_of (^^A, uctx)[0]:] += 20;
  s.[:members_of (^^A, uctx)[2]:] += 30;
  auto [ sa, sb, sc ] = s;
  if (sa != 21 || sb != 12 || sc != 33)
    __builtin_abort ();
  T t = { 4, 5, 6 };
  t.[:members_of (^^B, uctx)[1]:] += 40;
  t.[:members_of (^^B, uctx)[2]:] += 50;
  t.[:members_of (^^B, uctx)[0]:] += 60;
  auto [ ta, tb, tc ] = t;
  if (ta != 64 || tb != 45 || tc != 56)
    __builtin_abort ();
  U u = { U::F, U::I };
  u.[:nonstatic_data_members_of (^^C, uctx)[0]:] = u.[:^^C::E::G:];
  u.[:nonstatic_data_members_of (^^C, uctx)[0]:] = u.[:^^C::G:];
  u.[:nonstatic_data_members_of (^^C, uctx)[1]:] = u.[:^^C::H:];
  if (u.j != U::G || u.k != U::E::H)
    __builtin_abort ();
  V v = {};
  v.[:members_of (^^A, uctx)[1]:] = 1;
  v.[:members_of (^^B, uctx)[2]:] = 2;
  v.[:members_of (^^D, uctx)[0]:] = 3;
}

template <typename A, typename B, typename C, typename D>
void
fred ()
{
  A s = { 1, 2, 3 };
  s.[:members_of (^^A, uctx)[1]:] += 10;
  s.[:members_of (^^A, uctx)[0]:] += 20;
  s.[:members_of (^^A, uctx)[2]:] += 30;
  auto [ sa, sb, sc ] = s;
  if (sa != 21 || sb != 12 || sc != 33)
    __builtin_abort ();
  B t = { 4, 5, 6 };
  t.[:members_of (^^B, uctx)[1]:] += 40;
  t.[:members_of (^^B, uctx)[2]:] += 50;
  t.[:members_of (^^B, uctx)[0]:] += 60;
  auto [ ta, tb, tc ] = t;
  if (ta != 64 || tb != 45 || tc != 56)
    __builtin_abort ();
  C u = { U::F, U::I };
  u.[:nonstatic_data_members_of (^^C, uctx)[0]:] = u.[:^^C::E::G:];
  u.[:nonstatic_data_members_of (^^C, uctx)[0]:] = u.[:^^C::G:];
  u.[:nonstatic_data_members_of (^^C, uctx)[1]:] = u.[:^^C::H:];
  if (u.j != U::G || u.k != U::E::H)
    __builtin_abort ();
  D v = {};
  v.[:members_of (^^A, uctx)[1]:] = 1;
  v.[:members_of (^^B, uctx)[2]:] = 2;
  v.[:members_of (^^D, uctx)[0]:] = 3;
}

int
main ()
{
  foo ();
  bar <42> ();
  baz <S, T, U, V> ();
  qux <S, T, U, V> ();
  fred <S, T, U, V> ();
}
