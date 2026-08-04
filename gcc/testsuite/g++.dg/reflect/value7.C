// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do run { target c++26 } }
// { dg-additional-options "-freflection" }
// Like value6.C, but with function-local static.

using info = decltype(^^::);

struct S
{
  info i;
  int n;
};
struct Holder { const info *p; int n; };

void
g ()
{
  constexpr static info arr[] = { ^^int, ^^double, ^^char };
  constexpr static S s = { ^^int, 42 };
  constexpr static S sarr[2] = { { ^^int, 1 }, { ^^double, 2 } };
  constexpr static const info *p = arr;

  constexpr const void *vp = &p;
  constexpr const auto p1 = &arr[0];
  constexpr const auto p2 = arr;
  constexpr const auto p3 = arr + 1;
  constexpr const auto p4 = arr + 3;
  constexpr const auto p5 = &s;
  constexpr const auto p6 = &s.i;
  constexpr const auto p7 = &sarr[1];
  constexpr const auto p8 = &sarr[1].i;
  constexpr bool cond = true;
  constexpr auto p_cond = cond ? &arr[0] : &arr[1];
  constexpr Holder h = { &arr[0], 1 };
  constexpr auto cpdm = &S::i;

  constexpr static const void *svp = &p;
  constexpr static const auto sp1 = &arr[0];
  constexpr static const auto sp2 = arr;
  constexpr static const auto sp3 = arr + 1;
  constexpr static const auto sp4 = arr + 3;
  constexpr static const auto sp5 = &s;
  constexpr static const auto sp6 = &s.i;
  constexpr static const auto sp7 = &sarr[1];
  constexpr static const auto sp8 = &sarr[1].i;
  constexpr static auto sp_cond = cond ? &arr[0] : &arr[1];
  constexpr static Holder sh = { &arr[0], 1 };
  constexpr static auto scpdm = &S::i;
}

int main () {}
