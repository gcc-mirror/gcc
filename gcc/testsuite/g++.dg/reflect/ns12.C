// PR c++/127707
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

namespace N {
  struct S {};
  struct R {};
}
namespace A = N;
struct S {};

using typename [:^^N:]::R;
R r;

template<auto ns>
void
f ()
{
  using typename [:ns:]::S;
  S s;
}

template<auto ns>
void
f2 ()
{
  [] (auto) {
    using typename [:ns:]::S;
    S s;
  } (0);
}

template<auto ns>
void
f3 ()
{
  [] <auto ns2> () {
    using typename [:ns2:]::S;
    S s;
  }.template operator()<ns>();
}

template<auto ns>
void
fbad ()
{
  using [:ns:]::S;
  S s; // { dg-error "expected" }
}

void
g ()
{
  f<^^::>();
  f<^^N>();
  f<^^A>();
  f2<^^::>();
  f2<^^N>();
  f2<^^A>();
  f3<^^::>();
  f3<^^N>();
  f3<^^A>();
}
