// PR c++/126654
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

int v;

namespace N {
  struct S {};
  namespace M {
    struct S {};
    int v;
  }
  int v;
}
namespace A = N;
struct S {};
enum class E { S };
struct C { struct S {}; };

template<auto ns>
void
f ()
{
  using [:ns:]::S;
  using [:ns:]::v;
  v = 42;
}

template<auto ns>
void
fe ()
{
  using [:ns:]::S;
  auto e = S;
}

template<auto ns>
void
f2 ()
{
  using [:ns:]::S; // { dg-error "expected a reflection of a class, namespace, or enumeration" }
// { dg-message "but .int. is a type" "" { target *-*-* } .-1 }
}

template<auto ns>
void
f3 ()
{
  using [:ns:]::S;  // { dg-error "using-declaration for member at non-class scope" }
}

template<typename T>
void
f4 ()
{
  using T::x;  // { dg-error "is not a class, namespace, or enumeration" }
}

void
g ()
{
  f<^^::>();
  f<^^N>();
  f<^^A>();
  f<^^N::M>();
  fe<^^E>();
  f2<^^int>();
  f3<^^C>();
  f4<int>();
}
