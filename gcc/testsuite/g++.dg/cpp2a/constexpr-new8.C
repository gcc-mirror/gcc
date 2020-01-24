// PR c++/91369
// { dg-do compile { target c++2a } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

struct A {
  constexpr A () : p{new int} {}
  constexpr ~A () { delete p; }
  int *p;
};

constexpr bool
test ()
{
  A{};
  return true;
}

constexpr auto res = test ();
static_assert (res);
