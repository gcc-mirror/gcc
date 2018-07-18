// PR c++/78313 (see also PR c++/72774)
// { dg-do compile }

void baz ();
namespace A { void foo (); }
void bar ()
{
  using A::foo;
  0 ? static_cast<foo> (0) : baz; // { dg-bogus "did you mean" }
  // { dg-error "does not name a type" "" { target *-*-* } .-1 }
}
