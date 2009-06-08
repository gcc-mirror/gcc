// PR c++/40373
// { dg-compile }

struct A;	// { dg-bogus "candidates are" "" { xfail *-*-* } }
namespace
{
  struct A;	// { dg-bogus "struct" "" { xfail *-*-* } }
}

struct B {};

template <typename T> void
foo (T t)
{
  t.~A ();	// { dg-error "does not match destructor name" }
}

void
bar ()
{
  foo (B ());	// { dg-bogus "instantiated from here" "" { xfail *-*-* } }
}

// { dg-bogus "is ambiguous" "" { xfail *-*-* } 15 }
