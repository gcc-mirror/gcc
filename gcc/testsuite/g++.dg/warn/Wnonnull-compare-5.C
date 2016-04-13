// PR c++/69902
// { dg-do compile }
// { dg-options "-Wall" }

struct A { virtual ~A (); };
struct B : A {};

bool
foo (A &a)
{
  return dynamic_cast<B *>(&a) == (B *) 0;	// { dg-bogus "nonnull argument" }
}

bool
bar (A &a)
{
  return dynamic_cast<B *>(&a) != (B *) 0;	// { dg-bogus "nonnull argument" }
}
