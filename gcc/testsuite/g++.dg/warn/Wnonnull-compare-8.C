// PR c++/70295
// { dg-do compile }
// { dg-options "-O2 -Wnonnull-compare" }

struct A { A (); virtual ~A (); bool foo (bool); };
struct B : virtual public A { B (); virtual ~B (); };

bool
A::foo (bool x)
{
  if (x && dynamic_cast<B *>(this) != (B *) 0)	// { dg-bogus "nonnull argument" }
    return true;
  return false;
}
