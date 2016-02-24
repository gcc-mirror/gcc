// PR c++/69922
// { dg-do compile }
// { dg-options "-Wnonnull-compare" }

struct T { virtual ~T (); };
struct S { virtual ~S (); T *f (bool); };
struct U : S, T {};

T *
S::f (bool b)
{
  return b ? static_cast<U *> (this) : (U *) 0;	// { dg-bogus "nonnull argument" }
}
