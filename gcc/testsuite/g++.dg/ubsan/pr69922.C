// PR c++/69922
// { dg-do compile }
// { dg-options "-fsanitize=vptr -Wnonnull-compare" }

struct S { virtual ~S (); };
struct T : S { T *bar (); T *baz (); T *q; bool b; };

T *
T::bar ()
{
  return static_cast<T*>(reinterpret_cast<S*>(this));	// { dg-bogus "nonnull argument" }
}

T *
T::baz ()
{
  return static_cast<T*>(reinterpret_cast<S*>(b ? this : q));	// { dg-bogus "nonnull argument" }
}
