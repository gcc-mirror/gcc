// PR c++/79962
// { dg-options "-Wnonnull" }

template <class T>
__attribute__ ((__nonnull__ (T::i))) void f (typename T::U) { }

struct S1 { enum { i = 1 }; typedef void* U; };
struct S2 { static const int i = 1; typedef void* U; };

void
g ()
{
  f<S1>(0); // { dg-warning "null argument where non-null required" }
  f<S2>(0); // { dg-warning "null argument where non-null required" }
}
