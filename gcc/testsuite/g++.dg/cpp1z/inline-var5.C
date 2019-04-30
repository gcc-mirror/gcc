// PR c++/89405
// { dg-do compile { target c++17 } }
// { dg-options "-fno-weak" }

template <int N>
struct S
{
  static constexpr int a = N;	// { dg-warning "semantics of inline variable" }
};				// { dg-message "you can work around this" "" { target *-*-* } .-1 }

const int *x = &S<0>::a;
// PR c++/87921
// { dg-do compile { target c++17 } }

template <class H>
struct X
{
  static inline long x[] = { 1L };
  long foo () { return x[0]; }
};

void
bar ()
{
  class L {};
  X<L> v {};
}
