// PR c++/108503
// { dg-do compile { target c++17 } }
// { dg-additional-options "-Wall" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}
struct A {
  template <int I> int get () { return 1; }
};
template <> struct std::tuple_size <A> { static const int value = 3; };
template <int I> struct std::tuple_element <I, A> { using type = int; };

struct B {
  A *begin ();
  A *end ();
};

void
foo (B a)
{
  #pragma omp for collapse(2)
  for (auto [i, j, k] : a)
    for (int l = i; l < j; l += k)	// { dg-error "initializer expression refers to iteration variable 'i'" }
      ;					// { dg-error "condition expression refers to iteration variable 'j'" "" { target *-*-* } .-1 }
}					// { dg-error "increment expression refers to iteration variable 'k'" "" { target *-*-* } .-2 }
