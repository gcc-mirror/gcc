// PR c++/103480
// { dg-do compile { target c++14 } }
// { dg-options "-Wuseless-cast" }

template <typename T, int N>
struct A { typedef T t[N]; };
template <typename T, int N>
struct B { typename A<T, N>::t b; };
struct C {
  constexpr C (C &&) {}
  template <int N>
  static auto bar ()
  {
    B<C, N> r;
    return r;		// { dg-bogus "useless cast to type" }
  }
  C () = default;
};

void
foo ()
{
  C::bar<2> ();
}
