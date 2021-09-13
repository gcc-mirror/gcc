// PR c++/97034
// { dg-do compile { target c++20 } }

namespace N {
template <typename, typename> struct S {
  template <typename T, typename U> S(T, U);
};
} // namespace N
template <int I> struct E {
  template<typename U> struct M {
    template <typename T> struct G { T t; };
    void fn() { G{N::S<char, int>{'a', 1}}; }
  };
};

void
g ()
{
  E<1>::M<int> m;
  m.fn ();
}
