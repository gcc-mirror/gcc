// { dg-do compile { target c++11 } }

inline namespace N __attribute__((__abi_tag__ ("foo"))) {}
template <typename> struct A;
namespace N {
template <typename> class B;
}
template <typename> class C {};
template <typename> struct D {
  template <typename _Up> using G = C<_Up>;
};
template <typename T> struct F {
  template <typename U> struct H {
    typedef typename D<T>::template G<U> I;
  };
};
template <typename T, typename = C<T>> struct J {
  C<A<const B<char>>> L;
  typedef F<C<int>>::H<A<const B<char>>>::I M;
  J<M> *a;
};
