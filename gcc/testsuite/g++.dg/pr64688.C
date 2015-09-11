// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=c++11 -O3 -march=westmere" }

template <typename T> struct A { typedef typename T::next type; };
template <typename> struct B;
template <typename T> struct N : T {};
template <int N> struct C {
  static const int value = N;
  typedef C<N + 1> next;
};
template <typename Sequence>
struct R : N<typename B<typename Sequence::tag>::template P<Sequence>> {};
template <typename Base> struct O : Base {
  typedef typename A<typename Base::size>::type size;
};
template <typename = int> struct D {
  typedef int tag;
  typedef C<0> size;
};
template <> struct B<int> {
  template <typename> struct P : O<O<O<D<>>>>::size {};
};
template <typename> struct F;
template <typename> struct G;
template <typename, typename, int> struct H;
template <typename Element, typename Layout> struct H<Element, Layout, 3> {};
template <int, typename E, typename L, int N> unsigned char at_c(H<E, L, N>) {}
template <typename> class I;
template <typename> class J;
template <typename> class K;
template <typename, typename> struct Q;
struct L {
  typedef Q<unsigned char, F<O<O<O<D<>>>>>> *type;
};
template <typename XIterator> struct M { typedef K<J<I<XIterator>>> view_t; };
template <typename, typename>
struct Q : H<unsigned, F<int>, R<O<O<O<D<>>>>>::value> {};
template <typename Iterator> struct G<I<Iterator>> { typedef Iterator type; };
template <typename> class J {
public:
  typedef G<I<Q<unsigned, int> *>>::type x_iterator;
};
template <typename> class K {
public:
  J<int>::x_iterator row_begin(int);
};
template <typename Op> void measure_time(Op p1) { p1(); }
template <typename, typename> struct fill_nongil_t;
template <typename T, typename P>
struct fill_nongil_t<K<J<I<Q<T, F<O<O<O<D<>>>>>> *>>>, P> {
  typedef K<J<I<Q<T, F<O<O<O<D<>>>>>> *>>> View;
  View _v;
  P _p;
  fill_nongil_t(View, P);
  void operator()() {
    T *first = (T *)_v.row_begin(0);
    T last;
    while (first != &last) {
      first[0] = first[1] = at_c<1>(_p);
      first[2] = at_c<2>(_p);
      first += 3;
    }
  }
};
template <typename, typename> void test_fill(int) {
  M<L::type>::view_t __trans_tmp_1;
  measure_time(fill_nongil_t<K<J<I<Q<unsigned char, F<O<O<O<D<>>>>>> *>>>,
                             Q<unsigned char, F<O<O<O<D<>>>>>>>(
      __trans_tmp_1, Q<unsigned char, F<O<O<O<D<>>>>>>()));
}
void performance_testtest_method() { test_fill<K<int>, Q<unsigned, int>>(0); }
