// PR c++/53574
// { dg-do compile { target c++11 } }
// { dg-options "-fstack-usage" }

template <typename> struct A { typedef int type; };
struct B {
  typedef __SIZE_TYPE__ H;
};
template <typename> class allocator : public B {};
template <typename _Alloc> struct C {
  template <typename T>
  static typename T::H foo(T *);
  typedef decltype(foo((_Alloc *)0)) H;
  template <typename U>
  static typename A<H>::type bar(U) { return typename A<H>::type (); }
  static int baz(_Alloc p1) { bar(p1); return 0; }
};
template <typename _Alloc> struct I : C<_Alloc> {};
template <typename, typename> struct J {
  typedef I<allocator<int>> K;
  K k;
};
struct D : J<int, allocator<int>> {
  void fn(int, int) {
    K m;
    I<K>::baz(m);
  }
};
template <class Ch, class = int, class = int> struct F {
  F();
  F(const Ch *);
  F test();
  D d;
};
int l;
struct G {
  G(F<char>);
};
char n;
template <class Ch, class Tr, class Alloc> F<Ch, Tr, Alloc>::F(const Ch *) {
  test();
}
template <class Ch, class Tr, class Alloc>
F<Ch, Tr, Alloc> F<Ch, Tr, Alloc>::test() {
  d.fn(l, 0);
  return F<Ch, Tr, Alloc> ();
}
G fn1() { return G(&n); }
