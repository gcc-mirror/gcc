// { dg-do compile }
// { dg-options "-g" }
template <typename _Tp> using decay_t = _Tp;
template <typename> struct A;
template <typename> struct B { B(A<int>); };
template <typename> struct C {
      template <typename U> using constructor = B<decay_t<U>>;
        typedef constructor<int> dummy;
};
template <typename> struct D {};
C<int> a;
D<B<int>> fn1() { fn1, a; }
