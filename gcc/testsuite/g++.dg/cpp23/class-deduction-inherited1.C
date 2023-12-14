// Modified example from P2582R1
// { dg-do compile { target c++23 } }

template <typename T> struct B {
  B(T);
};
B(bool) -> B<char>;
template <typename T> struct C : public B<T> {
  using B<T>::B;
};
template <typename T> struct D : public B<T> {};

C c(42);            // OK, deduces C<int>
using ty1 = decltype(c);
using ty1 = C<int>;

D d(42);            // { dg-error "deduction|no match" }

C c2(true);           // OK, deduces C<char>
using ty2 = decltype(c2);
using ty2 = C<char>;

template <typename T> struct E : public B<int> {
  using B<int>::B;
};

E e(42);            // { dg-error "deduction|no match" }

template <typename T, typename U, typename V> struct F {
  F(T, U, V);
};
template <typename T, typename U> struct G : F<U, T, int> {
  using F<U, T, int>::F;
};

G g(true, 'a', 1);  // OK, deduces G<char, bool>
using ty3 = decltype(g);
using ty3 = G<char, bool>;
