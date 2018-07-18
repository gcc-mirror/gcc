// PR c++/83958
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename = void> struct A;
class B;
template <typename, typename, typename = A<>> class C;
template <typename, typename> struct D;
template <typename T, typename U, typename V, typename, typename, typename W>
struct E {
  using X = W;
  X operator* ();
  T operator++ ();
  template <typename P, typename R, typename S, typename Q>
  bool operator!= (E<P, U, V, R, S, Q>);
};
template <typename T, typename U, typename>
struct F {
  class G;
  using H = D<T, U>;
  using I = E<G, T, U, G, H, H &>;
  class G : public I {};
  G begin ();
  G end ();
};
template <typename T, typename U, typename V> struct C : F<T, U, V> {
  using J = F<T, U, V>;
  using J::begin;
  using J::end;
};
using K = class L;
struct M {
  void foo () { for (auto & [ a ] : m) {} }	// { dg-error "incomplete type" }
  C<K, B> m;					// { dg-warning "only available with" "" { target c++14_down } .-1 }
};
