// P2893R3 - Variadic friends
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <class... Ts>
class A {
  class X {};
  friend Ts...;		// { dg-warning "variadic friends or friend type declarations with multiple types only available with" "" { target c++23_down } }
};
template <class... Ts, class... Us>
class A<A<Ts...>, A<Us...>> {
  class X {};
  friend
#if __cplusplus < 202002L
  typename
#endif
  Ts::Y..., Us...;	// { dg-warning "variadic friends or friend type declarations with multiple types only available with" "" { target c++23_down } }
};
template <typename T, typename U>
class B {
  class X {};
  friend T, U;		// { dg-warning "variadic friends or friend type declarations with multiple types only available with" "" { target c++23_down } }
};
template <typename T, typename U, typename... Vs>
class C {
  class X {};
  friend U, Vs..., T;	// { dg-warning "variadic friends or friend type declarations with multiple types only available with" "" { target c++23_down } }
};
class E;
class F;
class G;
class H;
class I;
class J;
class K;
class L;
class M;
class N;
class O;
class P;
class E : A<E, F>::X {};
class F : A<E, F>::X {};
class G : B<G, H>::X {};
class H : B<G, H>::X {};
class I : C<I, J>::X {};
class J : C<I, J>::X {};
class K : C<K, L, M, N, O>::X {};
class L : C<K, L, M, N, O>::X {};
class M : C<K, L, M, N, O>::X {};
class N : C<K, L, M, N, O>::X {};
class O : C<K, L, M, N, O>::X {};
struct Q { class Y : A<A<Q>, A<P, long>>::X {}; };
class P : A<A<Q>, A<P, long>>::X {};
struct R { class Y; };
struct S { class Y; };
class R::Y : A<A<R, S>, A<P, double>>::X {};
class S::Y : A<A<R, S>, A<P, double>>::X {};
A<int> a;
