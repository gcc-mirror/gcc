// PR c++/102535
// Verify __is_trivially_constructible works with multi-arg paren init of
// aggrs.
// { dg-do compile { target c++11 } }

struct A { int x; };
struct B { float y; };
struct C { char z; };
struct D { A a; B b; C c; };

template<class T, class... Ts>
struct is_trivially_constructible {
  static const bool value = __is_trivially_constructible(T, Ts...);
};

extern int n[1 + is_trivially_constructible<D, A>::value];
extern int n[1 + is_trivially_constructible<D, A, B>::value];
extern int n[1 + is_trivially_constructible<D, A, B, C>::value];
#if __cpp_aggregate_paren_init
extern int n[1 + true];
#else
extern int n[1 + false];
#endif
