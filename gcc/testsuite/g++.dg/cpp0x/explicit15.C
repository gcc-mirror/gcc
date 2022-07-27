// PR c++/87820
// { dg-do compile { target c++11 } }

struct A {
  constexpr explicit operator int() const { return 0; }
};

template<class T>
void f() {
  A a;
  T t1 = a; // { dg-error "cannot convert" }
  T t2 = {a}; // { dg-error "cannot convert" }
  T t3(a);
  T t4{a};
  T t5 = T(a);
  T t6 = T{a};
  new T(a);
  new T{a};
}

template<class T>
void g() {
  T t;
  int n1 = t; // { dg-error "cannot convert" }
  int n2 = {t}; // { dg-error "cannot convert" }
  int n3(t);
  int n4{t};
  int n5 = int(t);
  int n6 = int{t};
  new int(t);
  new int{t};
}

template void f<int>();
template void g<A>();

template<class T>
struct B {
  static constexpr A a{};
  static constexpr T t1 = a; // { dg-error "cannot convert" }
  static constexpr T t2 = {a}; // { dg-error "cannot convert" }
  static constexpr T t4{a};
  static constexpr T t5 = T(a);
  static constexpr T t6 = T{a};
};

template<class T>
struct C {
  static constexpr T t{};
  static constexpr int n1 = t; // { dg-error "cannot convert" }
  static constexpr int n2 = {t}; // { dg-error "cannot convert" }
  static constexpr int n4{t};
  static constexpr int n5 = int(t);
  static constexpr int n6 = int{t};
};

template struct B<int>;
template struct C<A>;

#if __cpp_inline_variables
template<class T>
struct D {
  static inline A a;
  static inline T t1 = a; // { dg-error "cannot convert" "" { target c++17 } }
  static inline T t2 = {a}; // { dg-error "cannot convert" "" { target c++17 } }
  static inline T t4{a};
  static inline T t5 = T(a);
  static inline T t6 = T{a};
};

template<class T>
struct E {
  static inline T t;
  static inline int n1 = t; // { dg-error "cannot convert" "" { target c++17 } }
  static inline int n2 = {t}; // { dg-error "cannot convert" "" { target c++17 } }
  static inline int n4{t};
  static inline int n5 = int(t);
  static inline int n6 = int{t};
};

template struct D<int>;
template struct E<A>;
#endif
