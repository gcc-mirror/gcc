// { dg-do compile { target c++11 } }
// { dg-options "-fconcepts" }

// This is like pr84979-2.C, except that we swap the types passed to
// the template functions foo1 and foo2, so that the expectations WRT
// D's typeness are not met.

template <typename T>
void foo1(T& t) {
  typename T::template C<void> tcv = t;
  typename T::template C<auto> u = tcv; // { dg-error "" }
  T::template C<auto>::f (tcv, u); // { dg-error "" }
  (typename T::template D<auto> (t)); // { dg-error "" }
}

struct T1 {
  template <typename T> struct C {
    C(T1&);
    static void f(T1&, C&);
  };
  template <typename T> struct D {
    D(T1&);
  };
};

template <typename T>
void foo2(T& t) {
  typename T::template C<void> tcv = t;
  typename T::template C<auto> u = tcv; // { dg-error "" }
  T::template C<auto>::f (tcv, u); // { dg-error "" }
  T::template D<auto> (t); // { dg-error "" }
}

struct T2 {
  template <typename T> struct C {
    C(T2&);
    static void f(T2&, C&);
  };
  template <typename T> static void D(T2&);
};

void f(T1& t1, T2& t2) {
  foo1 (t2);
  foo2 (t1);
}
