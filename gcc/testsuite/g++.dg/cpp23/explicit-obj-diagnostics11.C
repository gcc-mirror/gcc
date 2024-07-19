// PR c++/115783
// { dg-do compile { target c++23 } }

struct A {
  int f(this auto);

  static void s() {
    f(); // { dg-error "without object" }
  }
};

int n = A::f(); // { dg-error "without object" }

struct B {
  void ns() {
    A::f(); // { dg-error "without object" }
  }

  static void s() {
    A::f(); // { dg-error "without object" }
  }
};

template<class T>
struct C {
  void ns() {
    A::f(); // { dg-error "without object" }
    T::f(); // { dg-error "without object" }
  }

  static void s() {
    A::f(); // { dg-error "without object" }
    T::f(); // { dg-error "without object" }
  };
};

template struct C<A>;

template<class T>
struct D : T {
  void ns() {
    A::f(); // { dg-error "without object" }
    T::f(); // { dg-error "not a member of 'B'" }
  }
};

template struct D<B>; // { dg-message "required from here" }
template struct D<A>; // { dg-bogus "required from here" }
