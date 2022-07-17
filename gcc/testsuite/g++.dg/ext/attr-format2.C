// PR c++/101833
// { dg-do compile }
// { dg-options "-Wall" }

struct B { };

struct V : virtual B {
  V(int, const char *, ...) __attribute__((format(printf, 3, 4)));
};

struct D : B {
  D(int, const char *, ...) __attribute__((format(printf, 3, 4)));
};

struct D2 : B {
  template<typename T>
  D2(T, const char *, ...) __attribute__((format(printf, 3, 4)));
};

struct V2 : virtual B {
  template<typename T>
  V2(T, const char *, ...) __attribute__((format(printf, 3, 4)));
};

struct X {
  template<typename T>
  X(T, ...) __attribute__((format(printf, 2, 3)));
};

V v(1, "%s %d", "foo", 1);
D d(1, "%s %d", "foo", 1);
D2 d2(1, "%s %d", "foo", 1);
V2 v2(1, "%s %d", "foo", 1);

// Test that it actually works.
V e1(1, "%d", 1L); // { dg-warning "expects argument of type" }
D e2(1, "%d", 1L); // { dg-warning "expects argument of type" }
X e3("%d", 1L); // { dg-warning "expects argument of type" }
