// { dg-options "-fpermissive -w" }

struct A {
  static void g(int);
};

struct S {
  static int i;

  friend void f(int = i);
  friend void A::g(int = i);
};
