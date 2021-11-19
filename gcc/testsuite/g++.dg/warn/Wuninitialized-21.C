// PR c++/19808
// { dg-do compile }
// { dg-options "-Wuninitialized" }

struct A {
  int a;
  int b;
  A(int) {}
};

struct S {
  A a;
  A a2;
  S() :
    /* We don't warn here, because we consider partial initialization
       as initializing the whole object.  */
    a((a2.a = 42)),
    a2(a2.a)
  { }
};
