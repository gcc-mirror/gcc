// PR c++/114439
// { dg-do compile { target c++11 } }

struct S {
  int *list = arr;
  __extension__ int arr[];
};

struct R {
  int *list = arr;
  int arr[2];
};

struct A {
  A() {}
  S s[2]{};
};

struct A2 {
  A2() {}
  S s[2]{ {}, {} };
};

struct B {
  B() {}
  R r[2]{};
};

struct B2 {
  B2() {}
  R r[2]{ {}, {} };
};

struct S1 { S1(); };
struct S2 {
  S2() {}
  S1 a[1] {};
};
