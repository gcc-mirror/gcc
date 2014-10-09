// PR c++/61489
// { dg-options "-Wmissing-field-initializers" }

struct mystruct1 {
  int a, b;
};

struct aux2 {
  aux2();
};

struct mystruct2 {
  aux2 a, b;
};

struct aux3 {
  int x;
};

struct mystruct3 {
  aux3 a, b;
};

mystruct1 obj11 = {};
mystruct1 obj12 = {0};       // { dg-warning "missing initializer" }

mystruct2 obj21 = {};
mystruct2 obj22 = {aux2()};  // { dg-warning "missing initializer" }

mystruct3 obj31 = {};
mystruct3 obj32 = {0};       // { dg-warning "missing initializer" }
