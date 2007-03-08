// PR c++/27601
// Origin: Patrik Hägglund  <patrik.hagglund@bredband.net>
// { dg-do compile }

struct bar {
  static int foo;
  static int baz();
};

int a = __builtin_offsetof(bar, foo);  // { dg-error "static data member" }
int av = __builtin_offsetof(volatile bar, foo);  // { dg-error "static data member" }
int b = __builtin_offsetof(bar, baz);  // { dg-error "member function" }
int b0 = __builtin_offsetof(bar, baz[0]);  // { dg-error "function" }
int bv0 = __builtin_offsetof(volatile bar, baz[0]);  // { dg-error "function" }
int c = __builtin_offsetof(bar, ~bar);  // { dg-error "member function" }

typedef int I;
enum E { };

int d = __builtin_offsetof(I, ~I);  // { dg-error "destructor" }
int e = __builtin_offsetof(E, ~E);  // { dg-error "destructor" }
