// PR c++/27601
// Origin: Patrik Hägglund  <patrik.hagglund@bredband.net>
// { dg-do compile }

struct bar {
  static int foo;
};

int a = __builtin_offsetof(bar, foo);  // { dg-error "static data member" }
