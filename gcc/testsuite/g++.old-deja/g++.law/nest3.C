// { dg-do assemble  }
// GROUPS passed nest
#include <iostream>

struct inner {
  static void f() { std::cout << "inner::f()\n";}
};

struct outer {

  struct inner {
    static void f() { std::cout << "outer::inner::f()\n";}
  };

  static void f() {
    inner::f();     //call of outer::inner::f()
    ::inner::f();   //(try to) call inner::f() => parse error
  }
};

int main() {
  outer::f();
  std::cout << std::endl;
  return 0;
}
