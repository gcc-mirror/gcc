// Build don't link: 
// GROUPS passed nest
#include <iostream.h>

struct inner {
  static void f() { cout << "inner::f()\n";}
};

struct outer {

  struct inner {
    static void f() { cout << "outer::inner::f()\n";}
  };

  static void f() {
    inner::f();     //call of outer::inner::f()
    ::inner::f();   //(try to) call inner::f() => parse error
  }
};

int main() {
  outer::f();
  cout << endl;
  return 0;
}
