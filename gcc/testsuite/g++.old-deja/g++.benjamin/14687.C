// 981203 bkoz
// g++/14687
// excess errors test - XFAIL *-*-*

#include <assert.h>
unsigned int gtest;

// 7.3.3 the using declaration

// p 3
struct belieze {
  void f(char);
  void g(char);
  enum E { e };
  union { int x; };
};

struct dominica: belieze {
  using belieze::f;
  void f(int i) { f('c'); } // calls belieze::f(char)
  void g(int i) { g('c'); } // recursively calls dominca::g(int)
};


// p 6
namespace A {
  void f(int i) { gtest = 1; }
}

using A::f; 	 //f is a synonym for A::f, that is for A::f(int)

namespace A {
  void f(char c) { gtest = 3; }
}

void foo(void) {
  f('a'); 	 //calls f(int), even though A::f(char) exits
  assert (gtest = 1);
}

void bar(void) {
  using A::f;    //f is a synonm for A::f, that is for A::f(int) and A::f(char)
  f('a'); 	 //calls f(char)
  assert (gtest = 3);
}

int main(void)
{
  foo();
  bar();

  return 0;
}
