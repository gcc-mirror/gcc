// { dg-do assemble  }
// { dg-options "" }
// Origin: Mark Mitchell <mark@codesourcery.com>

# 1 "foo" 3
void f () {}
extern "C" void foo(int);
extern "C" void foo(int) throw();
