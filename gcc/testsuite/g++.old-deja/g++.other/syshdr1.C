// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>
// Special g++ Options:

# 1 "foo" 3
void f () {}
extern "C" void foo(int);
extern "C" void foo(int) throw();
