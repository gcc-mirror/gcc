// Make sure that we call terminate when a throw() spec is violated even
// with -fnothrow-opt.  The function pointers are there to make sure that
// the compiler doesn't get clever about optimizing the calls based on
// knowledge about the called functions.

// { dg-options "-fnothrow-opt" }
// { dg-do run }

#include <exception>
#include <cstdlib>

void my_terminate ()
{
  std::exit (0);
}

void g() { throw 1; }
void (*p1)() = g;
void f() throw() { p1(); }
void (*p2)() = f;
void h() { p2(); }

int main()
{
  std::set_terminate (my_terminate);

  try { h(); }
  catch (int) { }

  return 1;
}
