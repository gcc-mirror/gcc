// Runtime version of cond3.C.  We call terminate when the A cleanup throws
// because we've already initialized the exception object.
// { dg-do run }

#include <exception>
#include <cstdlib>

void my_terminate ()
{
  std::exit (0);
}

struct A {
  A(int) { }
  ~A()
#if __cplusplus <= 201402L
  throw(int)			// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#else
  noexcept(false)
#endif
  { throw 1; };
};
struct B {
  B(A) { }
  ~B() { }
};
bool b;

int main()
{
  std::set_terminate (my_terminate);
  try
    {
      throw b ? B(1) : B(1);
    }
  catch (...) { }
  return 1;
}
