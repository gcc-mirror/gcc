// Test that checking of a nothrow specification uses the one on the
// definition.  In C++17 throw() is equivalent to noexcept(true).
// { dg-do run { target { c++11 && c++14_down } } }

#include <exception>
#include <cstdlib>

void my_unexpected ()
{
  std::exit (0);
}

void f() noexcept;
void f() throw()
{
  throw 1;
}

int main()
{
  std::set_unexpected (my_unexpected);
  f();
  return 1;
}
