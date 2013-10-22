// Test that checking of a nothrow specification uses the one on the
// definition.
// { dg-options "-std=c++11" }
// { dg-do run }

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
