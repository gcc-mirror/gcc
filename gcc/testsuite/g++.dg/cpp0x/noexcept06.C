// Test that checking of a nothrow specification uses the one on the
// definition.
// { dg-do run { target c++11 } }
// { dg-options "-Wno-terminate" }

#include <exception>
#include <cstdlib>

void my_unexpected ()
{
  std::abort ();
}
void my_terminate ()
{
  std::exit (0);
}

void f() throw();
void f() noexcept
{
  throw 1;
}

int main()
{
  std::set_unexpected (my_unexpected);
  std::set_terminate (my_terminate);
  f();
  return 1;
}
