// PR c++/13944

// Verify that we still call terminate() if we do run the copy constructor,
// and it throws.

// { dg-do run }

#include <cstdlib>
#include <exception>

struct A
{
  A() { }
  A(const A&) { throw 1; }
};

A a;

void
good_terminate() { std::exit (0); }

int main()
{
  std::set_terminate (good_terminate);
  try
    {
      throw a;
    }
  catch (...)
    {
      return 2;
    }
  return 3;
}
