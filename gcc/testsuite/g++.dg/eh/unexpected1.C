// PR 3719
// Test that an unexpected handler can rethrow to categorize.
// { dg-do run { target c++14_down } }

#include <exception>

extern "C" void abort ();

struct One { };
struct Two { };

static void
handle_unexpected ()
{
  try
  {
    throw;
  }
  catch (One &)
  {
    throw Two ();
  }
}

static void
doit () throw (Two)			// { dg-warning "deprecated" "" { target { c++11 } } }
{
  throw One ();
}

int main ()
{
  std::set_unexpected (handle_unexpected); // { dg-warning "deprecated" "" { target { c++11 } } }

  try
  {
    doit ();
  }
  catch (Two &)
  {
  }
  catch (...)
  {
    abort ();
  }
}
