// PR 3719
// Test that an unexpected handler can rethrow to categorize.
// { dg-do run }

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
doit () throw (Two)
{
  throw One ();
}

int main ()
{
  std::set_unexpected (handle_unexpected);

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
