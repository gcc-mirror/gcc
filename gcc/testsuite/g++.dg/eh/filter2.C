// Test that terminate gets run when a catch filter fails to match while
// running destructors.  Original bug depended on a::~a being inlined.
// { dg-do run }
// { dg-options -O }
// { dg-skip-if "requires hosted libstdc++ for cstdlib abort" { ! hostedlib } }

#include <exception>
#include <cstdlib>

struct e1 {};
struct e2 {};

struct a
{
  a () { }

  ~a ()
    {
      try
	{
	  throw e1();
	}
      catch (e2 &)
	{
        }
    }
};

void
ex_test ()
{
  a aa;
  try
    {
      throw e1 ();
    }
  catch (e2 &)
    {
    }
}

void my_terminate ()
{
  std::exit (0);
}

int
main ()
{
  std::set_terminate (my_terminate);

  try
    {
      ex_test ();
    }
  catch (...)
    {
    }
  abort ();
}
