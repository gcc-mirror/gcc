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
