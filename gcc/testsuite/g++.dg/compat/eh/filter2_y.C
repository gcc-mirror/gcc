#include <exception>
#include <cstdlib>

struct e1 {};
struct e2 {};

struct a
{
  a () { }

  ~a ()
#if __cplusplus >= 201103L
    // Give this destructor a potentially-throwing exception specification so
    // that we verify std::terminate gets called due to an exception during
    // unwinding, not just because the destructor is noexcept.
    noexcept(false)
#endif
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
