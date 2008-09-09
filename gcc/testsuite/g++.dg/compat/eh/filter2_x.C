#include <exception>
#include <cstdlib>

extern "C" void abort (void);

extern void my_terminate (void);
extern void ex_test (void);

void
filter2_x ()
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
