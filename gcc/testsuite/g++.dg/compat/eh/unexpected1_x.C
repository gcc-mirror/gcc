#include <exception>

struct One { };
struct Two { };

extern "C" void abort ();
extern void doit (void) throw (Two);
extern void handle_unexpected (void);

void
unexpected1_x ()
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
