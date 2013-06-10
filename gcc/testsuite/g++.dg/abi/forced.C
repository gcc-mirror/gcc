// { dg-do run { target *-*-linux* *-*-gnu* } }
// { dg-options "-pthread" }

#include <pthread.h>
#include <cxxabi.h>
extern "C" int printf (const char *, ...);

int main()
{
  try
    {
      pthread_exit (0);
    }
  catch (abi::__forced_unwind &)
    {
      printf ("caught forced unwind\n");
      throw;
    }
  catch (...)
    {
      printf ("caught ...\n");
      return 1;
    }
}
