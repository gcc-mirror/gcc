#include "filter1.h"

extern "C" void exit (int);
extern "C" void abort (void);
extern void ex_test (void);

void
filter1_x ()
{
  try
    {
      ex_test ();
    }
  catch (...)
    {
    }
  abort ();
}

a::a() { }
a::~a() { exit (0); }
