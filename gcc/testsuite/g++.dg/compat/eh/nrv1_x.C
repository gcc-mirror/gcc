extern "C" void exit (int);
extern "C" void abort (void);

#include "nrv1.h"

extern A f (void);

int c, d;

void nrv1_x ()
{
  try
    { A a = f(); }
  catch (...) { }
  if (d < c)
    abort ();
  exit (0);
}

A::A() { ++c; }
A::~A() { ++d; }
