extern "C" void exit (int);
extern "C" void abort (void);

#include "ctor2.h"

int r;

void ctor2_x () {

  try
    { 
      DerivedStream str;
    }
  catch (...) { }

  if (r != 0)
    abort ();
  exit (0);
}
