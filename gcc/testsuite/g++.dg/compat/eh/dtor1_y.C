extern int r;
int ad;

#include "dtor1.h"

A::~A () { ++ad; }

B::~B ()
try
  {
    throw 1;
  }
catch (...)
  {
    if (!ad)
      r = 1;
    return;
  }
