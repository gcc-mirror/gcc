// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw*} }
//  Test handling of MI thunks in dllimported classes.

// To build the dll and client app:
// g++ -shared -o MI.dll dllexport-MI1.C
// g++ -o MItest.exe dllimport-MI1.C  -L. MI.dll

#include <stdlib.h>
#include "dll-MI1.h"

extern DLL_IMPEXP MI1 dllMI1;

// This should use the implicit copy ctor for D1 (not imported)
// and the explicit copy ctor for D2 (dll-imported). 
MI1 dllMI1LocalCopy = dllMI1;

class  MI2 : public D1, public D2
{
public:
  int vf() const { return D2::vf();}
};

class  MI3 : public MI1
{
};

int main ()

{
  MI1 bar1;
  MI2 bar2; 
  MI3 bar3;

  if (dllMI1.vf() != D1_return)
    abort();

  if (dllMI1LocalCopy.vf() != D1_return)
    abort();

  if (bar1.vf() != D1_return)
    abort();

  if (bar2.vf() != (D2_return))
    abort();

  if (bar3.vf() != D1_return )
    abort();
}

// Scan for import of explicit copy ctor for D2, but no import
// of compiler generated copy ctor for D1. 
// { dg-final { scan-assembler  "__imp___ZN2D2C2ERKS_" } }
// { dg-final { scan-assembler-not "__imp___ZN2D1C2ERKS_" } }
