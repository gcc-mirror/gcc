// Ick!
// { dg-additional-options -fmodule-mapper=$srcdir/g++.dg/modules/map-1.map }
// { dg-additional-files map-1.map }

import frob;

int main ()
{
  if (frob (-2) != 2)
    return 1;

  return 0;
}
