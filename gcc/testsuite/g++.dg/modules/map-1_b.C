// Ick!
// { dg-additional-options "-fmodules-ts -fmodule-mapper=[srcdir]/map-1_b.map?MAP" }
// { dg-additional-files map-1.map }

import frob;

int main ()
{
  if (frob (-2) != 2)
    return 1;

  return 0;
}
