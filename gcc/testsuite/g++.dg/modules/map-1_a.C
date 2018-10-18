// { dg-module-do run }
// { dg-additional-options "-fmodules-ts" }

// Ick!  no cross-host testing for you!
// { dg-additional-options -fmodule-mapper=$srcdir/g++.dg/modules/map-1.map }
// { dg-additional-files map-1.map }

export module frob;
// { dg-module-bmi "=map-1_a.nms" }

export int frob (int i)
{
  return -i;
}
