// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=[srcdir]/map-1.map" }

// Ick!  no cross-host testing for you!
// { dg-additional-files map-1.map }

export module frob;
// { dg-module-cmi "=map-1_a.nms" }

export int frob (int i)
{
  return -i;
}
