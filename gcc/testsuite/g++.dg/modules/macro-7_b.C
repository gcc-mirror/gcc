// { dg-additional-options -fmodules-ts }
export module bar;
// { dg-module-cmi bar }

import foo;

export inline int One ()
{
  return Factory<1> ();
}
