// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

import "used-1_a.H";

inline int wrapper (int x)
{
  return frob (x);
}
