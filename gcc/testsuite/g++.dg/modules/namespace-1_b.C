// { dg-additional-options "-fmodules-ts" }
export module Frink;
// { dg-module-cmi Frink }

import Frob;

export int frab (int x)
{
  return impl::doit (x) + ompl::doneit (x);
}
