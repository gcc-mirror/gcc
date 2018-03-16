// { dg-additional-options "-fmodules-ts" }
export module Frink;
// { dg-module-bmi Frink }

import Frob;

export int frab (int x)
{
  return impl::doit (x) + ompl::doneit (x);
}
