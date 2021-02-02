// { dg-additional-options "-fmodules-ts" }
export module Frob;
// { dg-module-cmi Frob }

namespace impl
{
  export int doit (int);
}

namespace ompl
{
  export int doneit (int);
}
