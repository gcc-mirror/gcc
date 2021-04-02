// { dg-additional-options "-fmodules-ts" }
export module bar;
// { dg-module-cmi bar }

import foo;

namespace bar 
{
  export int frob (int i)
  {
    return i;
  }
}
