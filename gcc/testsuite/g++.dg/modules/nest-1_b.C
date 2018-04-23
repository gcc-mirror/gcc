export module bar;
// { dg-module-bmi bar }

import foo;

namespace bar 
{
  export int frob (int i)
  {
    return i;
  }
}
