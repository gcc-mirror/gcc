// { dg-additional-options {-fmodule-header -fno-module-lazy} }
// { dg-module-cmi {} }

#include "pr99283-5.h"

template<typename _Tp>
void Bar ()
{
  __traits<unsigned>::__min;
}

import  "pr99283-5_a.H";
