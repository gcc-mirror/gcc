// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

import  "pr99480_a.H";

template<typename _Tp>
struct atomic<_Tp*>
{
};
