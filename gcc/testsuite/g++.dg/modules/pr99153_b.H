// PR 99153 Mismatched flags on template and result
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

import  "pr99153_a.H";

template<class _T1>
inline  void pair<_T1>::Frob()
{ }


template<typename _T2>
inline void Widget () 
{
}
