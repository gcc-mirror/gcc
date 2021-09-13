// PR 99283 part 1 ICE on specialization
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _Facet>
_Facet &use_facet ();
