// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

import  "pr99283-1_a.H";

template<typename _Facet>
_Facet &use_facet ();

extern template
char &use_facet<char> ();
