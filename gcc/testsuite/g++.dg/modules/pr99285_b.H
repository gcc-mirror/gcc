// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
import  "pr99285_a.H";

template<typename _Rep1, typename _Rep2,
	 typename _CRep = typename common_type<_Rep1, _Rep2>::type>
struct X;
