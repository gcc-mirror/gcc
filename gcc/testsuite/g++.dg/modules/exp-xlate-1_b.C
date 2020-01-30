// { dg-additional-options -fmodules-ts }
export module evil;
// { dg-module-cmi !evil }

// FIXME: xfailing until __import and co are actual keywords.
export
#include "exp-xlate-1_a.H" // { dg-error "not permitted" "" { xfail *-*-* } }
// { dg-bogus "-:before end of line" "" { xfail *-*-* } }
// { dg-bogus "before .__import" "" { xfail *-*-* } .-2 }

// { dg-prune-output {not writing module} }
