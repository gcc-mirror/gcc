// { dg-additional-options -fmodules-ts }
export module evil;
// { dg-module-cmi !evil }

export
#include "exp-xlate-1_a.H" // { dg-error "not permitted" }

// { dg-prune-output {not writing module} }
