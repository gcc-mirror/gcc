// { dg-additional-options -fmodules-ts }
export module evil;
// { dg-module-cmi !evil }

export // { dg-error "not part of following" }
#include "exp-xlate-1_a.H" // { dg-error "must be contiguous" }
