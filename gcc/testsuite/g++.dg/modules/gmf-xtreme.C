// PR c++/114630
// { dg-additional-options "-fmodules-ts -fdump-lang-module" }
// { dg-module-cmi empty }

module;
#include "xtreme-header.h"
export module empty;

// The whole GMF should be discarded here
// { dg-final { scan-lang-dump "Wrote 0 clusters" module } }
