// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
// We end up with instantiated noexcept specs in the CMI data matching
// textually loaded fns with uninstantiated ones.  Have to propagate,
// not reinstantiate.
#include "except-3.h"
