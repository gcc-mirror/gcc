// { dg-additional-options {-fmodule-header -fdump-lang-module-uid} }
// { dg-module-cmi {} }

#include "pmf-2.h"

// { dg-final { scan-lang-dump-times {Written:-[0-9]* ptrmem type} 1 module } }
