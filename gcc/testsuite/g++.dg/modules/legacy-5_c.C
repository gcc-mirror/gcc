// { dg-additional-options "-fdump-lang-module -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-5.map" }

// These map to the same module, which we should import only once.
#include "header.h"
#include <header.h>

// { dg-final { scan-lang-dump {Starting module <header.h>} module } }
// { dg-final { scan-lang-dump-not {Starting module "header.h"} module } }
