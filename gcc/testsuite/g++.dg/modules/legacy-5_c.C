// { dg-additional-options "-fdump-lang-module -fmodules-atom -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-5.map" }

// These map to the same module, which we should import only once.
#include "header.h"
#include <header.h>

// { dg-final { scan-lang-dump module {Starting module system:header.h} } }
// { dg-final { scan-lang-dump-not module {Starting module user:header.h} } }
