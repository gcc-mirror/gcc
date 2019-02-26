// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-5.map" }

// These map to the same module, which we should import only once.
#include "header.h"
#include <header.h>

// { dg-final { scan-file legacy-5_b.i {__import <header.h>;\n__import <header.h>;\n} } }
