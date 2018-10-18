// { dg-do preprocess }
// { dg-additional-options "-fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-5.map" }

// These map to the same module, which we should import only once.
#include "header.h"
#include <header.h>

// { dg-final { scan-file legacy-5_b.i {import <header.h>;\n *import <header.h>;\n} } }
