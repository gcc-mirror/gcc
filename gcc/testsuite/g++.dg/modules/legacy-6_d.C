// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-6.map" }

#include "legacy-6_a.H"
int i;
#include "legacy-6_b.H"

// { dg-final { scan-file legacy-6_d.i {__import "[^\n]*legacy-6_a.H";\nint i;} } }

// { dg-final { scan-file legacy-6_d.i {int i;\n__import "[^\n]*legacy-6_b.H";\n} } }
