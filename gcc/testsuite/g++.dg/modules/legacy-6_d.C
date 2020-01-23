// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
int i;
#include "legacy-6_b.H"

// { dg-final { scan-file legacy-6_d.i {__import "[^\n]*legacy-6_a.H";\n(# 4 "[^\n]*legacy-6_d.C"\n\n)?int i;} } }
// { dg-final { scan-file legacy-6_d.i {int i;\n__import "[^\n]*legacy-6_b.H";\n} } }
