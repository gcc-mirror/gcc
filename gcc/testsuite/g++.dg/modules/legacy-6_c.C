// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
#include "legacy-6_b.H"
int i;

// { dg-final { scan-file legacy-6_c.i {__import "[^\n]*legacy-6_a.H";(\n# 4 "[^\n]*legacy-6_c.C"\n)?\n__import "[^\n]*legacy-6_b.H";(\n# 5 "[^\n]*legacy-6_c.C"\n)?\nint i;} } }
