// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-6.map" }

#include "legacy-6_a.H"
#include "legacy-6_b.H"
int i;

// { dg-final { scan-file legacy-6_c.i "__import \"\[^\n]*legacy-6_a.H\";\n__import \"\[^\n]*legacy-6_b.H\";\nint i;" } }
