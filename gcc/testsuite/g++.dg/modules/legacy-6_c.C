// { dg-do preprocess }
// { dg-additional-options "-fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-6.map" }

#include "legacy-6_a.H"
#include "legacy-6_b.H"
int i;

// { dg-final { scan-file legacy-6_c.i " *import \"legacy-6_a.H\";\n *import \"legacy-6_b.H\";\nint i;" } }
