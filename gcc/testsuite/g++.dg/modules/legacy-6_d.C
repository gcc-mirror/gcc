// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|mapper-server\\ -f\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
int i;
#include "legacy-6_b.H"

// { dg-final { scan-file legacy-6_d.i {__import "[^\n]*legacy-6_a.H" \[\[__translated\]\];\nint i;} } }
// { dg-final { scan-file legacy-6_d.i {int i;\n__import "[^\n]*legacy-6_b.H" \[\[__translated\]\];\n} } }
