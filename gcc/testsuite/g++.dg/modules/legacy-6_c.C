// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|mapper-server\\ -f\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
#include "legacy-6_b.H"
int i;

// { dg-final { scan-file legacy-6_c.i {__import "[^\n]*legacy-6_a.H" \[\[__translated\]\];\n__import "[^\n]*legacy-6_b.H" \[\[__translated\]\];\nint i;} } }
