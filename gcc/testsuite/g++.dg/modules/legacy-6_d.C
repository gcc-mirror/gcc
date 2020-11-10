// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|@g++-mapper-server\\ -mt\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
int i;
#include "legacy-6_b.H"

// { dg-final { scan-file legacy-6_d.i {import  "[^\n]*legacy-6_a.H" \[\[__translated\]\];\nint i;} } }
// { dg-final { scan-file legacy-6_d.i {int i;\nimport  "[^\n]*legacy-6_b.H" \[\[__translated\]\];\n} } }
