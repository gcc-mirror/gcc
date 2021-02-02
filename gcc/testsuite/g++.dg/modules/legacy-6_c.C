// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts -fmodule-mapper=|@g++-mapper-server\\ -mt\\ [srcdir]/legacy-6.map" }

#include "legacy-6_a.H"
#include "legacy-6_b.H"
int i;

// { dg-final { scan-file legacy-6_c.i {import  "[^\n]*legacy-6_a.H" \[\[__translated\]\];\nimport  "[^\n]*legacy-6_b.H" \[\[__translated\]\];\nint i;} } }
