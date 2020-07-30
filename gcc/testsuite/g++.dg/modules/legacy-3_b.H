// { dg-do preprocess }
// { dg-additional-options -fmodule-header }

#define frob FROB

// Enough blank lines to force a line sync before the legacy import

// this should be diverted, if it isn't the above #define will break us
#include "legacy-3_a.H"
int move (int X = __LINE__); // Capture __LINE__ in a non-definition

// this should not be diverted
#include "legacy-3.h"

// { dg-final { scan-file legacy-3_b.i {\n# 9 "[^\n]*legacy-3_b.H"\nimport  "[^\n]*legacy-3_a.H" \[\[__translated\]\];\nint move \(int X = 10\);\n} } }

