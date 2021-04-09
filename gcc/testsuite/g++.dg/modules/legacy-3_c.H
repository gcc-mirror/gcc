// { dg-do preprocess }
// { dg-additional-options -fmodule-header }

#define frob FROB

// this should be translated, if it isn't the above #define will break
// us
MARK1 __LINE__
#include  "legacy-3_a.H"
MARK2 __LINE__
int move (int X = __LINE__); // Capture __LINE__ in a non-definition
// this should also be translated, but elided too
MARK2 __LINE__
#include "legacy-3_a.H"
MARK3 __LINE__

// this should not be translated
#include "legacy-3.h"

// { dg-final { scan-file legacy-3_c.i {MARK1 8\nimport  "[^\n]*legacy-3_a.H" \[\[__translated\]\];\nMARK2 10\n} } }
// We should have stopped.
// { dg-final { scan-file legacy-3_c.i {move} } }
// { dg-final { scan-file legacy-3_c.i {MARK2 13\n\nMARK3 15\n} } }
// { dg-final { scan-file-not legacy-3_c.i {# [^\n]*legacy-3_a.H} } }
// { dg-final { scan-file legacy-3_c.i {# [^\n]*legacy-3.h} } }
