// { dg-do preprocess }
// { dg-additional-options "-fmodule-legacy -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-3.map" }

#define frob FROB

// this should be translated, if it isn't the above #define will break
// us
MARK1
#include  "legacy-3_a.H"
int move (int X = __LINE__); // Capture __LINE__ in a non-definition

// this should also be translated
MARK2
#include "legacy-3_a.H"

// this should not be translated
#include "legacy-3.h"

// { dg-final { scan-file legacy-3_c.i "MARK1\n import \"legacy-3_a.H\";\n" } }
// We should have stopped.
// { dg-final { scan-file legacy-3_c.i "move" } }
// { dg-final { scan-file legacy-3_c.i "MARK2\n import \"legacy-3_a.H\";\n" } }
// { dg-final { scan-file-not legacy-3_c.i "# \[^\n]*legacy-3_a.H" } }
// { dg-final { scan-file legacy-3_c.i "# \[^\n]*legacy-3.h" } }
