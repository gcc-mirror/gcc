// { dg-do preprocess }
// { dg-additional-options "-fmodule-legacy=legacy-3_c.H -fmodule-mapper=|cxx-mapper\\ -f\\ $srcdir/g++.dg/modules/legacy-3.map -fmodule-preamble" }

#define frob FROB

// this should be diverted, if it isn't the above #define will break us
#include  "legacy-3_a.H"
int move (int X = __LINE__); // Capture __LINE__ in a non-definition

// this should not be diverted
#include "legacy-3.h"

// { dg-final { scan-file legacy-3_c.i "\n import \"legacy-3_a.H\";\n" } }
// We should have stopped.
// { dg-final { scan-file-not legacy-3_c.i "move" } }
