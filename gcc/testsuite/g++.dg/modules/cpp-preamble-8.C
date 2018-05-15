// { dg-do preprocess }
// { dg-additional-options -EE }

#if 1
#include "cpp-preamble-8.h"
#endif

// { dg-final { scan-file cpp-preamble-8.i "/cpp-preamble-8.h\\\" 1" } }
// { dg-final { scan-file cpp-preamble-8.i "/cpp-preamble-8.C\\\" 2" } }
// { dg-final { scan-file cpp-preamble-8.i "import x;\n" } }
// { dg-final { scan-file-not cpp-preamble-8.i "int" } }
