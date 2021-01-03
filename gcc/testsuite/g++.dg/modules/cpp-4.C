// { dg-do preprocess }

#if 1
#include "cpp-4.h"
#endif

// { dg-final { scan-file cpp-4.i "/cpp-4.h\\\" 1" } }
// { dg-final { scan-file cpp-4.i "/cpp-4.C\\\" 2" } }
// { dg-final { scan-file cpp-4.i "import x;\n" } }
// { dg-final { scan-file cpp-4.i "int" } }
