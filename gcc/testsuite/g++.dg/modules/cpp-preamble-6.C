// { dg-do preprocess }
// { dg-additional-options -fmodule-preamble }

import X;
// We're in a preamble.
#if 1
import X; // Still in preamble
#else
int i;  // ignored.
#endif
int y; // Stop

// { dg-final { scan-file cpp-preamble-6.i "import X;\n\n\nimport X;\n" } }
// { dg-final { scan-file-not cpp-preamble-6.i "int" } }
