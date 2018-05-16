// { dg-do preprocess }
// { dg-additional-options -fmodule-preamble }

import x
int i;
int j;

// { dg-final { scan-file cpp-preamble-3.i "import x\nint i;" } }
// { dg-final { scan-file-not cpp-preamble-3.i "int j;" } }
