// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }
#define Q 0
#undef Q

import "cpp-5_a.H";

Q

// { dg-final { scan-file cpp-5_c.i {\n__import "[^\n]*cpp-5_a.H";(\n# 6 "[^\n]*cpp-5_c.C"\n)?\n\n0\n} } }
