// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }
#define Q 0
#undef Q

import "cpp-5_a.H";

Q

// { dg-final { scan-file cpp-5_c.i {\nimport "[^\n]*cpp-5_a.H";\n\n0\n} } }
