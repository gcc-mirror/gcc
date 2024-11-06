// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

module bob;
#pragma GCC unused
import "./cpp-2_b.H" [[ CLOSE ]];
import "cpp-2_a.H" [[ CLOSE;
int i;
#ifndef NOPE
import nope;
#endif
think

// { dg-final { scan-file cpp-2_c.i {cpp-2_c.C"\n\n\n\nmodule bob;\n#pragma GCC unused\nimport "[^\n]*\./cpp-2_b.H" \[\[ CLOSE ]];\nimport "[^\n]*cpp-2_a.H" \[\[ ]];\n} } }
// { dg-final { scan-file cpp-2_c.i "int i;" } }
// { dg-final { scan-file-not cpp-2_c.i "import *nope;" } }
// { dg-final { scan-file cpp-2_c.i "THIS IS STDIO\n" } }
