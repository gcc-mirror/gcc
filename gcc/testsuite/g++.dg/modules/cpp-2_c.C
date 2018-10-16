// { dg-do preprocess }
// { dg-additional-options "-fmodules-atom" }
 
module bob;
#pragma GCC unused
import <stdio.h>;
import "string.h";
int i;
#ifndef NOPE
import nope;
#endif
think

// { dg-final { scan-file cpp-2_c.i "cpp-2_c.C\"\n\n\n\nmodule bob;\n#pragma GCC unused\nimport <stdio.h>;\nimport \"string.h\";\n" } }
// { dg-final { scan-file cpp-2_c.i "int i;" } }
// { dg-final { scan-file-not cpp-2_c.i "import nope;" } }
// { dg-final { scan-file cpp-2_c.i "THIS IS STDIO\n" } }
