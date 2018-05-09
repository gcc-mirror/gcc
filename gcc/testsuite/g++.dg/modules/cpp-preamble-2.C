// { dg-do preprocess }
// { dg-additional-options -EE }

module bob;
import <stdio.h>;
import "string.h";
int i;
import nope;

// { dg-final { scan-file cpp-preamble-2.i "cpp-preamble-2.C\"\n\n\n\nmodule bob;\nimport <stdio.h>;\nimport \"string.h\";\n" } }
// { dg-final { scan-file-not cpp-preamble-2.i "int i;" } }
// { dg-final { scan-file-not cpp-preamble-2.i "import nope;" } }
