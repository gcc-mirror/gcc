// { dg-do preprocess }
// { dg-additional-options -EE }

#pragma bob
import x;
int i;

// { dg-final { scan-file cpp-preamble-4.i "#pragma bob\nimport x;\n" } }
// { dg-final { scan-file-not cpp-preamble-4.i "int i;" } }
