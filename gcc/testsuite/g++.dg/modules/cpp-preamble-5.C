// { dg-do preprocess }
// { dg-additional-options -fmodule-preamble }

#define NAME(X) X; // { dg-message "ends inside macro" }

export module NAME(bob)

int i;
// { dg-final { scan-file cpp-preamble-5.i "export module bob;\n" } }
// { dg-final { scan-file-not cpp-preamble-5.i "int i;" } }
