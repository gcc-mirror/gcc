// { dg-do preprocess }

#define NAME(X) X;

export module NAME(bob)

int i;
// { dg-final { scan-file cpp-3.i "\nexport module bob;\n" } }
// { dg-final { scan-file cpp-3.i "\nint i;\n" } }
