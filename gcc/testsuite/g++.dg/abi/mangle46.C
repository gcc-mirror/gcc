// PR c++/48008
// { dg-options "-fabi-version=5 -fabi-compat-version=5" }
// Test that we retain function-cv-quals in template argument mangling.

template <class T>
struct A
{ };

typedef void cfn(int) const;
typedef void fn(int);

// { dg-final { scan-assembler  "_Z1f1AIFviEE" } }
void f(A<fn>) { }
// { dg-final { scan-assembler  "_Z1f1AIKFviEE" } }
void f(A<cfn>) { }
