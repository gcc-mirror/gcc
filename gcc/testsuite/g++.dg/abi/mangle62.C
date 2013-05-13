// Before v8, we mistakenly treated an unqualified function type
// as a substitution candidate for a function type with function-cv-quals.
// Test for the conformant behavior.

// { dg-options -fabi-version=0 }

template <class T, class U> struct A { };
// { dg-final { scan-assembler "_Z1fP1AIKFvvEFvvEE" } }
void f (A<void()const, void()> *){}
// { dg-final { scan-assembler "_Z1gP1AIFvvEKFvvEE" } }
void g (A<void(), void()const> *){}
