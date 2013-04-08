// Before v8, we mistakenly treated an unqualified function type
// as a substitution candidate for a function type with function-cv-quals.
// Test for that for backward compatibility.

// { dg-options -fabi-version=7 }

template <class T, class U> struct A { };
// { dg-final { scan-assembler "_Z1fP1AIKFvvES0_E" } }
void f (A<void()const, void()> *){}
// { dg-final { scan-assembler "_Z1gP1AIFvvEKS0_E" } }
void g (A<void(), void()const> *){}
