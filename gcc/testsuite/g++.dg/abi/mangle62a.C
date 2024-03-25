// Before v8, we mistakenly treated an unqualified function type
// as a substitution candidate for a function type with function-cv-quals.
// Test for that for backward compatibility.

// { dg-options "-fabi-version=7 -fabi-compat-version=7 -Wabi=0" }

template <class T, class U> struct A { };
// { dg-final { scan-assembler "\n_?_Z1fP1AIKFvvES0_E\[: \t\n\]" } }
void f (A<void()const, void()> *){} // { dg-warning "mangle" }
// { dg-final { scan-assembler "\n_?_Z1gP1AIFvvEKS0_E\[: \t\n\]" } }
void g (A<void(), void()const> *){} // { dg-warning "mangle" }
