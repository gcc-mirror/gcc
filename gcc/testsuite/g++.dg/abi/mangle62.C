// Before v8, we mistakenly treated an unqualified function type
// as a substitution candidate for a function type with function-cv-quals.
// Test for the conformant behavior.

// { dg-options "-fabi-version=0 -fabi-compat-version=0 -Wabi=7" }

template <class T, class U> struct A { };
// { dg-final { scan-assembler "\n_?_Z1fP1AIKFvvEFvvEE\[: \t\n\]" } }
void f (A<void()const, void()> *){} // { dg-warning "mangled name" }
// { dg-final { scan-assembler "\n_?_Z1gP1AIFvvEKFvvEE\[: \t\n\]" } }
void g (A<void(), void()const> *){} // { dg-warning "mangled name" }
