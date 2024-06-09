// Test mangling of pointers to cv-qualified member functions
// { dg-additional-options -fabi-compat-version=0 }

struct A;
void f (void (A::*)() const) {}

// { dg-final { scan-assembler "\n_?_Z1fM1AKFvvE\[: \t\n\]" } }
