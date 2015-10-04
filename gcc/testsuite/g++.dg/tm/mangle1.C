// Test for transaction_safe mangling.
// { dg-options -fgnu-tm }

// { dg-final { scan-assembler "_Z1fPDxFvvE" } }
void f(void (*)() transaction_safe) {}

// { dg-final { scan-assembler "_Z1fPDxFvvEPFvvE" } }
void f(void (*)() transaction_safe, void (*)()) {}

// { dg-final { scan-assembler "_Z1fPDxFvvES0_" } }
void f(void (*)() transaction_safe, void (*)() transaction_safe) {}

// { dg-final { scan-assembler "_Z1f1AIKDxFvvEE" } }
template <class T> struct A { };
void f(A<void () const transaction_safe>) { }

// { dg-final { scan-assembler "_Z1fM1AIiEKDxFvvE" } }
void f(void (A<int>::*)() const transaction_safe) { }
