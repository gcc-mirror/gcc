// Test for pretty-printing in diagnostics.
// { dg-options "-fgnu-tm" }

void f();
void (*p)() transaction_safe = f; // { dg-error "void \\(\\*\\)\\(\\) transaction_safe" }

