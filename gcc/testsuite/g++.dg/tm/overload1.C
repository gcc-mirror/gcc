// Function declarations that differ only in the presence or absence of a
// tx-qualifier cannot be overloaded.
// { dg-options "-fgnu-tm" }

void f();			// { dg-message "" }
void f() transaction_safe;	// { dg-error "" }
