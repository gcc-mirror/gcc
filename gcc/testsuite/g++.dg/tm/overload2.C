// 13.4p1: A function with type F is selected for the function type FT of the
// target type required in the context if F (after possibly applying the
// transaction-safety conversion (4.14 [conv.tx])) is identical to FT.
// { dg-options "-fgnu-tm" }

void f() transaction_safe;
void f(int);

void (*p)() = f;
