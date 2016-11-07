// Testcase from P0012r1
// { dg-options -std=c++1z }

void (*p)() throw(int);
void (**pp)() noexcept = &p;   // { dg-error "" } cannot convert to pointer to noexcept function

struct S { typedef void (*p)(); operator p(); };
void (*q)() noexcept = S();   // { dg-error "" } cannot convert to pointer to noexcept function
