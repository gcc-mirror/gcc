// Testcase from P0012r1
// { dg-do compile { target c++17 } }

void (*p)() throw(int);	       // { dg-error "dynamic exception specification" }
void (**pp)() noexcept = &p;   // { dg-error "" } cannot convert to pointer to noexcept function

struct S { typedef void (*p)(); operator p(); };
void (*q)() noexcept = S();   // { dg-error "" } cannot convert to pointer to noexcept function
