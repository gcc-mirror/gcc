// { dg-do compile }
// { dg-options "-std=c++98" }

// Make sure we issue a diagnostic if a type with no linkage is used
// to declare a a variable that has linkage.

struct { int i; } a; // { dg-warning "unnamed type" }

void foo() { a.i; }
