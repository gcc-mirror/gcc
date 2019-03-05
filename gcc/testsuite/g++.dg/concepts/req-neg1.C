// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

void f1(int a) requires true;         // OK
auto f2(int a) -> bool requires true; // OK
auto f3(int a) requires true -> bool; // { dg-error "" } requires-clause precedes trailing-return-type
typedef void fn_t() requires true;    // { dg-error "typedef" }
void (*pf)() requires true;           // { dg-error "non-function" }
void (*fn(int))() requires false;     // { dg-error "return type" }
void g(int (*)() requires true);      // { dg-error "parameter|non-function" }
auto* p = new (void(*)(char) requires true); // { dg-error "type-id" }
