// { dg-do compile { target c++11 } }

int const foo1() -> double;  // { dg-error "1:.foo1. function with trailing return type" }
int volatile foo2() -> double;  // { dg-error "1:.foo2. function with trailing return type" }
int const volatile foo3() -> double;  // { dg-error "1:.foo3. function with trailing return type" }
