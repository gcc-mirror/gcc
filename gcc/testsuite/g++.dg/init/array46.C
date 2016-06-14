int foo();
int a[] = foo();  // { dg-error "14:initializer fails to determine size" }
// { dg-error "14:array must be initialized" "" { target *-*-* } 2 }
