// { dg-do compile { target c++11 } }

int a;
void b(__attribute__((c([](int *) {} (a == (0 = auto))))));  // { dg-error "6:variable or field .b. declared void" }
// { dg-error "expected" "" { target c++11 } .-1 }
