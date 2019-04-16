// { dg-do compile { target c++11 } }

int a;
void b(__attribute__((c([](int *) {} (a == (0 = auto))))));  // { dg-error "" }
