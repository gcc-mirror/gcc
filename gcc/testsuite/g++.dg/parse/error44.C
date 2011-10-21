// PR c++/31423
// { dg-options "-fms-extensions" }

struct C {
   int f() { return 1; }
   int g() { return 2; }
};

int f(C& c) {
   return c.g == &c.f; // { dg-error "forget the '&'" }
}
