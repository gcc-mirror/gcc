// PR c++/27572
// { dg-do compile }

void f1(typedef) {}        // { dg-error "9:typedef declaration" }
// { dg-error "no type" "" { target *-*-* } .-1 }
void f2(typedef x) {}      // { dg-error "9:typedef declaration" }
// { dg-error "type" "" { target *-*-* } .-1 }
void f3(typedef x[]) {}    // { dg-error "9:typedef declaration" }
// { dg-error "type" "" { target *-*-* } .-1 }
void f4(typedef int x) {}  // { dg-error "9:typedef declaration" }
