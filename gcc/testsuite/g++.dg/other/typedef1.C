// PR c++/27572
// { dg-do compile }

void f1(typedef) {}        // { dg-error "no type|typedef declaration" }
void f2(typedef x) {}      // { dg-error "no type|typedef declaration" }
void f3(typedef x[]) {}    // { dg-error "no type|typedef declaration" }
void f4(typedef int x) {}  // { dg-error "typedef declaration" }
