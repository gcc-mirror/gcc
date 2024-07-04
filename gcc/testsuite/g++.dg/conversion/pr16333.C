// PR c++/16333

struct X {
   X (const int (&)[3]);
};

int a[3];
X foo1 () { return a; }
const X &foo2 () { return a; } // { dg-message "returning reference to temporary" }
X &foo3 () { return a; } // { dg-error "cannot bind non-const lvalue ref" }
