// PR c++/13657

class C { public: int (*f())(); int bar(); };
int (*C::f())() { return C::bar; } // { dg-error "cannot convert 'C::bar'" }
