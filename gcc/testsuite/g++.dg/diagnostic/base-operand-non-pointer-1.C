class C { public: C f(); int get(); };
int f(C* p) { return p->f()->get(); }  // { dg-error "28:base operand of .->. has non-pointer type .C." }
