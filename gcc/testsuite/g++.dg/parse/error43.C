// PR c++/31423
// { dg-options "" }

class C { public: C* f(); int get(); };
int f(C* p) { return p->f->get(); }  // { dg-error "25:invalid use of member function" }
