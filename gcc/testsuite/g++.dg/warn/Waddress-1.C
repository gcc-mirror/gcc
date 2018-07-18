// PR c++/43906
// { dg-options "-Waddress -pedantic" }

extern void z();
typedef void (*ptrf) ();
typedef int (*ptrfn) (int);
int n;
const int m = 1;
struct S { };
struct T : S { };
struct U;
S s;
T t;
double d;

void f()  { if (z) z(); }               // { dg-warning "address" }

void gl() { if (z != 0) z(); }          // { dg-warning "address" }
void hl() { if (z != (ptrf)0) z(); }    // { dg-warning "address" }
void il() { if (z != (void*)0) z(); }   // { dg-warning "address|comparison" }
void jl() { if (&n != (int*)0) z(); }   // { dg-warning "address" }
void kl() { if (&m != (int*)0) z(); }   // { dg-warning "address" }
void ll() { if (&s != (T*)0) z(); }     // { dg-warning "address" }
void ml() { if (&t != (S*)0) z(); }     // { dg-warning "address" }

void nl() { if (z != (S*)0) z(); }      // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void pl() { if (z != (ptrfn)0) z(); }   // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void ql() { if (&d != (int*)0) z(); }   // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void rl() { if (&s != (U*)0) z(); }     // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }

void gr() { if (0 != z) z(); }          // { dg-warning "address" }
void hr() { if ((ptrf)0 != z) z(); }    // { dg-warning "address" }
void ir() { if ((void*)0 != z) z(); }   // { dg-warning "address|comparison" }
void jr() { if ((int*)0 != &n) z(); }   // { dg-warning "address" }
void kr() { if ((int*)0 != &m) z(); }   // { dg-warning "address" }
void lr() { if ((T*)0 != &s) z(); }     // { dg-warning "address" }
void mr() { if ((S*)0 != &t) z(); }     // { dg-warning "address" }

void nr() { if ((S*)0 != z) z(); }      // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void pr() { if ((ptrfn)0 != z) z(); }   // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void qr() { if ((int*)0 != &d) z(); }   // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
void rr() { if ((U*)0 != &s) z(); }     // { dg-error "comparison" }
// { dg-warning "address" "" { target *-*-* } .-1 }
