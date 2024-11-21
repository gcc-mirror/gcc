/* Test errors for void parameters (constraint violation in C2Y, previously
   implicit undefined behavior; bug 114816).  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

typedef void Void;
typedef const void CVoid;
typedef volatile void VVoid;

/* Valid cases.  */
void f1 (void);
void f2 (Void);
void df1 (void) {}
void df2 (Void) {}

/* All other variants are invalid.  */

void g1 (const void); /* { dg-error "void" } */
void g2 (CVoid); /* { dg-error "void" } */
void g3 (volatile void); /* { dg-error "void" } */
void g4 (VVoid); /* { dg-error "void" } */
void g5 (register void); /* { dg-error "void" } */
void g6 (register Void); /* { dg-error "void" } */

void h1 (void x); /* { dg-error "void" } */
void h2 (Void x); /* { dg-error "void" } */
void h3 (const void x); /* { dg-error "void" } */
void h4 (CVoid x); /* { dg-error "void" } */
void h5 (volatile void x); /* { dg-error "void" } */
void h6 (VVoid x); /* { dg-error "void" } */
void h7 (register void x); /* { dg-error "void" } */
void h8 (register Void x); /* { dg-error "void" } */

void i1 (void, ...); /* { dg-error "void" } */
void i2 (Void, ...); /* { dg-error "void" } */
void i3 (const void, ...); /* { dg-error "void" } */
void i4 (CVoid, ...); /* { dg-error "void" } */
void i5 (volatile void, ...); /* { dg-error "void" } */
void i6 (VVoid, ...); /* { dg-error "void" } */
void i7 (register void, ...); /* { dg-error "void" } */
void i8 (register Void, ...); /* { dg-error "void" } */

void j1 (void x, ...); /* { dg-error "void" } */
void j2 (Void x, ...); /* { dg-error "void" } */
void j3 (const void x, ...); /* { dg-error "void" } */
void j4 (CVoid x, ...); /* { dg-error "void" } */
void j5 (volatile void x, ...); /* { dg-error "void" } */
void j6 (VVoid x, ...); /* { dg-error "void" } */
void j7 (register void x, ...); /* { dg-error "void" } */
void j8 (register Void x, ...); /* { dg-error "void" } */

void k1 (int i, void); /* { dg-error "void" } */
void k2 (int i, Void); /* { dg-error "void" } */
void k3 (int i, const void); /* { dg-error "void" } */
void k4 (int i, CVoid); /* { dg-error "void" } */
void k5 (int i, volatile void); /* { dg-error "void" } */
void k6 (int i, VVoid); /* { dg-error "void" } */
void k7 (int i, register void); /* { dg-error "void" } */
void k8 (int i, register Void); /* { dg-error "void" } */
void k9 (int i, void x); /* { dg-error "void" } */
void k10 (int i, Void x); /* { dg-error "void" } */
void k11 (int i, const void x); /* { dg-error "void" } */
void k12 (int i, CVoid x); /* { dg-error "void" } */
void k13 (int i, volatile void x); /* { dg-error "void" } */
void k14 (int i, VVoid x); /* { dg-error "void" } */
void k15 (int i, register void x); /* { dg-error "void" } */
void k16 (int i, register Void x); /* { dg-error "void" } */

void l1 (void, int i); /* { dg-error "void" } */
void l2 (Void, int i); /* { dg-error "void" } */
void l3 (const void, int i); /* { dg-error "void" } */
void l4 (CVoid, int i); /* { dg-error "void" } */
void l5 (volatile void, int i); /* { dg-error "void" } */
void l6 (VVoid, int i); /* { dg-error "void" } */
void l7 (register void, int i); /* { dg-error "void" } */
void l8 (register Void, int i); /* { dg-error "void" } */
void l9 (void x, int i); /* { dg-error "void" } */
void l10 (Void x, int i); /* { dg-error "void" } */
void l11 (const void x, int i); /* { dg-error "void" } */
void l12 (CVoid x, int i); /* { dg-error "void" } */
void l13 (volatile void x, int i); /* { dg-error "void" } */
void l14 (VVoid x, int i); /* { dg-error "void" } */
void l15 (register void x, int i); /* { dg-error "void" } */
void l16 (register Void x, int i); /* { dg-error "void" } */

void dg1 (const void) {} /* { dg-error "void|incomplete" } */
void dg2 (CVoid) {} /* { dg-error "void|incomplete" } */
void dg3 (volatile void) {} /* { dg-error "void|incomplete" } */
void dg4 (VVoid) {} /* { dg-error "void|incomplete" } */
void dg5 (register void) {} /* { dg-error "void|incomplete" } */
void dg6 (register Void) {} /* { dg-error "void|incomplete" } */

void dh1 (void x) {} /* { dg-error "void|incomplete" } */
void dh2 (Void x) {} /* { dg-error "void|incomplete" } */
void dh3 (const void x) {} /* { dg-error "void|incomplete" } */
void dh4 (CVoid x) {} /* { dg-error "void|incomplete" } */
void dh5 (volatile void x) {} /* { dg-error "void|incomplete" } */
void dh6 (VVoid x) {} /* { dg-error "void|incomplete" } */
void dh7 (register void x) {} /* { dg-error "void|incomplete" } */
void dh8 (register Void x) {} /* { dg-error "void|incomplete" } */

void di1 (void, ...) {} /* { dg-error "void|incomplete" } */
void di2 (Void, ...) {} /* { dg-error "void|incomplete" } */
void di3 (const void, ...) {} /* { dg-error "void|incomplete" } */
void di4 (CVoid, ...) {} /* { dg-error "void|incomplete" } */
void di5 (volatile void, ...) {} /* { dg-error "void|incomplete" } */
void di6 (VVoid, ...) {} /* { dg-error "void|incomplete" } */
void di7 (register void, ...) {} /* { dg-error "void|incomplete" } */
void di8 (register Void, ...) {} /* { dg-error "void|incomplete" } */

void dj1 (void x, ...) {} /* { dg-error "void|incomplete" } */
void dj2 (Void x, ...) {} /* { dg-error "void|incomplete" } */
void dj3 (const void x, ...) {} /* { dg-error "void|incomplete" } */
void dj4 (CVoid x, ...) {} /* { dg-error "void|incomplete" } */
void dj5 (volatile void x, ...) {} /* { dg-error "void|incomplete" } */
void dj6 (VVoid x, ...) {} /* { dg-error "void|incomplete" } */
void dj7 (register void x, ...) {} /* { dg-error "void|incomplete" } */
void dj8 (register Void x, ...) {} /* { dg-error "void|incomplete" } */

void dk1 (int i, void) {} /* { dg-error "void|incomplete" } */
void dk2 (int i, Void) {} /* { dg-error "void|incomplete" } */
void dk3 (int i, const void) {} /* { dg-error "void|incomplete" } */
void dk4 (int i, CVoid) {} /* { dg-error "void|incomplete" } */
void dk5 (int i, volatile void) {} /* { dg-error "void|incomplete" } */
void dk6 (int i, VVoid) {} /* { dg-error "void|incomplete" } */
void dk7 (int i, register void) {} /* { dg-error "void|incomplete" } */
void dk8 (int i, register Void) {} /* { dg-error "void|incomplete" } */
void dk9 (int i, void x) {} /* { dg-error "void|incomplete" } */
void dk10 (int i, Void x) {} /* { dg-error "void|incomplete" } */
void dk11 (int i, const void x) {} /* { dg-error "void|incomplete" } */
void dk12 (int i, CVoid x) {} /* { dg-error "void|incomplete" } */
void dk13 (int i, volatile void x) {} /* { dg-error "void|incomplete" } */
void dk14 (int i, VVoid x) {} /* { dg-error "void|incomplete" } */
void dk15 (int i, register void x) {} /* { dg-error "void|incomplete" } */
void dk16 (int i, register Void x) {} /* { dg-error "void|incomplete" } */

void dl1 (void, int i) {} /* { dg-error "void|incomplete" } */
void dl2 (Void, int i) {} /* { dg-error "void|incomplete" } */
void dl3 (const void, int i) {} /* { dg-error "void|incomplete" } */
void dl4 (CVoid, int i) {} /* { dg-error "void|incomplete" } */
void dl5 (volatile void, int i) {} /* { dg-error "void|incomplete" } */
void dl6 (VVoid, int i) {} /* { dg-error "void|incomplete" } */
void dl7 (register void, int i) {} /* { dg-error "void|incomplete" } */
void dl8 (register Void, int i) {} /* { dg-error "void|incomplete" } */
void dl9 (void x, int i) {} /* { dg-error "void|incomplete" } */
void dl10 (Void x, int i) {} /* { dg-error "void|incomplete" } */
void dl11 (const void x, int i) {} /* { dg-error "void|incomplete" } */
void dl12 (CVoid x, int i) {} /* { dg-error "void|incomplete" } */
void dl13 (volatile void x, int i) {} /* { dg-error "void|incomplete" } */
void dl14 (VVoid x, int i) {} /* { dg-error "void|incomplete" } */
void dl15 (register void x, int i) {} /* { dg-error "void|incomplete" } */
void dl16 (register Void x, int i) {} /* { dg-error "void|incomplete" } */
