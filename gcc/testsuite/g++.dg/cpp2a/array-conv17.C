// PR c++/91364 - P0388R4: Permit conversions to arrays of unknown bound.
// { dg-do compile { target c++20 } }

// As conversion/qual1.C, but with [].

int *a[4];
const int *const(*ap1)[] = &a;
/* if at some level k the P2 is more cv-qualified than P1, then there
   must be a const at every single level (other than level zero) of P2
   up until k.  */
const int *(*ap2)[] = &a; // { dg-error "cannot convert" }
int *const(*ap3)[] = &a;
int *(*ap4)[] = &a;
int *(*const ap5)[] = &a;
const int *const(*const ap6)[] = &a;
int *const(*const ap7)[] = &a;
int *(*const ap8)[] = &a;

const int *b[4];
const int *const(*bp1)[] = &b;
const int *(*bp2)[] = &b;
int *const(*bp3)[] = &b; // { dg-error "cannot convert" }
int *(*bp4)[] = &b; // { dg-error "cannot convert" }
int *(*const bp5)[] = &b; // { dg-error "cannot convert" }
const int *const(*const bp6)[] = &b;
int *const(*const bp7)[] = &b; // { dg-error "cannot convert" }
int *(*const bp8)[] = &b; // { dg-error "cannot convert" }

int *c[2][3];
int const *const (*cp1)[] = c;
int const *(*cp2)[] = c; // { dg-error "cannot convert" }
int const *const (*const cp3)[] = c;
int *const (*cp4)[] = c;
int *(*cp5)[] = c;

double *const (*d)[3];
double const *const (*e)[] = d;
int *(*f)[3];
const int *const (*g)[] = f;
