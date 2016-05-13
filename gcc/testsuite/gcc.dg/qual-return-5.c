/* Test qualifiers on function return types after DR#423: those
   qualifiers are now ignored for all purposes (but _Atomic is not,
   for this purpose, a qualifier).  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

int f1 (void);
const int f1 (void);
volatile int f1 (void) { return 0; }

int *restrict f2 (void) { return 0; }
int *f2 (void);

const volatile long f3 (void);
long f3 (void);

const volatile void f4 (void) { }
void f4 (void);

_Atomic int f5 (void); /* { dg-message "previous declaration" } */
int f5 (void); /* { dg-error "conflicting" } */

int f6 (void); /* { dg-message "previous declaration" } */
_Atomic int f6 (void) { return 0; } /* { dg-error "conflicting" } */

/* The standard seems unclear regarding the case where restrict is
   applied to a function return type that may not be
   restrict-qualified; assume here that it is disallowed.  */
restrict int f7 (void); /* { dg-error "restrict" } */

typedef void FT (void);
FT *restrict f8 (void); /* { dg-error "restrict" } */
