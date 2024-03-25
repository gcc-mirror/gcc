/* Test qualifiers on function return types in C23 (C23 version of
   qual-return-5.c): those qualifiers are now ignored for all purposes,
   including _Atomic.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int f1 (void);
const int f1 (void);
volatile int f1 (void) { return 0; }

int *restrict f2 (void) { return 0; }
int *f2 (void);

const volatile long f3 (void);
long f3 (void);

const volatile void f4 (void) { }
void f4 (void);

_Atomic int f5 (void);
int f5 (void);

int f6 (void);
_Atomic int f6 (void) { return 0; }

/* The standard seems unclear regarding the case where restrict is
   applied to a function return type that may not be
   restrict-qualified; assume here that it is disallowed.  */
restrict int f7 (void); /* { dg-error "restrict" } */

typedef void FT (void);
FT *restrict f8 (void); /* { dg-error "restrict" } */
