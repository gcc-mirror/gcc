/* Test qualifiers on function return types in C23 (C23 version of
   qual-return-6.c): those qualifiers are now ignored for all purposes,
   including _Atomic, but should still get warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -Wignored-qualifiers" } */

const int f1 (void); /* { dg-warning "qualifiers ignored" } */
volatile int f2 (void) { return 0; } /* { dg-warning "qualifiers ignored" } */
const volatile void f3 (void) { } /* { dg-warning "qualifiers ignored" } */
const void f4 (void); /* { dg-warning "qualifiers ignored" } */
_Atomic int f5 (void); /* { dg-warning "qualifiers ignored" } */
_Atomic int f6 (void) { return 0; } /* { dg-warning "qualifiers ignored" } */
