/* Test qualifiers on function return types after DR#423: those
   qualifiers are now ignored for all purposes (except that _Atomic
   still affects the type), but should still get warnings.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -Wignored-qualifiers" } */

const int f1 (void); /* { dg-warning "qualifiers ignored" } */
volatile int f2 (void) { return 0; } /* { dg-warning "qualifiers ignored" } */
const volatile void f3 (void) { } /* { dg-warning "qualifiers ignored" } */
const void f4 (void); /* { dg-warning "qualifiers ignored" } */
_Atomic int f5 (void); /* { dg-warning "qualifiers ignored" } */
_Atomic int f6 (void) { return 0; } /* { dg-warning "qualifiers ignored" } */
