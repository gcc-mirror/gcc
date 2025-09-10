/* Test qualified function types: implementation-defined in C2y, undefined
   behavior previously.  GCC has an extension here, but does not allow it in
   pedantic mode.  _Atomic and restrict are constraint violations here.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors" } */

typedef void FUNC (void);

const FUNC f1; /* { dg-error "ISO C forbids qualified function types" } */
volatile FUNC f2; /* { dg-error "ISO C forbids qualified function types" } */
restrict FUNC f3; /* { dg-error "ISO C forbids qualified function types" } */
/* { dg-error "invalid use of 'restrict'" "restrict" { target *-*-* } .-1 } */
_Atomic FUNC f4; /* { dg-error "'_Atomic'-qualified function type" } */
