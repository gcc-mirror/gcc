/* { dg-do compile } */
/* { dg-excess-errors "COFF does not support weak symbols" { target *-*-coff i?86-pc-cygwin h8300-*-hms } } */
/* { dg-excess-errors "Darwin does not support weak symbols" { target *-*-darwin* } } */

extern void * foo (void);
void * foo (void) { return (void *)foo; } /* { dg-error "precede" } */

extern void * foo (void) __attribute__((weak));
