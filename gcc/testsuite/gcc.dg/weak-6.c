/* { dg-do compile } */

extern void * foo (void);
void * foo (void) { return (void *)foo; } /* { dg-error "precede" "" { xfail *-*-coff i?86-pc-cygwin h8300-*-hms } } */

#pragma weak foo
