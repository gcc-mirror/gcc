/* { dg-do compile } */
/* { dg-require-weak "" } */

extern void * foo (void);
void * foo (void) { return (void *)foo; } /* { dg-error "precede" } */

#pragma weak foo
