/* { dg-do compile } */

extern void * foo (void);
void * foo (void) { return (void *)foo; } /* { dg-error "precede" } */

extern void * foo (void) __attribute__((weak));
