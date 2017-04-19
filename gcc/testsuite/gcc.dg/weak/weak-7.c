/* { dg-do compile } */
/* { dg-require-weak "" } */

extern void * foo (void);
void * foo (void) { return (void *)foo; }
/* { dg-error "function pointer" "pointer conversion" { target *-*-* } .-1 } */
extern void * foo (void) __attribute__((weak));
