/* { dg-do compile } */
/* { dg-require-weak "" } */

extern void * foo (void);
void * foo (void) { return (void *)foo; } /* { dg-error "precede" } */
/* { dg-error "function pointer" "pointer conversion" { target *-*-* } 5 } */
extern void * foo (void) __attribute__((weak));
