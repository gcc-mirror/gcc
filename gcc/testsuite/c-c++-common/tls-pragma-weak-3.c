/* PR c/125418 */
/* { dg-do compile { target tls_native } } */
/* { dg-require-weak "" } */
/* { dg-require-visibility "" } */

/* A tls_model attribute whose argument is not a string is rejected and
   must not be left on the declaration for #pragma weak to read.  */

__attribute__((visibility("hidden"), tls_model(1)))
extern __thread int t1; /* { dg-error "argument not a string" } */
#pragma weak t1

#pragma weak t2
__attribute__((visibility("hidden"), tls_model(1)))
extern __thread int t2; /* { dg-error "argument not a string" } */

int *f1 (void) { return &t1; }
int *f2 (void) { return &t2; }
