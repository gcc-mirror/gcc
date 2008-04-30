/* Verify that we get errors for trying to put TLS data in 
   sections which can't work.  */
/* { dg-require-effective-target tls } */
/* { dg-do compile { target *-*-vxworks } } */

#define A(X)	__attribute__((section(X)))

__thread int i A("foo"); /* { dg-error "cannot be overridden" } */
