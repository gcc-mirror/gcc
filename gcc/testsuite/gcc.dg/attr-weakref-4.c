/* { dg-do compile } */
/* { dg-require-weak "" } */
static void __attribute__((weakref("bar"))) foo(void) { } /* { dg-warning "attribute ignored because function is defined" } */
static int __attribute__((weakref)) a=0; /* { dg-warning "attribute ignored because variable is initialized" } */
