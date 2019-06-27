/* Test some error conditions with function specific options.  */
/* { dg-do compile } */

/* no fma400 switch */
extern void error1 (void) __attribute__((__target__("fma400"))); /* { dg-error "unknown" } */

/* Multiple arch switches */
extern void error2 (void) __attribute__((__target__("arch=core2,arch=k8"))); /* { dg-error "attribute value 'arch=k8' was already specified in 'target' attribute" } */

/* Unknown tune target */
extern void error3 (void) __attribute__((__target__("tune=foobar"))); /* { dg-error "bad value" } */

/* option on a variable */
extern int error4 __attribute__((__target__("sse2"))); /* { dg-warning "ignored" } */
