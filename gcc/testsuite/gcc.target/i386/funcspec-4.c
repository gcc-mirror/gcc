/* Test some error conditions with function specific options.  */
/* { dg-do compile } */

/* Multiple arch switches */
extern void error2 (void) __attribute__((__target__("arch=core2,arch=k8"))); /* { dg-error "already specified" } */

/* Unknown tune target */
extern void error3 (void) __attribute__((__target__("tune=foobar"))); /* { dg-error "bad value" } */

/* option on a variable */
extern int error4 __attribute__((__target__("sse2"))); /* { dg-warning "ignored" } */
