/* Invalid tls_model attributes.  PR 35435.  */
/* { dg-require-effective-target tls } */

int v __attribute__((tls_model("initial-exec"))); /* { dg-warning "attribute ignored" } */
typedef int X __attribute__((tls_model("initial-exec"))); /* { dg-warning "attribute ignored" } */
void f(int x __attribute__((tls_model("initial-exec")))); /* { dg-warning "attribute ignored" } */
__thread int a __attribute__((tls_model(1))); /* { dg-error ".tls_model. argument not a string" } */
__thread int b __attribute__((tls_model("unknown"))); /* { dg-error ".tls_model. argument must be one of" } */
