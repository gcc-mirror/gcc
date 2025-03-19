/* { dg-compile } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -fchecking" }  */

typedef struct cl_lispunion *cl_object;
struct cl_lispunion {};
cl_object cl_error() __attribute__((noreturn));
volatile cl_object cl_coerce_value0;
void cl_coerce() { cl_error(); }
void L66safe_canonical_type(cl_object volatile);
