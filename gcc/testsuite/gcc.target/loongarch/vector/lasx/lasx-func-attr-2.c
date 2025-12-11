/* { dg-do compile } */
/* { dg-options "-O2 -mlasx" } */

typedef int v8i32 __attribute__ ((vector_size(32), aligned(32)));
extern v8i32 a, b, c;

__attribute__ ((target ("no-lasx")))
void
test (void)
{
  a = __builtin_lasx_xvadd_w (b, c); /* { dg-error "built-in function '__builtin_lasx_xvadd_w' is not enabled" } */
}
