/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */

typedef int v4i32 __attribute__ ((vector_size(16), aligned(16)));
extern v4i32 a, b, c;

__attribute__ ((target ("no-lsx")))
void
test (void)
{
  a = __builtin_lsx_vadd_w (b, c); /* { dg-error "built-in function '__builtin_lsx_vadd_w' is not enabled" } */
}
