/* PR target/127773 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx2" } */
/* The global register variable is live across the call to f, so
   vzeroupper must not be issued before the call.  */
/* { dg-final { scan-assembler-not "vzeroupper.*call\[ \t\]+_?f\\M" } } */

typedef double v4df __attribute__ ((vector_size (32)));
register v4df gv asm ("xmm8"); /* { dg-warning "call-clobbered register used for global register variable" } */

static __attribute__ ((noinline)) double
f (double x)
{
  return x * 3.0;
}

double
g (v4df *p, double x)
{
  gv += *p;
  return f (x) + 1;
}
