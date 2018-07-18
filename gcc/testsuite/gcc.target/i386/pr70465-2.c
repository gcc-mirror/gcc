/* PR target/70465 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mfpmath=387 -fomit-frame-pointer" } */
/* { dg-final { scan-assembler-not "fxch\t%st.1" { xfail *-*-* } } } */

extern float d[1024];

static inline long double
foo (long double a, long double b)
{
  return a < b ? a : b;
}

static inline long double
bar (long double a, long double b)
{
  return a > b ? a : b;
}

float
baz (void)
{
  long double c = d[0];
  return foo (bar (c, 0.0l), 1.0l);
}
