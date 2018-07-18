/* PR target/79593 */
/* { dg-do compile } */
/* { dg-options "-Ofast -mfpmath=387" } */

extern float global_data[1024];

static long double MIN (long double a, long double b) { return a < b ? a : b; }
static long double MAX (long double a, long double b) { return a > b ? a : b; }

float bar (void)
{
  long double delta = (global_data[0]);

  return (MIN (MAX (delta, 0.0l), 1.0l));
}

/* { dg-final { scan-assembler-not "fld\[ \t\]+%st" } } */
