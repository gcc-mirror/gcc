/* PR middle-end/122307: a quiet != (and ==) must not raise FE_INVALID
   for a quiet NaN operand, in any expansion form.  */
/* { dg-do run } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-additional-options "-ftrapping-math" } */
/* { dg-add-options ieee } */

#include <fenv.h>

#define NI __attribute__ ((noipa))

#define GEN(T, S) \
  NI int ne_##S (T a, T b) { return a != b; } \
  NI int eq_##S (T a, T b) { return a == b; } \
  NI int ne_sel_##S (T a, T b, int x, int y) { return a != b ? x : y; } \
  NI int eq_sel_##S (T a, T b, int x, int y) { return a == b ? x : y; } \
  NI int lg_##S (T a, T b) { return __builtin_islessgreater (a, b); } \
  NI int ueq_##S (T a, T b) { return !__builtin_islessgreater (a, b); }

GEN (float, f)
GEN (double, d)

#define CHECK(E, V) \
  do { \
    feclearexcept (FE_INVALID); \
    if ((E) != (V) || fetestexcept (FE_INVALID)) \
      __builtin_abort (); \
  } while (0)

int
main (void)
{
  volatile float nf = __builtin_nanf (""), of = 1.0f;
  volatile double nd = __builtin_nan (""), od = 1.0;

  CHECK (ne_f (nf, of), 1);
  CHECK (ne_f (of, nf), 1);
  CHECK (eq_f (nf, of), 0);
  CHECK (ne_sel_f (nf, of, 5, 7), 5);
  CHECK (eq_sel_f (nf, of, 5, 7), 7);
  CHECK (lg_f (nf, of), 0);
  CHECK (ueq_f (nf, of), 1);

  CHECK (ne_d (nd, od), 1);
  CHECK (ne_d (od, nd), 1);
  CHECK (eq_d (nd, od), 0);
  CHECK (ne_sel_d (nd, od, 5, 7), 5);
  CHECK (eq_sel_d (nd, od, 5, 7), 7);
  CHECK (lg_d (nd, od), 0);
  CHECK (ueq_d (nd, od), 1);

  /* Ordered operands still compare correctly.  */
  CHECK (ne_d (od, 2.0), 1);
  CHECK (ne_d (od, od), 0);
  return 0;
}
