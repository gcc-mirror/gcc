/* Test __builtin_tgmath: integer arguments with _FloatN / _FloatNx.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-add-options float64 } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32_runtime } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-require-effective-target float32x_runtime } */

extern void abort (void);
extern void exit (int);

#define CHECK_CALL(C, E, V)			\
  do						\
    {						\
      if ((C) != (E))				\
	abort ();				\
      extern __typeof (C) V;			\
    }						\
  while (0)

extern double var_d;
extern _Float32 var_f32;
extern _Float64 var_f64;
extern _Float32x var_f32x;
extern _Complex _Float32x var_cf32x;

_Float32 t1f (float x) { return x + 1; }
_Float32 t1d (double x) { return x + 2; }
_Float32 t1l (long double x) { return x + 3; }
_Float32 t1f64 (_Float64 x) { return x + 4; }

#define t1v(x) __builtin_tgmath (t1f, t1d, t1l, t1f64, x)

static void
test_1 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  _Float64 f64 = 4;
  int i = 5;
  CHECK_CALL (t1v (f), 2, var_f32);
  CHECK_CALL (t1v (d), 4, var_f32);
  CHECK_CALL (t1v (ld), 6, var_f32);
  CHECK_CALL (t1v (f64), 8, var_f32);
  CHECK_CALL (t1v (i), 7, var_f32);
}

float t2f (float x, float y) { return 10 * x + y; }
double t2d (double x, double y) { return 100 * x + y; }
long double t2l (long double x, long double y) { return 1000 * x + y; }
_Float32x t2f32x (_Float32x x, _Float32x y) { return 10000 * x + y; }
_Float64 t2f64 (_Float64 x, _Float64 y) { return 100000 * x + y; }

_Complex _Float32x
ct2f32x (_Complex _Float32x x, _Complex _Float32x y)
{
  return 1000000 * x + y;
}

#define t2v(x, y) __builtin_tgmath (t2f, t2d, t2l, t2f32x, t2f64, ct2f32x, x, y)

static void
test_2 (void)
{
  double d = 3;
  _Float64 f64 = 6;
  _Float32x f32x = 7;
  int i = 5;
  _Complex _Float32x cf32x = 8;
  CHECK_CALL (t2v (f32x, i), 70005, var_f32x);
  CHECK_CALL (t2v (i, f32x), 50007, var_f32x);
  CHECK_CALL (t2v (f64, i), 600005, var_f64);
  CHECK_CALL (t2v (i, f64), 500006, var_f64);
  CHECK_CALL (t2v (d, i), 305, var_d);
  CHECK_CALL (t2v (i, d), 503, var_d);
  CHECK_CALL (t2v (cf32x, i), 8000005, var_cf32x);
  CHECK_CALL (t2v (i, cf32x), 5000008, var_cf32x);
}

int
main (void)
{
  test_1 ();
  test_2 ();
  exit (0);
}
