/* Test __builtin_tgmath: valid uses, _FloatN types.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-require-effective-target float32_runtime } */

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

extern float var_f;
extern double var_d;
extern long double var_ld;
extern _Float32 var_f32;

float t1f (float x) { return x + 1; }
double t1d (double x) { return x + 2; }
long double t1l (long double x) { return x + 3; }
_Float32 t1f32 (_Float32 x) { return x + 4; }

#define t1v(x) __builtin_tgmath (t1f, t1d, t1l, t1f32, x)

static void
test_1 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  _Float32 f32 = 4;
  int i = 5;
  CHECK_CALL (t1v (f), 2, var_f);
  CHECK_CALL (t1v (d), 4, var_d);
  CHECK_CALL (t1v (ld), 6, var_ld);
  CHECK_CALL (t1v (f32), 8, var_f32);
  CHECK_CALL (t1v (i), 7, var_d);
}

int
main (void)
{
  test_1 ();
  exit (0);
}
