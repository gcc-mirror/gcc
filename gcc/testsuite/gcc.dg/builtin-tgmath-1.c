/* Test __builtin_tgmath: valid uses, standard floating-point types.  */
/* { dg-do run } */
/* { dg-options "" } */

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
extern _Complex float var_cf;
extern _Complex double var_cd;
extern _Complex long double var_cld;
extern int var_i;

typedef float float_type;
typedef double double_type;

/* Test simple case, real arguments and return type.  */

float_type t1f (float x) { return x + 1; }
double t1d (double_type x) { return x + 2; }
long double t1l (volatile long double x) { return x + 3; }

#define t1v(x) __builtin_tgmath (t1f, t1d, t1l, x)
#define t1vr(x) __builtin_tgmath (t1l, t1d, t1f, x)

static void
test_1 (void)
{
  float_type f = 1;
  volatile float vf = 2;
  double d = 3;
  long double ld = 4;
  int i = 5;
  long long ll = 6;
  CHECK_CALL (t1v (f), 2, var_f);
  CHECK_CALL (t1v (vf), 3, var_f);
  CHECK_CALL (t1v (d), 5, var_d);
  CHECK_CALL (t1v (ld), 7, var_ld);
  CHECK_CALL (t1v (i), 7, var_d);
  CHECK_CALL (t1v (ll), 8, var_d);
  CHECK_CALL (t1vr (f), 2, var_f);
  CHECK_CALL (t1vr (vf), 3, var_f);
  CHECK_CALL (t1vr (d), 5, var_d);
  CHECK_CALL (t1vr (ld), 7, var_ld);
  CHECK_CALL (t1vr (i), 7, var_d);
  CHECK_CALL (t1vr (ll), 8, var_d);
}

/* Test first argument not type-generic.  */

float t2f (int a, float x) { return a * x + 1; }
double t2d (int a, double x) { return a * x + 2; }
long double t2l (int a, long double x) { return a * x + 3; }

#define t2v(a, x) __builtin_tgmath (t2f, t2d, t2l, a, x)

static void
test_2 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  int i = 4;
  unsigned long long ll = 5;
  CHECK_CALL (t2v (1, f), 2, var_f);
  CHECK_CALL (t2v (2, d), 6, var_d);
  CHECK_CALL (t2v (3, ld), 12, var_ld);
  CHECK_CALL (t2v (4, i), 18, var_d);
  CHECK_CALL (t2v (5, ll), 27, var_d);
}

/* Test return type not type-generic.  */

int t3f (float x) { return x + 1; }
int t3d (double x) { return x + 2; }
int t3l (long double x) { return x + 3; }

#define t3v(x) __builtin_tgmath (t3f, t3d, t3l, x)

static void
test_3 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  short s = 4;
  CHECK_CALL (t3v (f), 2, var_i);
  CHECK_CALL (t3v (d), 4, var_i);
  CHECK_CALL (t3v (ld), 6, var_i);
  CHECK_CALL (t3v (s), 6, var_i);
}

/* Test multiple type-generic arguments.  */

float t4f (float x, float y) { return 10 * x + y; }
double t4d (double x, double y) { return 100 * x + y; }
long double t4l (long double x, long double y) { return 1000 * x + y; }

#define t4v(x, y) __builtin_tgmath (t4f, t4d, t4l, x, y)

static void
test_4 (void)
{
  float f1 = 1;
  float f2 = 2;
  double d1 = 3;
  double d2 = 4;
  long double ld = 5;
  long int l = 6;
  CHECK_CALL (t4v (f1, f2), 12, var_f);
  CHECK_CALL (t4v (f2, f1), 21, var_f);
  CHECK_CALL (t4v (f1, d1), 103, var_d);
  CHECK_CALL (t4v (d2, f2), 402, var_d);
  CHECK_CALL (t4v (f1, l), 106, var_d);
  CHECK_CALL (t4v (ld, f1), 5001, var_ld);
  CHECK_CALL (t4v (l, l), 606, var_d);
  CHECK_CALL (t4v (l, ld), 6005, var_ld);
}

/* Test complex argument, real return type.  */

float t5f (_Complex float x) { return 1 + __real__ x + 3 * __imag__ x; }
double t5d (_Complex double x) { return 2 + __real__ x + 4 * __imag__ x; }
long double t5l (_Complex long double x) { return 3 + __real__ x + 5 * __imag__ x; }

#define t5v(x) __builtin_tgmath (t5f, t5d, t5l, x)

static void
test_5 (void)
{
  float f = 1;
  _Complex float cf = 2 + 3i;
  double d = 4;
  _Complex double cd = 5 + 6i;
  long double ld = 7;
  _Complex long double cld = 8 + 9i;
  int i = 10;
  _Complex int ci = 11 + 12i;
  CHECK_CALL (t5v (f), 2, var_f);
  CHECK_CALL (t5v (cf), 12, var_f);
  CHECK_CALL (t5v (d), 6, var_d);
  CHECK_CALL (t5v (cd), 31, var_d);
  CHECK_CALL (t5v (ld), 10, var_ld);
  CHECK_CALL (t5v (cld), 56, var_ld);
  CHECK_CALL (t5v (i), 12, var_d);
  CHECK_CALL (t5v (ci), 61, var_d);
}

/* Test complex argument, complex return type.  */

_Complex float t6f (_Complex float x) { return 1 + x; }
_Complex double t6d (_Complex double x) { return 2 + x; }
_Complex long double t6l (_Complex long double x) { return 3 + x; }

#define t6v(x) __builtin_tgmath (t6f, t6d, t6l, x)

static void
test_6 (void)
{
  float f = 1;
  _Complex float cf = 2 + 3i;
  double d = 4;
  _Complex double cd = 5 + 6i;
  long double ld = 7;
  _Complex long double cld = 8 + 9i;
  int i = 10;
  _Complex int ci = 11 + 12i;
  CHECK_CALL (t6v (f), 2, var_cf);
  CHECK_CALL (t6v (cf), 3 + 3i, var_cf);
  CHECK_CALL (t6v (d), 6, var_cd);
  CHECK_CALL (t6v (cd), 7 + 6i, var_cd);
  CHECK_CALL (t6v (ld), 10, var_cld);
  CHECK_CALL (t6v (cld), 11 + 9i, var_cld);
  CHECK_CALL (t6v (i), 12, var_cd);
  CHECK_CALL (t6v (ci), 13 + 12i, var_cd);
}

/* Test real and complex argument, real return type.  */

float t7f (float x) { return 1 + x; }
float t7cf (_Complex float x) { return 2 + __real__ x; }
double t7d (double x) { return 3 + x; }
double t7cd (_Complex double x) { return 4 + __real__ x; }
long double t7l (long double x) { return 5 + x; }
long double t7cl (_Complex long double x) { return 6 + __real__ x; }

#define t7v(x) __builtin_tgmath (t7f, t7d, t7l, t7cf, t7cd, t7cl, x)

static void
test_7 (void)
{
  float f = 1;
  _Complex float cf = 2 + 3i;
  double d = 4;
  _Complex double cd = 5 + 6i;
  long double ld = 7;
  _Complex long double cld = 8 + 9i;
  int i = 10;
  _Complex int ci = 11 + 12i;
  CHECK_CALL (t7v (f), 2, var_f);
  CHECK_CALL (t7v (cf), 4, var_f);
  CHECK_CALL (t7v (d), 7, var_d);
  CHECK_CALL (t7v (cd), 9, var_d);
  CHECK_CALL (t7v (ld), 12, var_ld);
  CHECK_CALL (t7v (cld), 14, var_ld);
  CHECK_CALL (t7v (i), 13, var_d);
  CHECK_CALL (t7v (ci), 15, var_d);
}

/* Test real and complex argument, real and complex return type.  */

float t8f (float x) { return 1 + x; }
_Complex float t8cf (_Complex float x) { return 2 + x; }
double t8d (double x) { return 3 + x; }
_Complex double t8cd (_Complex double x) { return 4 + x; }
long double t8l (long double x) { return 5 + x; }
_Complex long double t8cl (_Complex long double x) { return 6 + x; }

#define t8v(x) __builtin_tgmath (t8f, t8d, t8l, t8cf, t8cd, t8cl, x)

static void
test_8 (void)
{
  float f = 1;
  _Complex float cf = 2 + 3i;
  double d = 4;
  _Complex double cd = 5 + 6i;
  long double ld = 7;
  _Complex long double cld = 8 + 9i;
  int i = 10;
  _Complex int ci = 11 + 12i;
  CHECK_CALL (t8v (f), 2, var_f);
  CHECK_CALL (t8v (cf), 4 + 3i, var_cf);
  CHECK_CALL (t8v (d), 7, var_d);
  CHECK_CALL (t8v (cd), 9 + 6i, var_cd);
  CHECK_CALL (t8v (ld), 12, var_ld);
  CHECK_CALL (t8v (cld), 14 + 9i, var_cld);
  CHECK_CALL (t8v (i), 13, var_d);
  CHECK_CALL (t8v (ci), 15 + 12i, var_cd);
}

/* Test multiple type-generic arguments, real and complex.  */

float t9f (float x, float y) { return x + 10 * y; }
_Complex float t9cf (_Complex float x, _Complex float y) { return x + 100 * y; }
double t9d (double x, double y) { return x + 1000 * y; }
_Complex double t9cd (_Complex double x, _Complex double y) { return x + 10000 * y; }
long double t9l (long double x, long double y) { return x + 100000 * y; }
_Complex long double t9cl (_Complex long double x, _Complex long double y) { return x + 1000000 * y; }

#define t9v(x, y) __builtin_tgmath (t9f, t9d, t9l, t9cf, t9cd, t9cl, x, y)

static void
test_9 (void)
{
  float f = 1;
  _Complex float cf = 2 + 3i;
  double d = 4;
  _Complex double cd = 5 + 6i;
  long double ld = 7;
  _Complex long double cld = 8 + 9i;
  int i = 10;
  _Complex int ci = 11 + 12i;
  CHECK_CALL (t9v (f, f), 11, var_f);
  CHECK_CALL (t9v (f, cf), 201 + 300i, var_cf);
  CHECK_CALL (t9v (cf, f), 102 + 3i, var_cf);
  CHECK_CALL (t9v (f, i), 10001, var_d);
  CHECK_CALL (t9v (i, f), 1010, var_d);
  CHECK_CALL (t9v (d, d), 4004, var_d);
  CHECK_CALL (t9v (d, cd), 50004 + 60000i, var_cd);
  CHECK_CALL (t9v (ld, i), 1000007, var_ld);
  CHECK_CALL (t9v (cf, cld), 8000002 + 9000003i, var_cld);
  CHECK_CALL (t9v (i, i), 10010, var_d);
  CHECK_CALL (t9v (ci, i), 100011 + 12i, var_cd);
}

/* Test functions rounding result to narrower type.  */

float t10d (double x) { return 1 + x; }
float t10l (long double x) { return 2 + x; }

#define t10v(x) __builtin_tgmath (t10d, t10l, x)

static void
test_10 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  short s = 4;
  CHECK_CALL (t10v (f), 2, var_f);
  CHECK_CALL (t10v (d), 3, var_f);
  CHECK_CALL (t10v (ld), 5, var_f);
  CHECK_CALL (t10v (s), 5, var_f);
}

int
main (void)
{
  test_1 ();
  test_2 ();
  test_3 ();
  test_4 ();
  test_5 ();
  test_6 ();
  test_7 ();
  test_8 ();
  test_9 ();
  test_10 ();
  exit (0);
}
