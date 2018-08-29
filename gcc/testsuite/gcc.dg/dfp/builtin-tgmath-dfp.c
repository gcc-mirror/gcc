/* Test __builtin_tgmath: valid uses, decimal floating-point types.  */
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
extern _Decimal32 var_d32;
extern _Decimal64 var_d64;
extern _Decimal128 var_d128;
extern int var_i;

/* Test decimal-only function, single argument.  */

_Decimal32 t1d32 (_Decimal32 x) { return x + 1; }
_Decimal64 t1d64 (_Decimal64 x) { return x + 2; }
_Decimal128 t1d128 (_Decimal128 x) { return x + 3; }

#define t1v(x) __builtin_tgmath (t1d32, t1d64, t1d128, x)

static void
test_1 (void)
{
  _Decimal32 d32 = 32;
  _Decimal64 d64 = 64;
  _Decimal128 d128 = 128;
  int i = 256;
  CHECK_CALL (t1v (d32), 33, var_d32);
  CHECK_CALL (t1v (d64), 66, var_d64);
  CHECK_CALL (t1v (d128), 131, var_d128);
  CHECK_CALL (t1v (i), 258, var_d64);
}

/* Test decimal-only function, two arguments.  */

_Decimal32 t2d32 (_Decimal32 x, _Decimal32 y) { return 10 * x + y; }
_Decimal64 t2d64 (_Decimal64 x, _Decimal64 y) { return 100 * x + y;; }
_Decimal128 t2d128 (_Decimal128 x, _Decimal128 y) { return 1000 * x + y; }

#define t2v(x, y) __builtin_tgmath (t2d32, t2d64, t2d128, x, y)

static void
test_2 (void)
{
  _Decimal32 d32 = 1;
  _Decimal64 d64 = 2;
  _Decimal128 d128 = 3;
  int i = 4;
  CHECK_CALL (t2v (d32, d32), 11, var_d32);
  CHECK_CALL (t2v (d64, d64), 202, var_d64);
  CHECK_CALL (t2v (d32, d64), 102, var_d64);
  CHECK_CALL (t2v (d128, d64), 3002, var_d128);
  CHECK_CALL (t2v (d128, i), 3004, var_d128);
  CHECK_CALL (t2v (i, i), 404, var_d64);
  CHECK_CALL (t2v (i, d32), 401, var_d64);
}

/* Test real-only function, single argument.  */

float t3f (float x) { return x + 1; }
double t3d (double x) { return x + 2; }
long double t3l (long double x) { return x + 3; }
_Decimal32 t3d32 (_Decimal32 x) { return x + 4; }
_Decimal64 t3d64 (_Decimal64 x) { return x + 5; }
_Decimal128 t3d128 (_Decimal128 x) { return x + 6; }

#define t3v(x) __builtin_tgmath (t3f, t3d, t3l, t3d32, t3d64, t3d128, x)

static void
test_3 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  int i = 4;
  _Decimal32 d32 = 5;
  _Decimal64 d64 = 6;
  _Decimal128 d128 = 7;
  CHECK_CALL (t3v (f), 2, var_f);
  CHECK_CALL (t3v (d), 4, var_d);
  CHECK_CALL (t3v (ld), 6, var_ld);
  CHECK_CALL (t3v (i), 6, var_d);
  CHECK_CALL (t3v (d32), 9, var_d32);
  CHECK_CALL (t3v (d64), 11, var_d64);
  CHECK_CALL (t3v (d128), 13, var_d128);
}

/* Test real-and-complex function, single argument.  */

float t4f (float x) { return x + 1; }
double t4d (double x) { return x + 2; }
long double t4l (long double x) { return x + 3; }
_Complex float t4cf (_Complex float x) { return x + 4; }
_Complex double t4cd (_Complex double x) { return x + 5; }
_Complex long double t4cl (_Complex long double x) { return x + 6; }
_Decimal32 t4d32 (_Decimal32 x) { return x + 7; }
_Decimal64 t4d64 (_Decimal64 x) { return x + 8; }
_Decimal128 t4d128 (_Decimal128 x) { return x + 9; }

#define t4v(x) __builtin_tgmath (t4f, t4d, t4l, t4cf, t4cd, t4cl, t4d32, t4d64, t4d128, x)

static void
test_4 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  int i = 4;
  _Complex float cf = 5;
  _Complex double cd = 6;
  _Complex long double cld = 7;
  _Complex int ci = 8;
  _Decimal32 d32 = 9;
  _Decimal64 d64 = 10;
  _Decimal128 d128 = 11;
  CHECK_CALL (t4v (f), 2, var_f);
  CHECK_CALL (t4v (d), 4, var_d);
  CHECK_CALL (t4v (ld), 6, var_ld);
  CHECK_CALL (t4v (i), 6, var_d);
  CHECK_CALL (t4v (cf), 9, var_cf);
  CHECK_CALL (t4v (cd), 11, var_cd);
  CHECK_CALL (t4v (cld), 13, var_cld);
  CHECK_CALL (t4v (ci), 13, var_cd);
  CHECK_CALL (t4v (d32), 16, var_d32);
  CHECK_CALL (t4v (d64), 18, var_d64);
  CHECK_CALL (t4v (d128), 20, var_d128);
}

/* Test real-and-complex function, real return type, single argument.  */

float t5f (float x) { return x + 1; }
double t5d (double x) { return x + 2; }
long double t5l (long double x) { return x + 3; }
float t5cf (_Complex float x) { return __real__ x + 4; }
double t5cd (_Complex double x) { return __real__ x + 5; }
long double t5cl (_Complex long double x) { return __real__ x + 6; }
_Decimal32 t5d32 (_Decimal32 x) { return x + 7; }
_Decimal64 t5d64 (_Decimal64 x) { return x + 8; }
_Decimal128 t5d128 (_Decimal128 x) { return x + 9; }

#define t5v(x) __builtin_tgmath (t5f, t5d, t5l, t5cf, t5cd, t5cl, t5d32, t5d64, t5d128, x)

static void
test_5 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  int i = 4;
  _Complex float cf = 5;
  _Complex double cd = 6;
  _Complex long double cld = 7;
  _Complex int ci = 8;
  _Decimal32 d32 = 9;
  _Decimal64 d64 = 10;
  _Decimal128 d128 = 11;
  CHECK_CALL (t5v (f), 2, var_f);
  CHECK_CALL (t5v (d), 4, var_d);
  CHECK_CALL (t5v (ld), 6, var_ld);
  CHECK_CALL (t5v (i), 6, var_d);
  CHECK_CALL (t5v (cf), 9, var_f);
  CHECK_CALL (t5v (cd), 11, var_d);
  CHECK_CALL (t5v (cld), 13, var_ld);
  CHECK_CALL (t5v (ci), 13, var_d);
  CHECK_CALL (t5v (d32), 16, var_d32);
  CHECK_CALL (t5v (d64), 18, var_d64);
  CHECK_CALL (t5v (d128), 20, var_d128);
}

/* Test real-and-complex function, two arguments.  */

float t6f (float x, float y) { return x * 10 + y; }
double t6d (double x, double y) { return x * 100 + y; }
long double t6l (long double x, long double y) { return x * 1000 + y; }
_Complex float t6cf (_Complex float x, _Complex float y) { return x * 10000 + y; }
_Complex double t6cd (_Complex double x, _Complex double y) { return x * 100000 + y; }
_Complex long double t6cl (_Complex long double x, _Complex long double y) { return x * 1000000 + y; }
_Decimal32 t6d32 (_Decimal32 x, _Decimal32 y) { return x * 50 + y; }
_Decimal64 t6d64 (_Decimal64 x, _Decimal64 y) { return x * 500 + y; }
_Decimal128 t6d128 (_Decimal128 x, _Decimal128 y) { return x * 5000 + y; }

#define t6v(x, y) __builtin_tgmath (t6f, t6d, t6l, t6cf, t6cd, t6cl, t6d32, t6d64, t6d128, x, y)

static void
test_6 (void)
{
  float f = 1;
  double d = 2;
  long double ld = 3;
  int i = 4;
  _Complex float cf = 5;
  _Complex double cd = 6;
  _Complex long double cld = 7;
  _Complex int ci = 8;
  _Decimal32 d32 = 9;
  _Decimal64 d64 = 10;
  _Decimal128 d128 = 11;
  CHECK_CALL (t6v (f, f), 11, var_f);
  CHECK_CALL (t6v (d, f), 201, var_d);
  CHECK_CALL (t6v (f, d), 102, var_d);
  CHECK_CALL (t6v (f, i), 104, var_d);
  CHECK_CALL (t6v (ld, f), 3001, var_ld);
  CHECK_CALL (t6v (i, ld), 4003, var_ld);
  CHECK_CALL (t6v (i, i), 404, var_d);
  CHECK_CALL (t6v (cf, f), 50001, var_cf);
  CHECK_CALL (t6v (cf, cf), 50005, var_cf);
  CHECK_CALL (t6v (cd, cf), 600005, var_cd);
  CHECK_CALL (t6v (d, cld), 2000007, var_cld);
  CHECK_CALL (t6v (ci, ci), 800008, var_cd);
  CHECK_CALL (t6v (ci, f), 800001, var_cd);
  CHECK_CALL (t6v (d32, d32), 459, var_d32);
  CHECK_CALL (t6v (d64, i), 5004, var_d64);
  CHECK_CALL (t6v (i, d32), 2009, var_d64);
  CHECK_CALL (t6v (d128, d32), 55009, var_d128);
}

/* Test decimal-only function rounding result to narrower type.  */

_Decimal32 t7d64 (_Decimal64 x) { return 1 + x; }
_Decimal32 t7d128 (_Decimal128 x) { return 2 + x; }

#define t7v(x) __builtin_tgmath (t7d64, t7d128, x)

static void
test_7 (void)
{
  _Decimal32 d32 = 1;
  _Decimal64 d64 = 2;
  _Decimal128 d128 = 3;
  short s = 4;
  CHECK_CALL (t7v (d32), 2, var_d32);
  CHECK_CALL (t7v (d64), 3, var_d32);
  CHECK_CALL (t7v (d128), 5, var_d32);
  CHECK_CALL (t7v (s), 5, var_d32);
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
  exit (0);
}
