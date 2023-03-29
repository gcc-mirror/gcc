/* Tests for _FloatN / _FloatNx types: compile and execution tests for
   built-in functions.  Before including this file, define WIDTH as
   the value N; define EXT to 1 for _FloatNx and 0 for _FloatN.  */

#define CONCATX(X, Y) X ## Y
#define CONCAT(X, Y) CONCATX (X, Y)
#define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
#define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

#if EXT
# define TYPE CONCAT3 (_Float, WIDTH, x)
# define CST(C) CONCAT4 (C, f, WIDTH, x)
# define FN(F) CONCAT4 (F, f, WIDTH, x)
#else
# define TYPE CONCAT (_Float, WIDTH)
# define CST(C) CONCAT3 (C, f, WIDTH)
# define FN(F) CONCAT3 (F, f, WIDTH)
#endif

extern void exit (int);
extern void abort (void);

extern int test_i;
extern long int test_l;
extern long long int test_ll;
extern TYPE test_type;
extern __typeof (FN (__builtin_inf) ()) test_type;
extern __typeof (FN (__builtin_huge_val) ()) test_type;
extern __typeof (FN (__builtin_nan) ("")) test_type;
extern __typeof (FN (__builtin_nans) ("")) test_type;
extern __typeof (FN (__builtin_fabs) (0)) test_type;
extern __typeof (FN (__builtin_copysign) (0, 0)) test_type;
extern __typeof (FN (__builtin_acosh) (0)) test_type;
extern __typeof (FN (__builtin_acos) (0)) test_type;
extern __typeof (FN (__builtin_asinh) (0)) test_type;
extern __typeof (FN (__builtin_asin) (0)) test_type;
extern __typeof (FN (__builtin_atanh) (0)) test_type;
extern __typeof (FN (__builtin_atan) (0)) test_type;
extern __typeof (FN (__builtin_cbrt) (0)) test_type;
extern __typeof (FN (__builtin_cosh) (0)) test_type;
extern __typeof (FN (__builtin_cos) (0)) test_type;
extern __typeof (FN (__builtin_erfc) (0)) test_type;
extern __typeof (FN (__builtin_erf) (0)) test_type;
extern __typeof (FN (__builtin_exp2) (0)) test_type;
extern __typeof (FN (__builtin_exp) (0)) test_type;
extern __typeof (FN (__builtin_expm1) (0)) test_type;
extern __typeof (FN (__builtin_lgamma) (0)) test_type;
extern __typeof (FN (__builtin_log10) (0)) test_type;
extern __typeof (FN (__builtin_log1p) (0)) test_type;
extern __typeof (FN (__builtin_log2) (0)) test_type;
extern __typeof (FN (__builtin_logb) (0)) test_type;
extern __typeof (FN (__builtin_log) (0)) test_type;
extern __typeof (FN (__builtin_nextafter) (0, 0)) test_type;
extern __typeof (FN (__builtin_sinh) (0)) test_type;
extern __typeof (FN (__builtin_sin) (0)) test_type;
extern __typeof (FN (__builtin_tanh) (0)) test_type;
extern __typeof (FN (__builtin_tan) (0)) test_type;
extern __typeof (FN (__builtin_tgamma) (0)) test_type;
extern __typeof (FN (__builtin_atan2) (0, 0)) test_type;
extern __typeof (FN (__builtin_fdim) (0, 0)) test_type;
extern __typeof (FN (__builtin_fmod) (0, 0)) test_type;
extern __typeof (FN (__builtin_frexp) (0, &test_i)) test_type;
extern __typeof (FN (__builtin_ldexp) (0, 0)) test_type;
extern __typeof (FN (__builtin_hypot) (0, 0)) test_type;
extern __typeof (FN (__builtin_ilogb) (0)) test_i;
extern __typeof (FN (__builtin_llrint) (0)) test_ll;
extern __typeof (FN (__builtin_llround) (0)) test_ll;
extern __typeof (FN (__builtin_lrint) (0)) test_l;
extern __typeof (FN (__builtin_lround) (0)) test_l;
extern __typeof (FN (__builtin_modf) (0, &test_type)) test_type;
extern __typeof (FN (__builtin_pow) (0, 0)) test_type;
extern __typeof (FN (__builtin_remainder) (0, 0)) test_type;
extern __typeof (FN (__builtin_remquo) (0, 0, &test_i)) test_type;
extern __typeof (FN (__builtin_scalbln) (0, 0L)) test_type;
extern __typeof (FN (__builtin_scalbn) (0, 0)) test_type;

volatile TYPE inf_cst = FN (__builtin_inf) ();
volatile TYPE huge_val_cst = FN (__builtin_huge_val) ();
volatile TYPE nan_cst = FN (__builtin_nan) ("");
volatile TYPE nans_cst = FN (__builtin_nans) ("");
volatile TYPE neg0 = -CST (0.0), neg1 = -CST (1.0), one = 1.0;
volatile TYPE t1 = FN (__builtin_acosh) (CST (1.0));
volatile TYPE t2 = FN (__builtin_acos) (CST (1.0));
volatile TYPE t3 = FN (__builtin_asinh) (CST (0.0));
volatile TYPE t4 = FN (__builtin_asin) (CST (0.0));
volatile TYPE t5 = FN (__builtin_atanh) (CST (0.0));
volatile TYPE t6 = FN (__builtin_atan) (CST (0.0));
volatile TYPE t7 = FN (__builtin_cbrt) (CST (27.0));
volatile TYPE t8 = FN (__builtin_cosh) (CST (0.0));
volatile TYPE t9 = FN (__builtin_cos) (CST (0.0));
volatile TYPE t10 = FN (__builtin_erfc) (CST (0.0));
volatile TYPE t11 = FN (__builtin_erf) (CST (0.0));
volatile TYPE t12 = FN (__builtin_exp2) (CST (1.0));
volatile TYPE t13 = FN (__builtin_exp) (CST (0.0));
volatile TYPE t14 = FN (__builtin_expm1) (CST (0.0));
volatile TYPE t15 = FN (__builtin_log10) (CST (1.0));
volatile TYPE t16 = FN (__builtin_log1p) (CST (0.0));
volatile TYPE t17 = FN (__builtin_log2) (CST (1.0));
volatile TYPE t18 = FN (__builtin_logb) (CST (1.0));
volatile TYPE t19 = FN (__builtin_log) (CST (1.0));
volatile TYPE t20 = FN (__builtin_nextafter) (CST (0.0), CST (0.0));
volatile TYPE t21 = FN (__builtin_sinh) (CST (0.0));
volatile TYPE t22 = FN (__builtin_sin) (CST (0.0));
volatile TYPE t23 = FN (__builtin_tanh) (CST (0.0));
volatile TYPE t24 = FN (__builtin_tan) (CST (0.0));
volatile TYPE t25 = FN (__builtin_atan2) (CST (0.0), CST (1.0));
volatile TYPE t26 = FN (__builtin_fdim) (CST (0.0), CST (0.0));
volatile TYPE t27 = FN (__builtin_fmod) (CST (0.0), CST (1.0));
volatile TYPE t28 = FN (__builtin_ldexp) (CST (1.0), 1);
volatile TYPE t29 = FN (__builtin_hypot) (CST (3.0), CST (4.0));
volatile int t30 = FN (__builtin_ilogb) (CST (1.0));
volatile long long int t31 = FN (__builtin_llround) (CST (42.25));
volatile long int t32 = FN (__builtin_lround) (CST (42.25));
volatile TYPE t33 = FN (__builtin_pow) (CST (1.0), CST (2.0));
volatile TYPE t34 = FN (__builtin_remainder) (CST (7.0), CST (4.0));
volatile TYPE t35 = FN (__builtin_scalbln) (CST (1.0), 1L);
volatile TYPE t36 = FN (__builtin_scalbn) (CST (1.0), 1);

int
main (void)
{
  volatile TYPE r;
  if (!__builtin_isinf (inf_cst))
    abort ();
  if (!__builtin_isinf (huge_val_cst))
    abort ();
  if (inf_cst != huge_val_cst)
    abort ();
  if (!__builtin_isnan (nan_cst))
    abort ();
  if (!__builtin_isnan (nans_cst))
    abort ();
  r = FN (__builtin_fabs) (neg1);
  if (r != CST (1.0))
    abort ();
  r = FN (__builtin_copysign) (one, neg0);
  if (r != neg1)
    abort ();
  r = FN (__builtin_copysign) (inf_cst, neg1);
  if (r != -huge_val_cst)
    abort ();
  r = FN (__builtin_copysign) (-inf_cst, one);
  if (r != huge_val_cst)
    abort ();
  if (t1 != CST (0.0))
    abort ();
  if (t2 != CST (0.0))
    abort ();
  if (t3 != CST (0.0))
    abort ();
  if (t4 != CST (0.0))
    abort ();
  if (t5 != CST (0.0))
    abort ();
  if (t6 != CST (0.0))
    abort ();
  if (t7 != CST (3.0))
    abort ();
  if (t8 != CST (1.0))
    abort ();
  if (t9 != CST (1.0))
    abort ();
  if (t10 != CST (1.0))
    abort ();
  if (t11 != CST (0.0))
    abort ();
  if (t12 != CST (2.0))
    abort ();
  if (t13 != CST (1.0))
    abort ();
  if (t14 != CST (0.0))
    abort ();
  if (t15 != CST (0.0))
    abort ();
  if (t16 != CST (0.0))
    abort ();
  if (t17 != CST (0.0))
    abort ();
  if (t18 != CST (0.0))
    abort ();
  if (t19 != CST (0.0))
    abort ();
  if (t20 != CST (0.0))
    abort ();
  if (t21 != CST (0.0))
    abort ();
  if (t22 != CST (0.0))
    abort ();
  if (t23 != CST (0.0))
    abort ();
  if (t24 != CST (0.0))
    abort ();
  if (t25 != CST (0.0))
    abort ();
  if (t26 != CST (0.0))
    abort ();
  if (t27 != CST (0.0))
    abort ();
  if (t28 != CST (2.0))
    abort ();
  if (t29 != CST (5.0))
    abort ();
  if (t30 != 0)
    abort ();
  if (t31 != 42)
    abort ();
  if (t32 != 42)
    abort ();
  if (t33 != CST (1.0))
    abort ();
  if (t34 != -CST (1.0))
    abort ();
  if (t35 != CST (2.0))
    abort ();
  if (t36 != CST (2.0))
    abort ();
  exit (0);
}
