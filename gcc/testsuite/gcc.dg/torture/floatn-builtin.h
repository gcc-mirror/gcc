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

extern TYPE test_type;
extern __typeof (FN (__builtin_inf) ()) test_type;
extern __typeof (FN (__builtin_huge_val) ()) test_type;
extern __typeof (FN (__builtin_nan) ("")) test_type;
extern __typeof (FN (__builtin_nans) ("")) test_type;
extern __typeof (FN (__builtin_fabs) (0)) test_type;
extern __typeof (FN (__builtin_copysign) (0, 0)) test_type;

volatile TYPE inf_cst = FN (__builtin_inf) ();
volatile TYPE huge_val_cst = FN (__builtin_huge_val) ();
volatile TYPE nan_cst = FN (__builtin_nan) ("");
volatile TYPE nans_cst = FN (__builtin_nans) ("");
volatile TYPE neg0 = -CST (0.0), neg1 = -CST (1.0), one = 1.0;

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
  exit (0);
}
