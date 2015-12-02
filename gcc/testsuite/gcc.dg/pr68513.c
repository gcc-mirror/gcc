/* PR c/68513 */
/* { dg-do compile } */
/* { dg-options "-funsafe-math-optimizations -fno-math-errno -O -Wno-div-by-zero" } */

int i;
unsigned u;
volatile int *e;

#define E (i ? *e : 0)

/* Can't trigger some of them because operand_equal_p will return false
   for side-effects.  */

/* (x & ~m) | (y & m) -> ((x ^ y) & m) ^ x */
int
fn1 (void)
{
  int r = 0;
  r += (short) (E & ~u | i & u);
  r += -(short) (E & ~u | i & u);
  r += (short) -(E & ~u | i & u);
  return r;
}

/* sqrt(x) < y is x >= 0 && x != +Inf, when y is large.  */
double
fn2 (void)
{
  double r;
  r = __builtin_sqrt (E) < __builtin_inf ();
  return r;
}

/* sqrt(x) < c is the same as x >= 0 && x < c*c.  */
double
fn3 (void)
{
  double r;
  r = __builtin_sqrt (E) < 1.3;
  return r;
}

/* copysign(x,y)*copysign(x,y) -> x*x.  */
double
fn4 (double y, double x)
{
  return __builtin_copysign (E, y) * __builtin_copysign (E, y);
}

/* x <= +Inf is the same as x == x, i.e. !isnan(x).  */
int
fn5 (void)
{
  return E <= __builtin_inf ();
}

/* Fold (A & ~B) - (A & B) into (A ^ B) - B.  */
int
fn6 (void)
{
  return (i & ~E) - (i & E);
}

/* Fold (A & B) - (A & ~B) into B - (A ^ B).  */
int
fn7 (void)
{
  return (i & E) - (i & ~E);
}

/* x + (x & 1) -> (x + 1) & ~1 */
int
fn8 (void)
{
  return E + (E & 1);
}

/* Simplify comparison of something with itself.  */
int
fn9 (void)
{
  return E <= E | E >= E;
}

/* Fold (A & ~B) - (A & B) into (A ^ B) - B.  */
int
fn10 (void)
{
  return (i & ~E) - (i & E);
}

/* abs(x)*abs(x) -> x*x.  Should be valid for all types.  */
int
fn11 (void)
{
  return __builtin_abs (E) * __builtin_abs (E);
}

/* (x | CST1) & CST2 -> (x & CST2) | (CST1 & CST2) */
int
fn12 (void)
{
  return (E | 11) & 12;
}

/* fold_range_test */
int
fn13 (const char *s)
{
  return s[E] != '\0' && s[E] != '/';
}

/* fold_comparison */
int
fn14 (void)
{
  return (!!i ? : (u *= E / 0)) >= (u = E);
}

/* fold_mult_zconjz */
_Complex int
fn15 (_Complex volatile int *z)
{
  return *z * ~*z;
}
