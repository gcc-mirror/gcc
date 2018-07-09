/* PR libstdc++/85466 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-math-errno -fno-trapping-math -fdump-tree-optimized" } */
/* { dg-add-options ieee } */
/* { dg-final { scan-tree-dump-not "nextafter" "optimized" } } */
/* { dg-final { scan-tree-dump-not "nexttoward" "optimized" } } */

float nextafterf (float, float);
double nextafter (double, double);
long double nextafterl (long double, long double);
float nexttowardf (float, long double);
double nexttoward (double, long double);
long double nexttowardl (long double, long double);

#define CHECK(x) if (!(x)) __builtin_abort ()

#ifndef NEED_ERRNO
#define NEED_ERRNO 0
#endif
#ifndef NEED_EXC
#define NEED_EXC 0
#endif
#ifndef NO_LONG_DOUBLE
#define NO_LONG_DOUBLE (__LDBL_MANT_DIG__ == 106)
#endif

#define TEST(name, fn, type, L1, L2, l1, l2, MIN1,			     \
	     MAX1, DENORM_MIN1, EPSILON1, MIN2, MAX2, DENORM_MIN2)	     \
void									     \
name (void)								     \
{									     \
  const type a = fn (0.0##L1, 0.0##L2);					     \
  CHECK (a == 0.0##L1 && !__builtin_signbit (a));			     \
  const type b = fn (0.0##L1, -0.0##L2);				     \
  CHECK (b == 0.0##L1 && __builtin_signbit (b));			     \
  const type c = fn (__builtin_nan##l1 (""), 0.0##L2);			     \
  CHECK (__builtin_isnan##l1 (c));					     \
  const type d = fn (2.0##L1, __builtin_nan##l2 (""));			     \
  CHECK (__builtin_isnan##l1 (d));					     \
  const type e = NEED_EXC ? DENORM_MIN1 : fn (0.0##L1, 8.0##L2);	     \
  CHECK (e == DENORM_MIN1);						     \
  const type f = fn (1.0##L1, 8.0##L2);					     \
  CHECK (f == 1.0##L1 + EPSILON1);					     \
  const type g = fn (1.0##L1, -8.0##L2);				     \
  CHECK (g == 1.0##L1 - EPSILON1 / 2.0##L1);				     \
  const type h = fn (__builtin_inf (), 0.0##L2);			     \
  CHECK (h == MAX1);							     \
  const type i = fn (-1.0##L1, -__builtin_inf ());			     \
  CHECK (i == -1.0##L1 - EPSILON1);					     \
  const type j = fn (1.5##L1, __builtin_inf ());			     \
  CHECK (j == 1.5##L1 + EPSILON1);					     \
  const type k = fn (1.5##L1 - EPSILON1, 100.0##L2);			     \
  CHECK (k == 1.5##L1);							     \
  const type l								     \
    = (NEED_EXC || NEED_ERRNO) ? 0.0##L1 : fn (DENORM_MIN1, 0.0##L2);	     \
  CHECK (l == 0.0##L1 && !__builtin_signbit (l));			     \
  const type m								     \
    = (NEED_EXC || NEED_ERRNO) ? __builtin_inf##l1 ()			     \
      : fn (MAX1, __builtin_inf ());					     \
  CHECK (__builtin_isinf##l1 (m) && !__builtin_signbit (m));		     \
  const type n = fn (DENORM_MIN1, 12.0##L2);				     \
  CHECK (n == 2.0##L1 * DENORM_MIN1);					     \
  const type o = fn (n, 24.0##L2);					     \
  CHECK (o == 3.0##L1 * DENORM_MIN1);					     \
  const type p = fn (o, 132.0##L2);					     \
  CHECK (p == 4.0##L1 * DENORM_MIN1);					     \
  const type q = fn (2.0##L1 * DENORM_MIN1, -__builtin_inf ());		     \
  CHECK (q == DENORM_MIN1);						     \
  const type r = fn (3.0##L1 * DENORM_MIN1, DENORM_MIN2);		     \
  CHECK (r == 2.0##L1 * DENORM_MIN1);					     \
  const type s = fn (4.0##L1 * DENORM_MIN1, 2.0##L2 * DENORM_MIN2);	     \
  CHECK (s == 3.0##L1 * DENORM_MIN1);					     \
  const type t = fn (MIN1, 0.0##L2);					     \
  CHECK (t == MIN1 - DENORM_MIN1);					     \
  const type u = fn (MIN1 - DENORM_MIN1, -MIN2);			     \
  CHECK (u == MIN1 - 2.0##L1 * DENORM_MIN1);				     \
  const type v = fn (MIN1 - 2.0##L1 * DENORM_MIN1, 100.0##L2);		     \
  CHECK (v == MIN1 - DENORM_MIN1);					     \
  const type w = fn (MIN1 - DENORM_MIN1, MAX2);				     \
  CHECK (w == MIN1);							     \
  const type x = fn (MIN1, 17.0##L2);					     \
  CHECK (x == MIN1 + DENORM_MIN1);					     \
  const type y = fn (MIN1 + DENORM_MIN1, __builtin_inf##l2 ());		     \
  CHECK (y == MIN1 + 2.0##L1 * DENORM_MIN1);				     \
  const type z = fn (MIN1 / 2.0##L1, -MIN2);				     \
  CHECK (z == MIN1 / 2.0##L1 - DENORM_MIN1);				     \
  const type aa = fn (-MIN1 / 4.0##L1, MIN2);				     \
  CHECK (aa == -MIN1 / 4.0##L1 + DENORM_MIN1);				     \
  const type ab = fn (MIN1 * 2.0##L1, -MIN2);				     \
  CHECK (ab == MIN1 * 2.0##L1 - DENORM_MIN1);				     \
  const type ac = fn (MIN1 * 4.0##L1, MIN2);				     \
  CHECK (ac == MIN1 * 4.0##L1 - DENORM_MIN1 * 2.0##L1);			     \
  const type ad = fn (MIN1 * 64.0##L1, MIN2);				     \
  CHECK (ad == MIN1 * 64.0##L1 - DENORM_MIN1 * 32.0##L1);		     \
  const type ae = fn (MIN1 / 2.0##L1 - DENORM_MIN1, 100.0##L2);		     \
  CHECK (ae == MIN1 / 2.0##L1);						     \
  const type af = fn (-MIN1 / 4 + DENORM_MIN1, -100.0##L2);		     \
  CHECK (af == -MIN1 / 4.0##L1);					     \
  const type ag = fn (MIN1 * 2.0##L1 - DENORM_MIN1, 100.0##L2);		     \
  CHECK (ag == MIN1 * 2.0##L1);						     \
  const type ah = fn (MIN1 * 4.0##L1 - 2.0##L1 * DENORM_MIN1, 100.0##L2);    \
  CHECK (ah == MIN1 * 4.0##L1);						     \
  const type ai = fn (MIN1 * 64.0##L1 - 32.0##L1 * DENORM_MIN1, 100.0##L2);  \
  CHECK (ai == MIN1 * 64.0##L1);					     \
  const type aj = fn (MIN1 * 64.0##L1, 100.0##L2);			     \
  CHECK (aj == MIN1 * 64.0##L1 + 64.0##L1 * DENORM_MIN1);		     \
  const type ak = fn (MIN1 * 64.0##L1 + DENORM_MIN1 * 64.0##L1, 1024.0##L2); \
  CHECK (ak == MIN1 * 64.0##L1 + 128.0##L1 * DENORM_MIN1);		     \
  const type al = fn (128.0##L1, 128.0##L2);				     \
  CHECK (al == 128.0##L1);						     \
  const type am = fn (128.0##L1, 129.0##L2);				     \
  CHECK (am == 128.0##L1 + 128.0##L1 * EPSILON1);			     \
  const type an = fn (-128.0##L1 + -128.0##L1 * EPSILON1, -130.0##L2);	     \
  CHECK (an == -128.0##L1 - 256.0##L1 * EPSILON1);			     \
  const type ao = fn (128.0##L1 + 256.0##L1 * EPSILON1, 256.0##L2);	     \
  CHECK (ao == 128.0##L1 + 384.0##L1 * EPSILON1);			     \
  const type ap = fn (128.0##L1 + 384.0##L1 * EPSILON1, -0.0##L2);	     \
  CHECK (ap == 128.0##L1 + 256.0##L1 * EPSILON1);			     \
  const type aq = fn (128.0##L1 + 256.0##L1 * EPSILON1, 1.0##L2);	     \
  CHECK (aq == 128.0##L1 + 128.0##L1 * EPSILON1);			     \
  const type ar = fn (128.0##L1 + 128.0##L1 * EPSILON1, 0.0##L2);	     \
  CHECK (ar == 128.0##L1);						     \
  const type as = fn (128.0##L1, 0.0##L2);				     \
  CHECK (as == 128.0##L1 - 64.0##L1 * EPSILON1);			     \
  const type at = fn (128.0##L1 - 64.0##L1 * EPSILON1, 5.0##L2);	     \
  CHECK (at == 128.0##L1 - 128.0##L1 * EPSILON1);			     \
}

TEST (test1, nextafterf, float, F, F, f, f, __FLT_MIN__, __FLT_MAX__,
      __FLT_DENORM_MIN__, __FLT_EPSILON__, __FLT_MIN__, __FLT_MAX__,
      __FLT_DENORM_MIN__)
TEST (test2, nextafter, double, , , , , __DBL_MIN__, __DBL_MAX__,
      __DBL_DENORM_MIN__, __DBL_EPSILON__, __DBL_MIN__, __DBL_MAX__,
      __DBL_DENORM_MIN__)
#if !NO_LONG_DOUBLE
TEST (test3, nextafterl, long double, L, L, l, l, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__, __LDBL_EPSILON__, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__)
TEST (test4, nexttowardf, float, F, L, f, l, __FLT_MIN__, __FLT_MAX__,
      __FLT_DENORM_MIN__, __FLT_EPSILON__, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__)
TEST (test5, nexttoward, double, , L, , l, __DBL_MIN__, __DBL_MAX__,
      __DBL_DENORM_MIN__, __DBL_EPSILON__, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__)
TEST (test6, nexttowardl, long double, L, L, l, l, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__, __LDBL_EPSILON__, __LDBL_MIN__, __LDBL_MAX__,
      __LDBL_DENORM_MIN__)
#endif

int
main ()
{
  test1 ();
  test2 ();
#if !NO_LONG_DOUBLE
  test3 ();
  test4 ();
  test5 ();
  test6 ();
#endif
  return 0;
}
