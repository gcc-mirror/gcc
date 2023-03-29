/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-fsignaling-nans" } */
/* Workaround for PR57484 on ia32: */
/* { dg-additional-options "-msse2 -mfpmath=sse" { target { ia32 && sse2_runtime } } } */

#if !defined(EXT) && !defined(TYPE)
int
f1 (void)
{
  return __builtin_issignaling (__builtin_nansf (""));
}

int
f2 (void)
{
  return __builtin_issignaling (__builtin_nan (""));
}

int
f3 (void)
{
  return __builtin_issignaling (0.0L);
}

int
f4 (float x)
{
  return __builtin_issignaling (x);
}

int
f5 (double x)
{
  return __builtin_issignaling (x);
}

int
f6 (long double x)
{
  return __builtin_issignaling (x);
}
#else
#ifndef TYPE
# define CONCATX(X, Y) X ## Y
# define CONCAT(X, Y) CONCATX (X, Y)
# define CONCAT3(X, Y, Z) CONCAT (CONCAT (X, Y), Z)
# define CONCAT4(W, X, Y, Z) CONCAT (CONCAT (CONCAT (W, X), Y), Z)

# if EXT
#  define TYPE CONCAT3 (_Float, WIDTH, x)
#  define CST(C) CONCAT4 (C, f, WIDTH, x)
#  define FN(F) CONCAT4 (F, f, WIDTH, x)
# else
#  define TYPE CONCAT (_Float, WIDTH)
#  define CST(C) CONCAT3 (C, f, WIDTH)
#  define FN(F) CONCAT3 (F, f, WIDTH)
# endif
#endif
#ifndef NANS
# define NANS(x) FN (__builtin_nans) (x)
#endif
#ifndef NAN
# define NAN(x) FN (__builtin_nan) (x)
#endif
#ifndef INF
# define INF FN (__builtin_inf) ()
#endif

int
f1 (void)
{
  return __builtin_issignaling (NANS (""));
}

int
f2 (void)
{
  return __builtin_issignaling (NAN (""));
}

int
f3 (void)
{
  return __builtin_issignaling (CST (0.0));
}

int
f4 (TYPE x)
{
  return __builtin_issignaling (x);
}
#endif

#ifndef EXT
float x;
double y;
long double z;
#else
TYPE w;
#endif

int
main ()
{
  if (!f1 () || f2 () || f3 ())
    __builtin_abort ();
  asm volatile ("" : : : "memory");
#ifndef EXT
  if (f4 (x) || !f4 (__builtin_nansf ("0x123")) || f4 (42.0f) || f4 (__builtin_nanf ("0x234"))
      || f4 (__builtin_inff ()) || f4 (-__builtin_inff ()) || f4 (-42.0f) || f4 (-0.0f) || f4 (0.0f))
    __builtin_abort ();
  x = __builtin_nansf ("");
  asm volatile ("" : : : "memory");
  if (!f4 (x))
    __builtin_abort ();
  if (f5 (y) || !f5 (__builtin_nans ("0x123")) || f5 (42.0) || f5 (__builtin_nan ("0x234"))
      || f5 (__builtin_inf ()) || f5 (-__builtin_inf ()) || f5 (-42.0) || f5 (-0.0) || f5 (0.0))
    __builtin_abort ();
  y = __builtin_nans ("");
  asm volatile ("" : : : "memory");
  if (!f5 (y))
    __builtin_abort ();
  if (f6 (z) || !f6 (__builtin_nansl ("0x123")) || f6 (42.0L) || f6 (__builtin_nanl ("0x234"))
      || f6 (__builtin_infl ()) || f6 (-__builtin_infl ()) || f6 (-42.0L) || f6 (-0.0L) || f6 (0.0L))
    __builtin_abort ();
  z = __builtin_nansl ("");
  asm volatile ("" : : : "memory");
  if (!f6 (z))
    __builtin_abort ();
#else
  if (f4 (w) || !f4 (NANS ("0x123")) || f4 (CST (42.0)) || f4 (NAN ("0x234"))
      || f4 (INF) || f4 (-INF) || f4 (CST (-42.0)) || f4 (CST (-0.0)) || f4 (CST (0.0)))
    __builtin_abort ();
  w = NANS ("");
  asm volatile ("" : : : "memory");
  if (!f4 (w))
    __builtin_abort ();
#endif
  return 0;
}
