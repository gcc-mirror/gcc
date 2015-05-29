/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#ifdef __SIZEOF_INT128__
#define T __int128
#else
#define T long long
#endif

extern void impossible (void);

void f(T x)
{
  unsigned T y;
  unsigned T z;
  if (x < -7)
    return;
  if (x > 2)
    return;
  y = x;
  z = y * y;
  if (z == 666)
    impossible ();
}

void g(unsigned T x)
{
  unsigned T y;
  unsigned T z;
  unsigned T m = -1;
  m = m / 2;
  if (x < m-2)
    return;
  if (x > m-1)
    return;
  y = x;
  z = y * y;
  /* The product (ignoring it is a square) has only 3 possible values:
     4, 9 and 2^127+6.  At least one of the values 7, 666 and -666 is
     known to be impossible.  7 is the most logical in the current
     implementation.  */
  if (z == 7)
    impossible ();
}

/* { dg-final { scan-tree-dump-not "impossible" "optimized" } } */
