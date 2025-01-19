/* Test zero with different types as null pointer constant: bug 112556.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wno-pointer-compare" } */

enum e { ZERO };
enum e2 : bool { BZERO };
enum e3 : long { LZERO };

void *p1 = 0;
void *p2 = 0LL;
void *p3 = (char) 0;
void *p4 = 0UL;
void *p5 = (bool) 0;
void *p6 = (enum e) ZERO;
void *p7 = false;
void *p8 = BZERO;
void *p9 = (enum e2) 0;
void *p10 = LZERO;
void *p11 = (enum e3) 0;
#ifdef __BITINT_MAXWIDTH__
void *p12 = 0wb;
void *p13 = 0uwb;
#endif

void f (void *);

void *
g (void)
{
  p1 = 0;
  p2 = 0LL;
  p3 = (char) 0;
  p4 = 0UL;
  p5 = (bool) 0;
  p6 = (enum e) ZERO;
  p7 = false;
  p8 = BZERO;
  p9 = (enum e2) 0;
  p10 = LZERO;
  p11 = (enum e3) 0;
#ifdef __BITINT_MAXWIDTH__
  p12 = 0wb;
  p13 = 0uwb;
#endif
  f (0);
  f (0ULL);
  f (0L);
  f ((char) 0);
  f ((bool) 0);
  f ((enum e) ZERO);
  f (false);
  f (BZERO);
  f ((enum e2) 0);
  f (LZERO);
  f ((enum e3) 0);
#ifdef __BITINT_MAXWIDTH__
  f (0wb);
  f (0uwb);
#endif
  (1 ? p1 : 0);
  (1 ? p1 : 0L);
  (1 ? p1 : 0ULL);
  (1 ? p1 : (char) 0);
  (1 ? p1 : (bool) 0);
  (1 ? p1 : (enum e) 0);
  (1 ? p1 : false);
  (1 ? p1 : BZERO);
  (1 ? p1 : (enum e2) 0);
  (1 ? p1 : LZERO);
  (1 ? p1 : (enum e3) 0);
#ifdef __BITINT_MAXWIDTH__
  (1 ? p1 : 0wb);
  (1 ? p1 : 0uwb);
#endif
  p1 == 0;
  p1 == 0LL;
  p1 == 0U;
  p1 == (char) 0;
  p1 == (bool) 0;
  p1 == (enum e) 0;
  p1 == false;
  p1 == BZERO;
  p1 == (enum e2) 0;
  p1 == LZERO;
  p1 == (enum e3) 0;
#ifdef __BITINT_MAXWIDTH__
  p1 == 0wb;
  p1 == 0uwb;
#endif
  p1 != 0;
  p1 != 0LL;
  p1 != 0U;
  p1 != (char) 0;
  p1 != (bool) 0;
  p1 != (enum e) 0;
  p1 != false;
  p1 != BZERO;
  p1 != (enum e2) 0;
  p1 != LZERO;
  p1 != (enum e3) 0;
#ifdef __BITINT_MAXWIDTH__
  p1 != 0wb;
  p1 != 0uwb;
#endif
  return 0;
  return 0UL;
  return 0LL;
  return (char) 0;
  return (bool) 0;
  return (enum e) 0;
  return false;
  return BZERO;
  return (enum e2) 0;
  return LZERO;
  return (enum e3) 0;
#ifdef __BITINT_MAXWIDTH__
  return 0wb;
  return 0uwb;
#endif
}
