/* Test zero with different types as null pointer constant: bug 112556.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors -Wno-pointer-compare" } */

enum e { ZERO };

void *p1 = 0;
void *p2 = 0LL;
void *p3 = (char) 0;
void *p4 = 0UL;
void *p5 = (_Bool) 0;
void *p6 = (enum e) ZERO;

void f (void *);

void *
g (void)
{
  p1 = 0;
  p2 = 0LL;
  p3 = (char) 0;
  p4 = 0UL;
  p5 = (_Bool) 0;
  p6 = (enum e) ZERO;
  f (0);
  f (0ULL);
  f (0L);
  f ((char) 0);
  f ((_Bool) 0);
  f ((enum e) ZERO);
  (1 ? p1 : 0);
  (1 ? p1 : 0L);
  (1 ? p1 : 0ULL);
  (1 ? p1 : (char) 0);
  (1 ? p1 : (_Bool) 0);
  (1 ? p1 : (enum e) 0);
  p1 == 0;
  p1 == 0LL;
  p1 == 0U;
  p1 == (char) 0;
  p1 == (_Bool) 0;
  p1 == (enum e) 0;
  p1 != 0;
  p1 != 0LL;
  p1 != 0U;
  p1 != (char) 0;
  p1 != (_Bool) 0;
  p1 != (enum e) 0;
  return 0;
  return 0UL;
  return 0LL;
  return (char) 0;
  return (_Bool) 0;
  return (enum e) 0;
}
