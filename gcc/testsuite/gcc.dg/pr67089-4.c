/* PR target/67089 */
/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort (void);

int cnt, d;

__attribute__((noinline, noclone))
void foo (int x)
{
  asm volatile ("" : "+m" (d) : "g" (x) : "memory");
  cnt++;
}

#define T(n, type, op, cond) \
__attribute__((noinline, noclone))	\
type					\
f##n (type x, type y)			\
{					\
  type r = op;				\
  cond;					\
  return r;				\
}

T (1, unsigned int, x + y, if (r > y) foo (0))
T (2, unsigned long, x + y, if (r <= y) foo (0))
T (3, unsigned short, x + y, if (y < r) foo (r))
T (4, unsigned long long, x + y, if (y >= r) foo (0))
T (5, unsigned int, x + y, if (r >= y) foo (0))
T (6, unsigned long, x + y, if (r < y) foo (0))
T (7, unsigned short, x + y, if (y <= r) foo (r))
T (8, unsigned long long, x + y, if (d || y > r) foo (0))
T (9, unsigned int, x + y, if (d || r > y) foo (0))
T (10, unsigned long, x + y, if (d || r <= y) foo (0))
T (11, unsigned char, x + y, if (d || y < r) foo (0))
T (12, unsigned long long, x + y, if (d || y >= r) foo (0))
T (13, unsigned int, x + y, if (d || r >= y) foo (0))
T (14, unsigned long, x + y, if (d || r < y) foo (0))
T (15, unsigned short, x + y, if (d || y <= r) foo (0))
T (16, unsigned long long, x + y, if (d || y > r) foo (0))

int
main ()
{
  if (f1 (-7U, 0) != -7U || cnt != 1) abort ();
  if (f1 (-7U, 6) != -1U || cnt != 2) abort ();
  if (f1 (-7U, 7) != 0U || cnt != 2) abort ();
  if (f1 (-7U, 8) != 1U || cnt != 2) abort ();
  if (f2 (-9UL, 0) != -9UL || cnt != 2) abort ();
  if (f2 (-9UL, 8) != -1UL || cnt != 2) abort ();
  if (f2 (-9UL, 9) != 0UL || cnt != 3) abort ();
  if (f2 (-9UL, 10) != 1UL || cnt != 4) abort ();
  if (f3 (-15, 0) != (unsigned short) -15 || cnt != 5) abort ();
  if (f3 (-15, 14) != (unsigned short) -1 || cnt != 6) abort ();
  if (f3 (-15, 15) != 0 || cnt != 6) abort ();
  if (f3 (-15, 16) != 1 || cnt != 6) abort ();
  if (f4 (-9132ULL, 0) != -9132ULL || cnt != 6) abort ();
  if (f4 (-9132ULL, 9131) != -1ULL || cnt != 6) abort ();
  if (f4 (-9132ULL, 9132) != 0 || cnt != 7) abort ();
  if (f4 (-9132ULL, 9133) != 1ULL || cnt != 8) abort ();
  if (f5 (-7U, 0) != -7U || cnt != 9) abort ();
  if (f5 (-7U, 6) != -1U || cnt != 10) abort ();
  if (f5 (-7U, 7) != 0U || cnt != 10) abort ();
  if (f5 (-7U, 8) != 1U || cnt != 10) abort ();
  if (f6 (-9UL, 0) != -9UL || cnt != 10) abort ();
  if (f6 (-9UL, 8) != -1UL || cnt != 10) abort ();
  if (f6 (-9UL, 9) != 0UL || cnt != 11) abort ();
  if (f6 (-9UL, 10) != 1UL || cnt != 12) abort ();
  if (f7 (-15, 0) != (unsigned short) -15 || cnt != 13) abort ();
  if (f7 (-15, 14) != (unsigned short) -1 || cnt != 14) abort ();
  if (f7 (-15, 15) != 0 || cnt != 14) abort ();
  if (f7 (-15, 16) != 1 || cnt != 14) abort ();
  if (f8 (-9132ULL, 0) != -9132ULL || cnt != 14) abort ();
  if (f8 (-9132ULL, 9131) != -1ULL || cnt != 14) abort ();
  if (f8 (-9132ULL, 9132) != 0 || cnt != 15) abort ();
  if (f8 (-9132ULL, 9133) != 1ULL || cnt != 16) abort ();
  cnt = 0;
  if (f9 (-7U, 0) != -7U || cnt != 1) abort ();
  if (f9 (-7U, 6) != -1U || cnt != 2) abort ();
  if (f9 (-7U, 7) != 0U || cnt != 2) abort ();
  if (f9 (-7U, 8) != 1U || cnt != 2) abort ();
  if (f10 (-9UL, 0) != -9UL || cnt != 2) abort ();
  if (f10 (-9UL, 8) != -1UL || cnt != 2) abort ();
  if (f10 (-9UL, 9) != 0UL || cnt != 3) abort ();
  if (f10 (-9UL, 10) != 1UL || cnt != 4) abort ();
  if (f11 (-15, 0) != (unsigned char) -15 || cnt != 5) abort ();
  if (f11 (-15, 14) != (unsigned char) -1 || cnt != 6) abort ();
  if (f11 (-15, 15) != 0 || cnt != 6) abort ();
  if (f11 (-15, 16) != 1 || cnt != 6) abort ();
  if (f12 (-9132ULL, 0) != -9132ULL || cnt != 6) abort ();
  if (f12 (-9132ULL, 9131) != -1ULL || cnt != 6) abort ();
  if (f12 (-9132ULL, 9132) != 0 || cnt != 7) abort ();
  if (f12 (-9132ULL, 9133) != 1ULL || cnt != 8) abort ();
  if (f13 (-7U, 0) != -7U || cnt != 9) abort ();
  if (f13 (-7U, 6) != -1U || cnt != 10) abort ();
  if (f13 (-7U, 7) != 0U || cnt != 10) abort ();
  if (f13 (-7U, 8) != 1U || cnt != 10) abort ();
  if (f14 (-9UL, 0) != -9UL || cnt != 10) abort ();
  if (f14 (-9UL, 8) != -1UL || cnt != 10) abort ();
  if (f14 (-9UL, 9) != 0UL || cnt != 11) abort ();
  if (f14 (-9UL, 10) != 1UL || cnt != 12) abort ();
  if (f15 (-15, 0) != (unsigned short) -15 || cnt != 13) abort ();
  if (f15 (-15, 14) != (unsigned short) -1 || cnt != 14) abort ();
  if (f15 (-15, 15) != 0 || cnt != 14) abort ();
  if (f15 (-15, 16) != 1 || cnt != 14) abort ();
  if (f16 (-9132ULL, 0) != -9132ULL || cnt != 14) abort ();
  if (f16 (-9132ULL, 9131) != -1ULL || cnt != 14) abort ();
  if (f16 (-9132ULL, 9132) != 0 || cnt != 15) abort ();
  if (f16 (-9132ULL, 9133) != 1ULL || cnt != 16) abort ();
  return 0;
}
