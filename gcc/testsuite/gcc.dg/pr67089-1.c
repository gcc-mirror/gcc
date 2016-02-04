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

T (1, unsigned int, x - y, if (r > x) foo (0))
T (2, unsigned long, x - y, if (r <= x) foo (0))
T (3, unsigned short, x - y, if (x < r) foo (r))
T (4, unsigned long long, x - y, if (x >= r) foo (0))
T (5, unsigned int, x - y, if (r >= x) foo (0))
T (6, unsigned long, x - y, if (r < x) foo (0))
T (7, unsigned short, x - y, if (x <= r) foo (r))
T (8, unsigned long long, x - y, if (d || x > r) foo (0))
T (9, unsigned int, x - y, if (d || r > x) foo (0))
T (10, unsigned long, x - y, if (d || r <= x) foo (0))
T (11, unsigned char, x - y, if (d || x < r) foo (0))
T (12, unsigned long long, x - y, if (d || x >= r) foo (0))
T (13, unsigned int, x - y, if (d || r >= x) foo (0))
T (14, unsigned long, x - y, if (d || r < x) foo (0))
T (15, unsigned short, x - y, if (d || x <= r) foo (0))
T (16, unsigned long long, x - y, if (d || x > r) foo (0))

int
main ()
{
  if (f1 (5, 3) != 2U || cnt != 0) abort ();
  if (f1 (5, 7) != -2U || cnt != 1) abort ();
  if (f1 (5, 5) != 0U || cnt != 1) abort ();
  if (f1 (5, 0) != 5U || cnt != 1) abort ();
  if (f2 (7, 1) != 6UL || cnt != 2) abort ();
  if (f2 (7, 8) != -1UL || cnt != 2) abort ();
  if (f2 (9, 9) != 0UL || cnt != 3) abort ();
  if (f2 (9, 0) != 9UL || cnt != 4) abort ();
  if (f3 (15, 14) != 1 || cnt != 4) abort ();
  if (f3 (15, 25) != (unsigned short) -10 || cnt != 5) abort ();
  if (f3 (15, 15) != 0 || cnt != 5) abort ();
  if (f3 (15, 0) != 15 || cnt != 5) abort ();
  if (f4 (9132, 9127) != 5ULL || cnt != 6) abort ();
  if (f4 (9132, 9137) != -5ULL || cnt != 6) abort ();
  if (f4 (9132, 9132) != 0 || cnt != 7) abort ();
  if (f4 (9132, 0) != 9132ULL || cnt != 8) abort ();
  if (f5 (5, 3) != 2U || cnt != 8) abort ();
  if (f5 (5, 7) != -2U || cnt != 9) abort ();
  if (f5 (5, 5) != 0U || cnt != 9) abort ();
  if (f5 (5, 0) != 5U || cnt != 10) abort ();
  if (f6 (7, 1) != 6UL || cnt != 11) abort ();
  if (f6 (7, 8) != -1UL || cnt != 11) abort ();
  if (f6 (9, 9) != 0UL || cnt != 12) abort ();
  if (f6 (9, 0) != 9UL || cnt != 12) abort ();
  if (f7 (15, 14) != 1 || cnt != 12) abort ();
  if (f7 (15, 25) != (unsigned short) -10 || cnt != 13) abort ();
  if (f7 (15, 15) != 0 || cnt != 13) abort ();
  if (f7 (15, 0) != 15 || cnt != 14) abort ();
  if (f8 (9132, 9127) != 5ULL || cnt != 15) abort ();
  if (f8 (9132, 9137) != -5ULL || cnt != 15) abort ();
  if (f8 (9132, 9132) != 0 || cnt != 16) abort ();
  if (f8 (9132, 0) != 9132ULL || cnt != 16) abort ();
  cnt = 0;
  if (f9 (5, 3) != 2U || cnt != 0) abort ();
  if (f9 (5, 7) != -2U || cnt != 1) abort ();
  if (f9 (5, 5) != 0U || cnt != 1) abort ();
  if (f9 (5, 0) != 5U || cnt != 1) abort ();
  if (f10 (7, 1) != 6UL || cnt != 2) abort ();
  if (f10 (7, 8) != -1UL || cnt != 2) abort ();
  if (f10 (9, 9) != 0UL || cnt != 3) abort ();
  if (f10 (9, 0) != 9UL || cnt != 4) abort ();
  if (f11 (15, 14) != 1 || cnt != 4) abort ();
  if (f11 (15, 25) != (unsigned char) -10 || cnt != 5) abort ();
  if (f11 (15, 15) != 0 || cnt != 5) abort ();
  if (f11 (15, 0) != 15 || cnt != 5) abort ();
  if (f12 (9132, 9127) != 5ULL || cnt != 6) abort ();
  if (f12 (9132, 9137) != -5ULL || cnt != 6) abort ();
  if (f12 (9132, 9132) != 0 || cnt != 7) abort ();
  if (f12 (9132, 0) != 9132ULL || cnt != 8) abort ();
  if (f13 (5, 3) != 2U || cnt != 8) abort ();
  if (f13 (5, 7) != -2U || cnt != 9) abort ();
  if (f13 (5, 5) != 0U || cnt != 9) abort ();
  if (f13 (5, 0) != 5U || cnt != 10) abort ();
  if (f14 (7, 1) != 6UL || cnt != 11) abort ();
  if (f14 (7, 8) != -1UL || cnt != 11) abort ();
  if (f14 (9, 9) != 0UL || cnt != 12) abort ();
  if (f14 (9, 0) != 9UL || cnt != 12) abort ();
  if (f15 (15, 14) != 1 || cnt != 12) abort ();
  if (f15 (15, 25) != (unsigned short) -10 || cnt != 13) abort ();
  if (f15 (15, 15) != 0 || cnt != 13) abort ();
  if (f15 (15, 0) != 15 || cnt != 14) abort ();
  if (f16 (9132, 9127) != 5ULL || cnt != 15) abort ();
  if (f16 (9132, 9137) != -5ULL || cnt != 15) abort ();
  if (f16 (9132, 9132) != 0 || cnt != 16) abort ();
  if (f16 (9132, 0) != 9132ULL || cnt != 16) abort ();
  return 0;
}
