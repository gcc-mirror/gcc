/* { dg-require-effective-target int32plus } */

/* Test arithmetics on bitfields.  */
#ifndef T

extern void abort (void);
extern void exit (int);

#ifndef FIELDS1
#define FIELDS1
#endif
#ifndef FIELDS2
#define FIELDS2
#endif

struct { FIELDS1 unsigned int i : 6, j : 11, k : 15; FIELDS2 } b;
struct { FIELDS1 unsigned int i : 5, j : 1, k : 26; FIELDS2 } c;
struct { FIELDS1 unsigned int i : 16, j : 8, k : 8; FIELDS2 } d;

unsigned int ret1 (void) { return b.i; }
unsigned int ret2 (void) { return b.j; }
unsigned int ret3 (void) { return b.k; }
unsigned int ret4 (void) { return c.i; }
unsigned int ret5 (void) { return c.j; }
unsigned int ret6 (void) { return c.k; }
unsigned int ret7 (void) { return d.i; }
unsigned int ret8 (void) { return d.j; }
unsigned int ret9 (void) { return d.k; }

#define T(n, pre, post, op) 					\
void fn1_##n (unsigned int x) { pre b.i post; }			\
void fn2_##n (unsigned int x) { pre b.j post; }			\
void fn3_##n (unsigned int x) { pre b.k post; }			\
void fn4_##n (unsigned int x) { pre c.i post; }			\
void fn5_##n (unsigned int x) { pre c.j post; }			\
void fn6_##n (unsigned int x) { pre c.k post; }			\
void fn7_##n (unsigned int x) { pre d.i post; }			\
void fn8_##n (unsigned int x) { pre d.j post; }			\
void fn9_##n (unsigned int x) { pre d.k post; }

#include "20040629-1.c"
#undef T

#define FAIL(n, i) abort ()

int
main (void)
{
#define T(n, pre, post, op)					\
  b.i = 51;							\
  b.j = 636;							\
  b.k = 31278;							\
  c.i = 21;							\
  c.j = 1;							\
  c.k = 33554432;						\
  d.i = 26812;							\
  d.j = 156;							\
  d.k = 187;							\
  fn1_##n (3);							\
  if (ret1 () != (op (51, 3) & ((1 << 6) - 1)))			\
    FAIL (n, 1);						\
  b.i = 51;							\
  fn2_##n (251);						\
  if (ret2 () != (op (636, 251) & ((1 << 11) - 1)))		\
    FAIL (n, 2);						\
  b.j = 636;							\
  fn3_##n (13279);						\
  if (ret3 () != (op (31278, 13279) & ((1 << 15) - 1)))		\
    FAIL (n, 3);						\
  b.j = 31278;							\
  fn4_##n (24);							\
  if (ret4 () != (op (21, 24) & ((1 << 5) - 1)))		\
    FAIL (n, 4);						\
  c.i = 21;							\
  fn5_##n (1);							\
  if (ret5 () != (op (1, 1) & ((1 << 1) - 1)))			\
    FAIL (n, 5);						\
  c.j = 1;							\
  fn6_##n (264151);						\
  if (ret6 () != (op (33554432, 264151) & ((1 << 26) - 1)))	\
    FAIL (n, 6);						\
  c.k = 33554432;						\
  fn7_##n (713);						\
  if (ret7 () != (op (26812, 713) & ((1 << 16) - 1)))		\
    FAIL (n, 7);						\
  d.i = 26812;							\
  fn8_##n (17);							\
  if (ret8 () != (op (156, 17) & ((1 << 8) - 1)))		\
    FAIL (n, 8);						\
  d.j = 156;							\
  fn9_##n (199);						\
  if (ret9 () != (op (187, 199) & ((1 << 8) - 1)))		\
    FAIL (n, 9);						\
  d.k = 187;

#include "20040629-1.c"
#undef T
  return 0;
}

#else

#ifndef opadd
#define opadd(x, y) (x + y)
#define opsub(x, y) (x - y)
#define opinc(x, y) (x + 1)
#define opdec(x, y) (x - 1)
#define opand(x, y) (x & y)
#define opior(x, y) (x | y)
#define opxor(x, y) (x ^ y)
#define opdiv(x, y) (x / y)
#define oprem(x, y) (x % y)
#define opadd3(x, y) (x + 3)
#define opsub7(x, y) (x - 7)
#define opand21(x, y) (x & 21)
#define opior19(x, y) (x | 19)
#define opxor37(x, y) (x ^ 37)
#define opdiv17(x, y) (x / 17)
#define oprem19(x, y) (x % 19)
#endif

T(1, , += x, opadd)
T(2, ++, , opinc)
T(3, , ++, opinc)
T(4, , -= x, opsub)
T(5, --, , opdec)
T(6, , --, opdec)
T(7, , &= x, opand)
T(8, , |= x, opior)
T(9, , ^= x, opxor)
T(a, , /= x, opdiv)
T(b, , %= x, oprem)
T(c, , += 3, opadd3)
T(d, , -= 7, opsub7)
T(e, , &= 21, opand21)
T(f, , |= 19, opior19)
T(g, , ^= 37, opxor37)
T(h, , /= 17, opdiv17)
T(i, , %= 19, oprem19)

#endif
