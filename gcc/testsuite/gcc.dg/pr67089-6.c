/* PR target/67089 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-if-convert -fdump-tree-widening_mul" } */

extern void abort (void);

int cnt;
unsigned int a[16], b[16], c[16], d;
void foo (int x);

__attribute__((noinline, noclone)) void
f0 (unsigned int x)
{
  for (int i = 0; i < 16; i++)
    {
      unsigned int r = x - a[i];
      b[i] = r;
      c[i] = r > x ? 7 : x;
    }
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
T (5, unsigned int, x - y, if (d || r > x) foo (0))
T (6, unsigned long, x - y, if (d || r <= x) foo (0))
T (7, unsigned char, x - y, if (d || x < r) foo (0))
T (8, unsigned long long, x - y, if (d || x >= r) foo (0))
T (9, unsigned int, x + y, if (r >= x) foo (0))
T (10, unsigned long, x + y, if (r < x) foo (0))
T (11, unsigned short, x + y, if (x <= r) foo (r))
T (12, unsigned long long, x + y, if (d || x > r) foo (0))
T (13, unsigned int, x + y, if (d || r >= x) foo (0))
T (14, unsigned long, x + y, if (d || r < x) foo (0))
T (15, unsigned short, x + y, if (d || x <= r) foo (0))
T (16, unsigned long long, x + y, if (d || x > r) foo (0))
T (17, unsigned int, x + y, if (r >= y) foo (0))
T (18, unsigned long, x + y, if (r < y) foo (0))
T (19, unsigned short, x + y, if (y <= r) foo (r))
T (20, unsigned long long, x + y, if (d || y > r) foo (0))
T (21, unsigned int, x + y, if (d || r >= y) foo (0))
T (22, unsigned long, x + y, if (d || r < y) foo (0))
T (23, unsigned short, x + y, if (d || y <= r) foo (0))
T (24, unsigned long long, x + y, if (d || y > r) foo (0))
T (25, unsigned short, 2U - x, if (r > 2U) foo (0))
T (26, unsigned char, 2U - x, if (r <= 2U) foo (0))

/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 16 "widening_mul" { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } } */
/* { dg-final { scan-tree-dump-times "SUB_OVERFLOW" 11 "widening_mul" { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } } */
/* { dg-final { scan-tree-dump-times "ADD_OVERFLOW" 12 "widening_mul" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
/* { dg-final { scan-tree-dump-times "SUB_OVERFLOW" 9 "widening_mul" { target { { i?86-*-* x86_64-*-* } && ia32 } } } } */
