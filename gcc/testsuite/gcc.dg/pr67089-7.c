/* PR target/67089 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-if-convert -fdump-tree-widening_mul" } */

extern void abort (void);

int cnt, d;
void foo (int x);

#define T(n, type, op, cond) \
__attribute__((noinline, noclone))	\
type					\
f##n (type x, type y)			\
{					\
  type r = op;				\
  cond;					\
  return r;				\
}

T (1, unsigned int, x - y, if (r >= x) foo (0))
T (2, unsigned long, x - y, if (r < x) foo (0))
T (3, unsigned short, x - y, if (x <= r) foo (r))
T (4, unsigned long long, x - y, if (d || x > r) foo (0))
T (5, unsigned int, x - y, if (d || r >= x) foo (0))
T (6, unsigned long, x - y, if (d || r < x) foo (0))
T (7, unsigned short, x - y, if (d || x <= r) foo (0))
T (8, unsigned long long, x - y, if (d || x > r) foo (0))
T (9, unsigned int, x - y, if (r > y) foo (0))
T (10, unsigned long, x - y, if (r <= y) foo (0))
T (11, unsigned short, x - y, if (y < r) foo (r))
T (12, unsigned long long, x - y, if (y >= r) foo (0))
T (13, unsigned int, x - y, if (r >= y) foo (0))
T (14, unsigned long, x - y, if (r < y) foo (0))
T (15, unsigned short, x - y, if (y <= r) foo (r))
T (16, unsigned long long, x - y, if (d || y > r) foo (0))
T (17, unsigned int, x - y, if (d || r > y) foo (0))
T (18, unsigned long, x - y, if (d || r <= y) foo (0))
T (19, unsigned char, x - y, if (d || y < r) foo (0))
T (20, unsigned long long, x - y, if (d || y >= r) foo (0))
T (21, unsigned int, x - y, if (d || r >= y) foo (0))
T (22, unsigned long, x - y, if (d || r < y) foo (0))
T (23, unsigned short, x - y, if (d || y <= r) foo (0))
T (24, unsigned long long, x - y, if (d || y > r) foo (0))
T (25, unsigned int, x + y, if (r > x) foo (0))
T (26, unsigned long, x + y, if (r <= x) foo (0))
T (27, unsigned short, x + y, if (x < r) foo (r))
T (28, unsigned long long, x + y, if (x >= r) foo (0))
T (29, unsigned int, x + y, if (d || r > x) foo (0))
T (30, unsigned long, x + y, if (d || r <= x) foo (0))
T (31, unsigned char, x + y, if (d || x < r) foo (0))
T (32, unsigned long long, x + y, if (d || x >= r) foo (0))
T (33, unsigned int, x + y, if (r > y) foo (0))
T (34, unsigned long, x + y, if (r <= y) foo (0))
T (35, unsigned short, x + y, if (y < r) foo (r))
T (36, unsigned long long, x + y, if (y >= r) foo (0))
T (37, unsigned int, x + y, if (d || r > y) foo (0))
T (38, unsigned long, x + y, if (d || r <= y) foo (0))
T (39, unsigned char, x + y, if (d || y < r) foo (0))
T (40, unsigned long long, x + y, if (d || y >= r) foo (0))

/* { dg-final { scan-tree-dump-not "ADD_OVERFLOW" "widening_mul" } } */
/* { dg-final { scan-tree-dump-not "SUB_OVERFLOW" "widening_mul" } } */
