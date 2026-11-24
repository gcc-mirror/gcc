/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

/* Transformed: (t * (C + 1)) / C -> t + t / C.  */

unsigned
plus_1 (unsigned short b)
{
  unsigned t = b;
  return t * 7 / 6;
}

int
signed_plus_1 (short b)
{
  int t = b;
  return t * 11 / 10;
}

/* Known negative range.  */
int
signed_plus_neg (short b)
{
  int t = b;
  if (t >= 0)
    return 0;
  return t * 25 / 24;
}

/* Signed division by 2^k is not turned into a shift.  */
int
signed_pow2 (short b)
{
  int t = b;
  return t * 9 / 8;
}

/* No range needed: signed overflow is undefined.  */
int
no_range_signed (int t)
{
  return t * 19 / 18;
}

/* Unsigned divisor with the sign bit set.  t * 0x80000003 only
   avoids overflow for t in [0, 1].  */
unsigned
large_c (unsigned t)
{
  if (t > 1)
    return 0;
  return t * 0x80000003u / 0x80000002u;
}

/* Transformed: (t * (2^k + 1)) >> k -> t + (t >> k).  */

/* Original PR reproducer: unsigned t / 2 has become t >> 1.  */
unsigned
pr126294 (unsigned short b)
{
  unsigned t = b;
  return t * 3 / 2;
}

/* Not transformed: (t * (C - 1)) / C with trunc_div.  */

unsigned
minus_1 (unsigned short b)
{
  unsigned t = b;
  return t * 12 / 13;
}

int
signed_minus_1 (short b)
{
  int t = b;
  return t * 16 / 17;
}

/* Known non-negative range.  */
int
signed_minus_nonneg (short b)
{
  int t = b;
  if (t < 0)
    return 0;
  return t * 14 / 15;
}

/* Not transformed: the multiplication may overflow.  */

unsigned
no_range_unsigned (unsigned t)
{
  return t * 20 / 19;
}

/* Range known but too wide.  */
unsigned
overflow_range (unsigned t)
{
  if (t > 0x7fffffff)
    return 0;
  return t * 23 / 22;
}

unsigned
no_range_shift (unsigned t)
{
  return t * 33 / 32;
}

/* Not transformed: constants not adjacent.  */

unsigned
no_transform (unsigned short b)
{
  unsigned t = b;
  return t * 5 / 3;
}

/* Not transformed: the multiplication has another use.  */

unsigned
multi_use_div (unsigned short b, unsigned *p)
{
  unsigned t = b;
  unsigned m = t * 29;
  *p = m;
  return m / 28;
}

/* Undefined-overflow path.  */
int
multi_use_signed (int t, int *p)
{
  int m = t * 31;
  *p = m;
  return m / 30;
}

/* Shift pattern.  */
unsigned
multi_use_shift (unsigned short b, unsigned *p)
{
  unsigned t = b;
  unsigned m = t * 65;
  *p = m;
  return m / 64;
}

/* Transformed: (t * (C + 1)) / C -> t + t / C.  */
/* { dg-final { scan-tree-dump-not "\\* 7;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 6;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 11;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 10;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 25;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 24;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 9;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 8;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 19;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 18;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 2147483651;" "optimized" } } */

/* Transformed: (t * (2^k + 1)) >> k -> t + (t >> k).  */
/* { dg-final { scan-tree-dump-not "\\* 3;" "optimized" } } */
/* { dg-final { scan-tree-dump " >> 1;" "optimized" } } */

/* Not transformed: (t * (C - 1)) / C with trunc_div.  */
/* { dg-final { scan-tree-dump "\\* 12;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 16;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 14;" "optimized" } } */

/* Not transformed: the multiplication may overflow.  */
/* { dg-final { scan-tree-dump "\\* 20;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 23;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 33;" "optimized" } } */

/* Not transformed: constants not adjacent.  */
/* { dg-final { scan-tree-dump "\\* 5;" "optimized" } } */

/* Not transformed: the multiplication has another use.  */
/* { dg-final { scan-tree-dump "\\* 29;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 31;" "optimized" } } */
/* { dg-final { scan-tree-dump "\\* 65;" "optimized" } } */
