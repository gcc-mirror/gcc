/* PR tree-optimization/125700  */
/* Test the original cases, alternative condition forms and reversed
   minimum/maximum operand orders.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Base test cases.  */
unsigned
fumin (unsigned x, unsigned y)
{
  if (x == -1u)
    return y;
  return x < y ? x : y;
}

signed
fsmin (signed x, signed y)
{
  if (x == __INT_MAX__)
    return y;
  return x < y ? x : y;
}

unsigned
fumax (unsigned x, unsigned y)
{
  if (x == 0)
    return y;
  return x > y ? x : y;
}

signed
fsmax (signed x, signed y)
{
  if (x == -__INT_MAX__ - 1)
    return y;
  return x > y ? x : y;
}

/* Conditional operator variants.  */
unsigned
fumin_ne (unsigned x, unsigned y)
{
  if (x != -1u)
    return x < y ? x : y;
  return y;
}

unsigned
fumax_ne (unsigned x, unsigned y)
{
  if (x != 0)
    return x > y ? x : y;
  return y;
}

unsigned
fumin_ge (unsigned x, unsigned y)
{
  if (x >= -1u)
    return y;
  return x < y ? x : y;
}

unsigned
fumax_le (unsigned x, unsigned y)
{
  if (x <= 0)
    return y;
  return x > y ? x : y;
}

unsigned
fumin_lt (unsigned x, unsigned y)
{
  if (!(x < -1u))
    return y;
  return x < y ? x : y;
}

unsigned
fumax_gt (unsigned x, unsigned y)
{
  if (!(x > 0))
    return y;
  return x > y ? x : y;
}

/* Commutativity tests for min/max operator.  */
unsigned
fumin_flip (unsigned x, unsigned y)
{
  if (x == -1u)
    return y;
  return y > x ? x : y;
}

unsigned
fumax_flip (unsigned x, unsigned y)
{
  if (x == 0)
    return y;
  return y < x ? x : y;
}

/* The if and all conditional operators should be removed
   from the optimized dump output.  */
/* { dg-final { scan-tree-dump-not {if \(} "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-not " != " "optimized" } } */
/* { dg-final { scan-tree-dump-not " <= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " >= " "optimized" } } */
/* { dg-final { scan-tree-dump-not " < " "optimized" } } */
/* { dg-final { scan-tree-dump-not " > " "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 6 "optimized" } } */
