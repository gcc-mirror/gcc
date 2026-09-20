/* PR tree-optimization/125700  */
/* Negative test cases, should not transform.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

unsigned
fumin_negative (unsigned x, unsigned y)
{
  if (x == 0)
    return y;
  return x < y ? x : y;
}

unsigned
fumax_negative (unsigned x, unsigned y)
{
  if (x == -1u)
    return y;
  return x > y ? x : y;
}

signed
fsmin_negative (signed x, signed y)
{
  if (x == -__INT_MAX__ - 1)
    return y;
  return x < y ? x : y;
}

signed
fsmax_negative (signed x, signed y)
{
  if (x == __INT_MAX__)
    return y;
  return x > y ? x : y;
}

/* Unsigned char widened to unsigned int.  */
unsigned
fumin_range_negative (unsigned char a, unsigned int b)
{
  unsigned aa = a;
  if (aa == 255)
    return b;
  return aa < b ? aa : b;
}

/* Signed char widened to int.  */
int
fsmin_range_negative (signed char a, signed int b)
{
  int aa = a;
  if (aa == 127)
    return b;
  return aa < b ? aa : b;
}

int
fsmax_range_negative (signed char a, signed int b)
{
  int aa = a;
  if (aa == -128)
    return b;
  return aa > b ? aa : b;
}

/* None of the if and '==' comparisons should be eliminated.  */
/* { dg-final { scan-tree-dump-times {if \(} 7 "optimized" } } */
/* { dg-final { scan-tree-dump-times " == " 7 "optimized" } } */
