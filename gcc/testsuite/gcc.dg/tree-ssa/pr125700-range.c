/* PR tree-optimization/125700  */
/* Test that the optimization fires using Ranger-derived value ranges,
   including ranges from widened types and range-limiting operations.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Unsigned char widened to unsigned int.  */
unsigned
fumin_char_range (unsigned char a, unsigned char b)
{
  unsigned aa = a;
  unsigned bb = b;
  if (aa == 255)
    return bb;
  return aa < bb ? aa : bb;
}

unsigned
fumax_char_range (unsigned char a, unsigned char b)
{
  unsigned aa = a;
  unsigned bb = b;
  if (aa == 0)
    return bb;
  return aa > bb ? aa : bb;
}

/* Signed char widened to int.  */
int
fsmin_char_range (signed char a, signed char b)
{
  int aa = a;
  int bb = b;
  if (aa == 127)
    return bb;
  return aa < bb ? aa : bb;
}

int
fsmax_char_range (signed char a, signed char b)
{
  int aa = a;
  int bb = b;
  if (aa == -128)
    return bb;
  return aa > bb ? aa : bb;
}

/* C is strictly above the upper bound of y's value range for MIN,
   and strictly below the lower bound for MAX.  */
unsigned
fumin_range_above_bound (unsigned x, unsigned y)
{
  if (y > 100)
    y = 100;
  if (x == 120)
    return y;
  return x < y ? x : y;
}

unsigned
fumax_range_below_bound (unsigned x, unsigned y)
{
  y &= 127;
  if (y < 27)
    y = 27;
  if (x == 10)
    return y;
  return x > y ? x : y;
}

int
fsmin_range_above_bound (int x, int y)
{
  y &= 127;
  if (y > 100)
    y = 100;
  if (x == 120)
    return y;
  return x < y ? x : y;
}

int
fsmax_range_below_bound (int x, int y)
{
  y &= 127;
  if (y < 27)
    y = 27;
  if (x == 10)
    return y;
  return x > y ? x : y;
}

/* C is equal to the upper bound of y's value range for MIN,
   and equal to the lower bound for MAX.  */
unsigned
fumin_range_at_bound (unsigned x, unsigned y)
{
  y &= 127;
  if (y > 100)
    y = 100;
  if (x == 100)
    return y;
  return x < y ? x : y;
}

unsigned
fumax_range_at_bound (unsigned x, unsigned y)
{
  y &= 127;
  if (y < 27)
    y = 27;
  if (x == 27)
    return y;
  return x > y ? x : y;
}

int
fsmin_range_at_bound (int x, int y)
{
  y &= 127;
  if (y > 100)
    y = 100;
  if (x == 100)
    return y;
  return x < y ? x : y;
}

int
fsmax_range_at_bound (int x, int y)
{
  y &= 127;
  if (y < 27)
    y = 27;
  if (x == 27)
    return y;
  return x > y ? x : y;
}

/* { dg-final { scan-tree-dump-not {if \(} "optimized" } } */
/* { dg-final { scan-tree-dump-not " == " "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 10 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 10 "optimized" } } */
