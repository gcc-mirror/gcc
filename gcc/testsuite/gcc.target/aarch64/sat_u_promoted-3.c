/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;

/* Keep the outer clamp live so that the narrowing rules do not fire.  */

u8
sub8_result_shared (u8 a, u8 b, int *out)
{
  int t = a - b;
  int r = t < 0 ? 0 : t;
  *out = r;
  return r;
}

u8
add8_result_shared (u8 a, u8 b, int *out)
{
  int t = a + b;
  int r = t > 255 ? 255 : t;
  *out = r;
  return r;
}

/* { dg-final { scan-tree-dump-not "\\.SAT_" "optimized" } } */
