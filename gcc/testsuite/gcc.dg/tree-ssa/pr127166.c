/* PR tree-optimization/127166 */
/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-gimple -fdump-tree-phiopt2" } */

unsigned short
signed_clip (int x)
{
  return ((unsigned int) x > 65535u ? (-x) >> 31 : x);
}

unsigned short
unsigned_clip (int x)
{
  return ((unsigned int) x > 65535u
	  ? (int) (-(unsigned int) x) >> 31 : x);
}

unsigned short
signed_split_clip (int x)
{
  int neg = -x;
  int sign = neg >> 31;
  unsigned short high = sign;

  return ((unsigned int) x > 65535u ? high : x);
}

unsigned short
unsigned_split_clip (int x)
{
  unsigned int ux = x;
  unsigned int neg = -ux;
  int sign = (int) neg >> 31;
  unsigned short high = sign;

  return (ux > 65535u ? high : x);
}

volatile int v;

unsigned short
volatile_clip (void)
{
  return ((unsigned int) v > 65535u ? (-v) >> 31 : v);
}

/* The direct signed form is canonicalized in GENERIC.  The split signed form
   is canonicalized in GIMPLE before loop if-conversion.  */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "phiopt2" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "phiopt2" } } */
