/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;

/* The narrow type supplies the range, so nonzero_bits has to be consulted:
   the PR125405 rule needs both masks in the IL.  */
int
sext8 (u8 x)
{
  return (int) ((x ^ 0x80u) - 0x80u);
}

int
sext16 (u16 x)
{
  return (int) ((x ^ 0x8000u) - 0x8000u);
}

long
sext8_from_mask (unsigned long x)
{
  unsigned long e = x & 0xff;
  return (long) ((e ^ 0x80UL) - 0x80UL);
}

/* { dg-final { scan-tree-dump-times "\\(signed char\\)" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\(signed short\\)" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\(signed char\\)" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\(signed short\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\^ " "optimized" } } */
