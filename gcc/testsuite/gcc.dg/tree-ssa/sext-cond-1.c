/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-phiopt2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef unsigned short u16;

/* if (x & 0x80) x -= 0x100; is (signed char) x, and the sign test is
   already materialised for the branch.  */
int
sext8 (u8 x)
{
  unsigned e = x;
  if (e & 0x80)
    e -= 0x100;
  return (int) e;
}

int
sext16 (u16 x)
{
  unsigned e = x;
  if (e & 0x8000)
    e -= 0x10000;
  return (int) e;
}

/* { dg-final { scan-tree-dump-not "if \\(" "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "PHI <" "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "if \\(" "optimized" } } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */
