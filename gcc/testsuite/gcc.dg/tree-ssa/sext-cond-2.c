/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef signed char s8;

/* An unknown x: 0x100 is not the weight of x's bit 7.  */
int
no_sext_unknown (int x)
{
  int e = x;
  if (e & 0x80)
    e -= 0x100;
  return e;
}

/* The wrong constant: 0x80 is not 2^8.  */
int
no_sext_wrong_const (u8 x)
{
  int e = x;
  if ((s8) x < 0)
    e -= 0x80;
  return e;
}

/* The arms are not the tested value.  */
int
no_sext_other_value (u8 x, u8 y)
{
  int e = y;
  if ((s8) x < 0)
    e -= 0x100;
  return e;
}

/* { dg-final { scan-tree-dump-times "if \\(" 3 "optimized" } } */
