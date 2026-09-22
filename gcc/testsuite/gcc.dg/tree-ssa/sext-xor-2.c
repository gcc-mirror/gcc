/* { dg-do compile } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

typedef unsigned char u8;
typedef signed char s8;

/* An unknown x: (x ^ 128) - 128 is not a sign-extension.  */
int
no_sext_unknown (int x)
{
  return (x ^ 0x80) - 0x80;
}

/* The xor is done in a signed type of precision 8, so the conversion
   sign-extends it and the result is not (signed char) x:  x = 1 gives
   -255.  */
int
no_sext_signed_xor (s8 x)
{
  return (int) (s8) (x ^ (s8) 0x80) - 128;
}

/* The same, with a range that does fit 8 bits: here nonzero_bits does not
   refuse, only the sign of the xor constant does.  For x in [0, 63] the
   value is x - 256.  */
int
no_sext_signed_xor_ranged (int a)
{
  s8 x = (s8) (a & 0x3f);
  s8 t = x ^ (s8) 0x80;
  return (int) t - 128;
}

/* The two constants do not pair as 2^(N-1) and -2^(N-1).  */
int
no_sext_bad_pair (u8 x)
{
  return (int) ((x ^ 0x80u) - 0x40u);
}

/* Not a narrowing: the result type is 8 bits wide itself.  */
int
no_sext_not_narrowing (u8 x)
{
  return (int) (u8) ((x ^ 0x80u) - 0x80u);
}

/* { dg-final { scan-tree-dump-times " \\^ " 5 "optimized" } } */
