/* PR target/127020.  */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -march=x86-64-v2" } */

extern __int128 a, b, c, z;

__int128
func (__int128 x)
{
  return (x ^ a ^ b ^ c ^ z);
}

/* { dg-final { scan-assembler-times "pxor" 4 } } */
/* { dg-final { scan-assembler-times "pinsrq" 1 } } */
/* { dg-final { scan-assembler-times "pextrq" 1 } } */
