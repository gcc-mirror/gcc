/* Ensure 32-bit ALU instructions are not generated if -malu32 is
   not enabled.  */

/* { dg-do compile } */
/* { dg-options "-mno-alu32 -masm=normal" } */

int foo (int a, int b)
{
  a += 1;
  b += a;
  b -= 5;
  a -= a;

  a *= 2;
  b *= a;

  a |= 0xfafa;
  b |= a;
  b &= 0x00ffff00;
  b &= a;

  a <<= 2;
  b <<= a;
  b >>= 5;
  a >>= b;

  int c = a;
  int d = 5;

  d ^= a;
  c ^= 0xe5e5e5e5;
  c = -c;

  unsigned int x = a;
  unsigned int y = b;
  x /= 3;
  y /= x;
  x %= 4;
  y %= x;

  return a + b - c + d - x + y;
}

/* { dg-final { scan-assembler-times "mov32\t0" 0 } } */
/* { dg-final { scan-assembler-times "add32\t0" 0 } } */
/* { dg-final { scan-assembler-times "sub32\t0" 0 } } */
/* { dg-final { scan-assembler-times "mul32\t0" 0 } } */
/* { dg-final { scan-assembler-times "div32\t0" 0 } } */
/* { dg-final { scan-assembler-times "mod32\t0" 0 } } */
/* { dg-final { scan-assembler-times "neg32\t0" 0 } } */
/* { dg-final { scan-assembler-times "and32\t0" 0 } } */
/* { dg-final { scan-assembler-times "or32\t0" 0 } } */
/* { dg-final { scan-assembler-times "xor32\t0" 0 } } */
/* { dg-final { scan-assembler-times "rsh32\t0" 0 } } */
/* { dg-final { scan-assembler-times "lsh32\t0" 0 } } */
/* { dg-final { scan-assembler-times "arsh32\t0" 0 } } */
