/* { dg-do compile } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -ftree-vectorize -mssse3 -mtune=slm" } */
#define byte unsigned char

void
pair_mul_sum(byte *in, byte *out, int size)
{
  int j;
  for(j = 0; j < size; j++)
    {
      byte a = in[0];
      byte b = in[1];
      byte c = in[2];
      byte d = in[3];
      out[0] = (byte)(a * b) + (byte)(b * c) + (byte)(c * d) + (byte)(d * a);
      in += 4;
      out += 1;
    }
}

/* { dg-final { scan-assembler "perm2i128|palignr" } } */
