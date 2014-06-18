/* { dg-do compile } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O2 -ftree-vectorize -mssse3 -mtune=slm" } */
#define byte unsigned char

void
matrix_mul (byte *in, byte *out, int size)
{
  int i;
  for (i = 0; i < size; i++)
    {
      byte in0 = in[0];
      byte in1 = in[1];
      byte in2 = in[2];
      byte out0, out1, out2, out3;
      out0 = in0 + in1;
      out1 = in0 + in2;
      out2 = in1 + in2;
      out3 = in0 + in1 + in2;
      out[0] = out0;
      out[1] = out1;
      out[2] = out2;
      out[3] = out3;
      in += 3;
      out += 4;
    }
}

/* { dg-final { scan-assembler "palignr" } } */
