/* { dg-do compile } */
/* { dg-options "-O -ftree-vectorize -mavx512bw -march=slm" } */

unsigned short out2[128 * 8], b0, b1, b2, b3, b4, b5, b6, b7, b8;

void
foo (unsigned short a0, unsigned short a1, unsigned short a2,
     unsigned short a3, unsigned short a4, unsigned short a5,
     unsigned short a6, unsigned short a7, unsigned short a8)
{
  int i;
  for (i = 0; i < 128; i++)
    {
      out2[i * 4] = a0 + 8;
    }
  for (i = 0; i < 128; i++)
    {
      b0 = a0 + 8;
      b1 = a1 + 7;
      b2 = a2 + 6;
      b3 = a3 + 5;
      b4 = a4 + 4;
      b5 = a5 + 3;
      b6 = a6 + 2;
      b7 = a7 + 1;
      b8 = a8 + 9;

      out2[i * 8] = b0;
      out2[i * 8 + 1] = b1;
      out2[i * 8 + 2] = b4;
      out2[i * 8 + 3] = b5;
      out2[i * 8 + 4] = b6;
      out2[i * 8 + 5] = b2;
      out2[i * 8 + 6] = b7;
      out2[i * 8 + 7] = b8;
    }
}
