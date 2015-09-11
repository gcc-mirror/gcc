/* { dg-do compile } */
/* { dg-additional-options "-mssse3" { target { i?86-*-* x86_64-*-* } } } */

#define byte unsigned char

void
matrix_mul (byte *in, byte *out, int size)
{
  int i;
  for (i = 0; i < size; i++)
    {
      out[0] = in[0] + in[1] + in[3];
      out[1] = in[0] + in[2] + in[4];
      out[2] = in[1] + in[2] + in[4];
      in += 4;
      out += 3;
    }
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" { target { i?86-*-* x86_64-*-* } } } } */
