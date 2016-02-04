/* PR target/68483 */
/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msse2 -mno-sse3" } */

void
test (int *input, int *out, unsigned x1, unsigned x2)
{
  unsigned i, j;
  unsigned end = x1;

  for (i = j = 0; i < 1000; i++)
    {
      int sum = 0;
      end += x2;
      for (; j < end; j++)
	sum += input[j];
      out[i] = sum;
    }
}

/* { dg-final { scan-assembler "psrldq\[^\n\r]*(8,|, 8)" { target ia32 } } } */
/* { dg-final { scan-assembler "psrldq\[^\n\r]*(4,|, 4)" { target ia32 } } } */
