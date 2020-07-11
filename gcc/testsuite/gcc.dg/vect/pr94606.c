/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve -msve-vector-bits=256" { target aarch64*-*-* } } */

const short mask[] = { 0, 0, 0, 0, 0, 0, 0, 0,
		       0, 0, 0, 1, 1, 1, 1, 1 };

int
foo (short *restrict x, short *restrict y)
{
  for (int i = 0; i < 16; ++i)
    if (mask[i])
      x[i] += y[i];
}
