/* { dg-do compile } */

void FFT(_Complex *X, int length)
{
  unsigned i, j;
  for (; i < length; i++)
    {
      X[i] = 0;
      for (j = 0; j < length; j++)
	X[i] = X[i] / length;
    }
}
