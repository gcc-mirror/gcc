/* { dg-do compile } */

extern "C" double pow(double, double);
template <typename T>
T pow(T x, unsigned int n)
{
  if (!n)
    return 1;

  T y = 1;
  while (n > 1)
    {
      if (n%2)
	y *= x;
      x = x*x;
      n /= 2;
    }
  return x*y;
}

void testVec(int* x)
{
  for (int i = 0; i < 8; ++i)
    x[i] = pow(x[i], 10);
}

/* { dg-final { scan-tree-dump "OUTER LOOP VECTORIZED" "vect" { target { vect_double && vect_hw_misalign } } } } */
