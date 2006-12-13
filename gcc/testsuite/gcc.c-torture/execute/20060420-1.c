extern void abort (void);

typedef float v4flt __attribute__ ((vector_size (16)));

void __attribute__ ((noinline)) foo (float *dst, float **src, int a, int n)
{
  int i, j;
  int z = sizeof (v4flt) / sizeof (float);
  unsigned m = sizeof (v4flt) - 1;

  for (j = 0; j < n && (((unsigned long) dst + j) & m); ++j)
    {
      float t = src[0][j];
      for (i = 1; i < a; ++i)
	t += src[i][j];
      dst[j] = t;
    }

  for (; j < (n - (4 * z - 1)); j += 4 * z)
    {
      v4flt t0 = *(v4flt *) (src[0] + j + 0 * z);
      v4flt t1 = *(v4flt *) (src[0] + j + 1 * z);
      v4flt t2 = *(v4flt *) (src[0] + j + 2 * z);
      v4flt t3 = *(v4flt *) (src[0] + j + 3 * z);
      for (i = 1; i < a; ++i)
	{
	  t0 += *(v4flt *) (src[i] + j + 0 * z);
	  t1 += *(v4flt *) (src[i] + j + 1 * z);
	  t2 += *(v4flt *) (src[i] + j + 2 * z);
	  t3 += *(v4flt *) (src[i] + j + 3 * z);
	}
      *(v4flt *) (dst + j + 0 * z) = t0;
      *(v4flt *) (dst + j + 1 * z) = t1;
      *(v4flt *) (dst + j + 2 * z) = t2;
      *(v4flt *) (dst + j + 3 * z) = t3;
    }
  for (; j < n; ++j)
    {
      float t = src[0][j];
      for (i = 1; i < a; ++i)
	t += src[i][j];
      dst[j] = t;
    }
}

float buffer[64];

int
main (void)
{
  int i;
  float *dst, *src[2];

  dst = buffer;
  dst += (-(long int) buffer & (16 * sizeof (float) - 1)) / sizeof (float);
  src[0] = dst + 16;
  src[1] = dst + 32;
  for (i = 0; i < 16; ++i)
    {
      src[0][i] = (float) i + 11 * (float) i;
      src[1][i] = (float) i + 12 * (float) i;
    }
  foo (dst, src, 2, 16);
  for (i = 0; i < 16; ++i)
    {
      float e = (float) i + 11 * (float) i + (float) i + 12 * (float) i;
      if (dst[i] != e)
	abort ();
    }
  return 0;
}
