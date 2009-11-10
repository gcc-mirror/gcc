/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern float fabsf(float);
extern float sqrtf(float);

static __attribute__((noinline)) void f (float *dst, float *src)
{
  int i, j;
  for (i = 0; i < 2; i++)
    {
      float len;
      dst[0] = src[0];
      dst[1] = src[1];
      len = sqrtf (dst[0] * dst[0] + dst[1] * dst[1]);
      if (len > 0.5f)
	{
	  len = 1.0f / len;
	  dst[0] *= len;
	  dst[1] *= len;
	}
    }
}

extern void abort (void);

int main()
{
  float dst[2], src[2];
  src[0] = 2.0f;
  src[1] = 5.0f;
  f (dst, src);
  if (fabsf (dst[0] * dst[0] + dst[1] * dst[1] - 1.0f) > 0.01f)
    abort ();
  return 0;
}
