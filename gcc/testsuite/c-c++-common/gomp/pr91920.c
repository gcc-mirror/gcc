/* PR middle-end/91920 */

void bar (float *);

void
foo (void)
{
  int i;
  float f[3] = { 0.0f, 0.0f, 0.0f };
#pragma omp parallel for default(none) reduction(+:f[:3])
  for (i = 0; i < 1000; i++)
    {
      int j;
      float k[3] = { 0.25f, 0.5f, 0.75f };
      for (j = 0; j < 3; j++)
	f[j] += k[j];
    }
  bar (f);
}
