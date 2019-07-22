__attribute__((noipa)) int
foo (int *a, int *r3)
{
  int r = 0, r2[2] = { 0, 0 }, i;
  #pragma omp parallel loop default (none) reduction (+:r, r2[:2], r3[:2]) shared (a) lastprivate (i)
  for (i = 0; i < 1024; i++)
    {
      r += a[i];
      r2[0] += a[i];
      r3[1] += a[i];
    };
  return r + r2[0] + r3[1] + i;
}

__attribute__((noipa)) int
bar (int *a, int *r3)
{
  int r = 0, r2[2] = { 0, 0 }, i;
  #pragma omp target parallel loop default (none) reduction (+:r, r2[0:2], r3[0:2]) shared (a) lastprivate (i)
  for (i = 0; i < 1024; i++)
    {
      r += a[i];
      r2[1] += a[i];
      r3[0] += a[i];
    }
  return r + r2[1] + r3[0] + i;
}

__attribute__((noipa)) int
baz (int *a, int *r3)
{
  int r = 0, r2[2] = { 0, 0 }, i;
  #pragma omp teams loop default (none) reduction (+:r, r2[0:2], r3[1:1]) shared (a) lastprivate (i)
  for (i = 0; i < 1024; i++)
    {
      r += a[i];
      r2[0] += a[i];
      r3[1] += a[i];
    }
  return r + r2[0] + r3[1] + i;
}

__attribute__((noipa)) int
qux (int *a, int *r3)
{
  int r = 0, r2[2] = { 0, 0 }, i;
  #pragma omp target teams loop default (none) reduction (+:r, r2[1:1], r3[0:2]) shared (a) lastprivate (i)
  for (i = 0; i < 1024; i++)
    {
      r += a[i];
      r2[1] += a[i];
      r3[0] += a[i] - 1;
      r3[1] += a[i];
    }
  return r + r2[1] + r3[0] + r3[1] + i;
}
