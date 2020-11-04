void
foo (void)
{
  int s[4] = { 0, 0, 0, 0 };
  int *p = s;
#pragma omp parallel reduction (+: s) allocate(s)
  s[0]++;
#pragma omp parallel reduction (+: s[0:3]) allocate(s)
  s[0]++;
#pragma omp parallel reduction (+: s[2:2]) allocate(s)
  s[2]++;
#pragma omp parallel reduction (+: p[:2]) allocate(p)
  p[0]++;
#pragma omp parallel reduction (+: p[2:2]) allocate(p)
  p[2]++;
}

void
bar (void)
{
  int s[4] = { 0, 0, 0, 0 };
  int *p = s;
  int i;
#pragma omp teams distribute parallel for reduction (+: s) allocate(s)
  for (i = 0; i < 64; i++)
    s[0]++;
#pragma omp teams distribute parallel for reduction (+: s[0:3]) allocate(s)
  for (i = 0; i < 64; i++)
    s[0]++;
#pragma omp teams distribute parallel for reduction (+: s[2:2]) allocate(s)
  for (i = 0; i < 64; i++)
    s[2]++;
#pragma omp teams distribute parallel for reduction (+: p[:2]) allocate(p)
  for (i = 0; i < 64; i++)
    p[0]++;
#pragma omp teams distribute parallel for reduction (+: p[2:2]) allocate(p)
  for (i = 0; i < 64; i++)
    p[2]++;
}
