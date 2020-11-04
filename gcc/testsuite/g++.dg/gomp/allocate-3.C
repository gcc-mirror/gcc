template <typename T>
void
foo (T &x, T (&y)[4], T *&z, int &u, int (&v)[4], int *&w)
{
  T s[4] = { 0, 0, 0, 0 };
  T *p = s;
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
  int s2[4] = { 0, 0, 0, 0 };
  int *p2 = s2;
#pragma omp parallel reduction (+: s2) allocate(s2)
  s2[0]++;
#pragma omp parallel reduction (+: s2[0:3]) allocate(s2)
  s2[0]++;
#pragma omp parallel reduction (+: s2[2:2]) allocate(s2)
  s2[2]++;
#pragma omp parallel reduction (+: p2[:2]) allocate(p2)
  p2[0]++;
#pragma omp parallel reduction (+: p2[2:2]) allocate(p2)
  p2[2]++;
#pragma omp parallel reduction (+: x) allocate(x)
  x++;
#pragma omp parallel reduction (+: y) allocate(y)
  y[0]++;
#pragma omp parallel reduction (+: y[0:3]) allocate(y)
  y[0]++;
#pragma omp parallel reduction (+: y[2:2]) allocate(y)
  y[2]++;
#pragma omp parallel reduction (+: z[:2]) allocate(z)
  z[0]++;
#pragma omp parallel reduction (+: z[2:2]) allocate(z)
  z[2]++;
#pragma omp parallel reduction (+: u) allocate(u)
  u++;
#pragma omp parallel reduction (+: v) allocate(v)
  v[0]++;
#pragma omp parallel reduction (+: v[0:3]) allocate(v)
  v[0]++;
#pragma omp parallel reduction (+: v[2:2]) allocate(v)
  v[2]++;
#pragma omp parallel reduction (+: w[:2]) allocate(w)
  w[0]++;
#pragma omp parallel reduction (+: w[2:2]) allocate(w)
  w[2]++;
}

template <typename T>
void
bar (T &x, T (&y)[4], T *&z, int &u, int (&v)[4], int *&w)
{
  T s[4] = { 0, 0, 0, 0 };
  T *p = s;
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
  int s2[4] = { 0, 0, 0, 0 };
  int *p2 = s2;
#pragma omp teams distribute parallel for reduction (+: s2) allocate(s2)
  for (i = 0; i < 64; i++)
    s2[0]++;
#pragma omp teams distribute parallel for reduction (+: s2[0:3]) allocate(s2)
  for (i = 0; i < 64; i++)
    s2[0]++;
#pragma omp teams distribute parallel for reduction (+: s2[2:2]) allocate(s2)
  for (i = 0; i < 64; i++)
    s2[2]++;
#pragma omp teams distribute parallel for reduction (+: p2[:2]) allocate(p2)
  for (i = 0; i < 64; i++)
    p2[0]++;
#pragma omp teams distribute parallel for reduction (+: p2[2:2]) allocate(p2)
  for (i = 0; i < 64; i++)
    p2[2]++;
#pragma omp teams distribute parallel for reduction (+: x) allocate(x)
  for (i = 0; i < 64; i++)
    x++;
#pragma omp teams distribute parallel for reduction (+: y) allocate(y)
  for (i = 0; i < 64; i++)
    y[0]++;
#pragma omp teams distribute parallel for reduction (+: y[0:3]) allocate(y)
  for (i = 0; i < 64; i++)
    y[0]++;
#pragma omp teams distribute parallel for reduction (+: y[2:2]) allocate(y)
  for (i = 0; i < 64; i++)
    y[2]++;
#pragma omp teams distribute parallel for reduction (+: z[:2]) allocate(z)
  for (i = 0; i < 64; i++)
    z[0]++;
#pragma omp teams distribute parallel for reduction (+: z[2:2]) allocate(z)
  for (i = 0; i < 64; i++)
    z[2]++;
#pragma omp teams distribute parallel for reduction (+: u) allocate(u)
  for (i = 0; i < 64; i++)
    u++;
#pragma omp teams distribute parallel for reduction (+: v) allocate(v)
  for (i = 0; i < 64; i++)
    v[0]++;
#pragma omp teams distribute parallel for reduction (+: v[0:3]) allocate(v)
  for (i = 0; i < 64; i++)
    v[0]++;
#pragma omp teams distribute parallel for reduction (+: v[2:2]) allocate(v)
  for (i = 0; i < 64; i++)
    v[2]++;
#pragma omp teams distribute parallel for reduction (+: w[:2]) allocate(w)
  for (i = 0; i < 64; i++)
    w[0]++;
#pragma omp teams distribute parallel for reduction (+: w[2:2]) allocate(w)
  for (i = 0; i < 64; i++)
    w[2]++;
}

void
baz (long int &x, long int (&y)[4], long int *&z)
{
#pragma omp parallel reduction (+: x) allocate(x)
  x++;
#pragma omp parallel reduction (+: y) allocate(y)
  y[0]++;
#pragma omp parallel reduction (+: y[0:3]) allocate(y)
  y[0]++;
#pragma omp parallel reduction (+: y[2:2]) allocate(y)
  y[2]++;
#pragma omp parallel reduction (+: z[:2]) allocate(z)
  z[0]++;
#pragma omp parallel reduction (+: z[2:2]) allocate(z)
  z[2]++;
}

void
qux (long long int &x, long long int (&y)[4], long long int *&z)
{
  int i;
#pragma omp teams distribute parallel for reduction (+: x) allocate(x)
  for (i = 0; i < 64; i++)
    x++;
#pragma omp teams distribute parallel for reduction (+: y) allocate(y)
  for (i = 0; i < 64; i++)
    y[0]++;
#pragma omp teams distribute parallel for reduction (+: y[0:3]) allocate(y)
  for (i = 0; i < 64; i++)
    y[0]++;
#pragma omp teams distribute parallel for reduction (+: y[2:2]) allocate(y)
  for (i = 0; i < 64; i++)
    y[2]++;
#pragma omp teams distribute parallel for reduction (+: z[:2]) allocate(z)
  for (i = 0; i < 64; i++)
    z[0]++;
#pragma omp teams distribute parallel for reduction (+: z[2:2]) allocate(z)
  for (i = 0; i < 64; i++)
    z[2]++;
}

void
test ()
{
  long int x = 0;
  long int y[4] = { 0, 0, 0, 0 };
  long int *z = y;
  int u = 0;
  int v[4] = { 0, 0, 0, 0 };
  int *w = v;
  long long int x2 = 0;
  long long int y2[4] = { 0, 0, 0, 0 };
  long long int *z2 = y2;
  foo (x, y, z, u, v, w);
  bar (x2, y2, z2, u, v, w);
}

namespace N
{
  int a;
  void foo ()
  {
    int i;
    #pragma omp parallel firstprivate (N::a) allocate (a)
    a++;
    #pragma omp parallel firstprivate (a) allocate (N::a)
    a++;
    #pragma omp teams distribute parallel for firstprivate (N::a) allocate (a)
    for (i = 0; i < 64; i++)
      a++;
    #pragma omp teams distribute parallel for firstprivate (a) allocate (N::a)
    for (i = 0; i < 64; i++)
      a++;
  }
}
