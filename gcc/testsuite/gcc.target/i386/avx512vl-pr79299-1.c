/* PR target/79299 */
/* { dg-do assemble { target avx512vl } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-Ofast -mavx512vl -masm=intel" } */

#define N 1024

unsigned long long a[N];
unsigned int b[N], c[N], d[N], e[N], f[N];
unsigned long long g[N], h[N], j[N], k[N];
float l[N], m[N], n[N], o[N];
double p[N], q[N], r[N], s[N];

void
f1 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = c[a[i]];
  for (i = 0; i < N; i++)
    e[i] = f[i] ? f[i] : c[a[i]];
}

void
f2 (void)
{
  int i;
  for (i = 0; i < N; i++)
    d[i] = c[b[i]];
  for (i = 0; i < N; i++)
    e[i] = f[i] ? f[i] : c[b[i]];
}

void
f3 (void)
{
  int i;
  for (i = 0; i < N; i++)
    h[i] = g[a[i]];
  for (i = 0; i < N; i++)
    j[i] = k[i] != 0.0 ? k[i] : g[a[i]];
}

void
f4 (void)
{
  int i;
  for (i = 0; i < N; i++)
    h[i] = g[b[i]];
  for (i = 0; i < N; i++)
    j[i] = k[i] != 0.0 ? k[i] : g[b[i]];
}

void
f5 (void)
{
  int i;
  for (i = 0; i < N; i++)
    m[i] = l[a[i]];
  for (i = 0; i < N; i++)
    n[i] = o[i] ? o[i] : l[a[i]];
}

void
f6 (void)
{
  int i;
  for (i = 0; i < N; i++)
    m[i] = c[b[i]];
  for (i = 0; i < N; i++)
    n[i] = o[i] ? o[i] : c[b[i]];
}

void
f7 (void)
{
  int i;
  for (i = 0; i < N; i++)
    q[i] = p[a[i]];
  for (i = 0; i < N; i++)
    r[i] = s[i] != 0.0 ? s[i] : p[a[i]];
}

void
f8 (void)
{
  int i;
  for (i = 0; i < N; i++)
    q[i] = p[b[i]];
  for (i = 0; i < N; i++)
    r[i] = s[i] != 0.0 ? s[i] : p[b[i]];
}
