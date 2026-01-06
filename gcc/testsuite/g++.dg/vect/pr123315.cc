// { dg-do compile }
// { dg-additional-options "-O3 -fvect-cost-model=dynamic" }

short a, b, c;
long long e, f;
extern int g[][2][2][23];
inline long max(long a, long b)
{
  return a > b ? a : b;
}
void h(unsigned d)
{
  for (; d < 23; d++) {
      c = b ?: g[1][1][1][d];
      e = max(e, (long long)a);
      f = max(f, (long long)7);
  }
}
