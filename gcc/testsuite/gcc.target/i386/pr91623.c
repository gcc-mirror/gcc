/* PR middle-end/91623 */
/* { dg-do compile } */
/* { dg-options "-O3 -msse4.1 -mno-sse4.2" } */

typedef long long V __attribute__((__vector_size__(16)));
V e, h;
int d;
const int i;

void foo (void);

void
bar (int k, int l)
{
  if (d && 0 <= k - 1 && l)
    foo ();
}

void
baz (void)
{
  V n = (V) { 1 };
  V g = (V) {};
  V o = g;
  for (int f = 0; f < i; ++f)
    {
      V a = o == n;
      h = a;
      bar (f, i);
      o = e;
    }
}
