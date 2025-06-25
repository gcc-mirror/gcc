/* { dg-do compile } */
/* { dg-options "-O1 -march=znver2 -std=gnu++17 -w" } */

typedef float a __attribute__ ((__vector_size__ (16)));
typedef long long b __attribute__ ((__vector_size__ (16)));
int c;
a d, e, f;
b g, h;
struct i
{
  i (b j) : k (j) {}
  i ();
  b k;
};
i
l (int j)
{
  g = (b)(__attribute__ ((__vector_size__ (4 * sizeof (1)))) int){ j, j, j,
                                                                   j };
  return g;
}
extern int m ();
void
n ()
{
  h = (__attribute__ ((
      __vector_size__ (2 * sizeof (long long)))) long long){ c };
  i o, p = l (2147483647 * 2 + 1);
  f = __builtin_ia32_blendvps (a (p.k), d, e);
  if (m ())
    {
      i q = l (2147483647 * 2 + 1);
      a r = __builtin_ia32_blendvps (a (q.k), d, e);
      o = b (r);
      i s;
    }
}
