/* PR/127922 */
/* { dg-lto-do run } */
/* { dg-require-effective-target bitint } */

typedef _BitInt(17) S;
typedef unsigned _BitInt(17) U;

__attribute__((noipa)) S
f(S *p, U *q)
{
  *p = 1;
  *q = 2;
  return *p;
}

int main()
{
  S x = 0;
  S result = f(&x, (U *)&x);
  if (result != 2)
    __builtin_abort ();
}
