/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -mrvv-max-lmul=conv-dynamic" } */

typedef short a;
typedef struct { a b, c, d; } e;
e *f;
a g;
long h, i;
void j (void)
{
  for (; i; i++)
    {
      h = f[i].c / 0.5;
      g = 5 * h;
      f[i].c = g;
    }
}
