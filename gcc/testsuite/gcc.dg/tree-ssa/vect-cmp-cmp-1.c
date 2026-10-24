/* { dg-do run } */
/* { dg-options "-O -fgimple" } */
/* PR tree-optimization/128123 */

typedef int v4si __attribute__((vector_size (4*sizeof(int))));
typedef v4si v4sib __attribute__((vector_mask));

__attribute__((noipa)) void a(v4si *x, v4si *y, v4si *res)
{
  v4sib m1 = *x < *y;
  v4sib m2 = *x > *y;
  *res = (v4si) (m1 < m2);
}

__attribute__((noipa)) void b(v4si *x, v4si *y, v4si *res)
{
  v4sib m1 = *x < *y;
  v4sib m2 = *x > *y;
  *res = (v4si) (m1 <= m2);
}

int main (void)
{
  v4si x = { 1, 2, 3, 4 }, y = { 2, 2, 2, 5 };
  v4si r;
  a(&x, &y, &r);
  v4si s;
  b(&x, &y, &s);
  for (int i = 0; i < 4; i++)
    if (r[i] != -(x[i] < y[i]) || s[i] != -(x[i] <= y[i]))
      __builtin_abort();
  return 0;
}

