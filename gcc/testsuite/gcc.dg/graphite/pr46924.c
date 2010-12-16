/* { dg-options "-O -fgraphite-identity -ffast-math -fno-tree-loop-im" } */

struct S
{
  int n;
  float *a;
};

float foo (struct S *s)
{
  float f = 0, g=0;
  int i;
  for (i = 0; i < s->n; i++)
    f += s->a[i];
  for (i = 0; i < s->n; i++)
    ;
  return f;
}
