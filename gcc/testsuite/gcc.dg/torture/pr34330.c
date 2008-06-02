/* { dg-do compile } */
/* { dg-require-effective-target pthread } */
/* { dg-options "-ftree-parallelize-loops=4 -ftree-vectorize" } */

struct T
{
  int t;
  struct { short s1, s2, s3, s4; } *s;
};

void
foo (int *a, int *b, int *c, int *d, struct T *e)
{
  int i;
  for (i = 0; i < e->t; i++)
    {
      e->s[i].s1 = a[i];
      e->s[i].s2 = b[i];
      e->s[i].s3 = c[i];
      e->s[i].s4 = d[i];
    }
}
