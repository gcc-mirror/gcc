/* PR c/33238 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89" } */
/* { dg-require-effective-target alloca } */

void
reverse (void *x, int y, int z)
{
  struct { char w[z]; } *p = x, a;
  int i, j;
  for (i = y - 1, j = 0; j < y / 2; i--, j++)
    ({ a = p[i]; p[i] = p[j]; p[j] = a; });
}
