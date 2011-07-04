/* PR debug/49602 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

static void
foo (int *x)
{
}

void
bar (int *x)
{
  int i;
  for (i = 0; i == 1; ++i)
    x = 0;
  foo (x);
}
