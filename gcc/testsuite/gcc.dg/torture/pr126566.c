/* PR tree-optimization/126566 */
/* { dg-do run } */
/* { dg-require-effective-target label_values } */

void *gp;

__attribute__((noipa)) int
f (int x, int y, int k)
{
  int c;
  if (k) { c = 0; goto out; }
  gp = &&lab;
lab:
  if (x < y) c = -1;
  else if (x > y) c = 1;
  else c = -128;
out:
  return c > 0;
}

int
main (void)
{
  if (f (1, 2, 0) != 0) __builtin_abort ();
  if (f (2, 1, 0) != 1) __builtin_abort ();
  return 0;
}
