/* { dg-do compile } */
/* PR tree-optimization/126571 */
typedef void (*FP) (void);

int
f (int c, FP fp, int *q)
{
  int r;
  if (c)
    r = *(int *) fp;
  else
    r = *q;
  return r + 1;
}
