/* PR debug/46387 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

struct S { double x; double y; short z; };
int a = 0, b = 0, c;
void bar (int, int, int);
void baz (int *, int *, int *);

void
foo (struct S *v)
{
  int x, y, z;
  if (!a && b != 0)
    return;
  if (v->z)
    baz (&x, &y, &z);
  else
    {
      x = v->x;
      y = v->y;
    }
  x = x / (5 + 1);
  y = y / (5 + 1);
  if (x < 0)
    x = 0;
  if (x > c - 1)
    x = c - 1;
  if (b == 0)
    bar (x, y, 1);
  return;
}
