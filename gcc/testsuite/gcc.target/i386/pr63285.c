/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug" } */

struct S { int a; };
struct T { int b, c; } a;
long b;
int c, d;
void bar (int, int);
void baz (void *, int);

void
foo (struct S *x, int y, int z, void *f, int *p, struct T *e)
{
  while (x)
    {
      baz (f, &d > p);
      if (z & 1)
        bar (f > (void *) &f, z);
    }
  if (c)
    {
      asm ("" : "+m" (a) : "i" (0));
      y--;
    }
  if (e->b == e->c)
    c = y;
  y--;
}
